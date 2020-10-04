library(tidyverse)
library(lubridate)

#set working directory to current file (To Do: find a better way to use relative paths)
original_wd<-getwd()
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#Oxford dataset has policy information for each state from January 1st to today in a single csv
states_policy<-read_csv('../data/Oxford Data/data/OxCGRT_US_latest.csv')
states_policy<-states_policy%>%
  filter(!is.na(RegionName))%>%
  rename(State=RegionName)
states_policy$Date<-ymd(as.character(states_policy$Date))



#JHU dataset has covid statistics for each state like death rate, incident rate, test rate, etc
#JHU dataset has one csv per day so we need to merge them all into one tibble
csv_names<-list.files('../data/JHU Data/csse_covid_19_data/csse_covid_19_daily_reports_us')
num_csv_names<-length(csv_names)-1 #-1 is to exclude readme file
csv_names<-csv_names[1:num_csv_names] #exclude readme

create_report_list<-function(i)
{
  daily_report<-read_csv(paste0('../data/JHU Data/csse_covid_19_data/csse_covid_19_daily_reports_us/',csv_names[i]))
  return(list(daily_report))
}

daily_report_list<-map(as.list(c(1:num_csv_names)),create_report_list)

#bind the daily reports by row to create a single tibble called states_covid_stats
states_covid_stats<-reduce(daily_report_list,bind_rows)
states_covid_stats$Last_Update<-date(states_covid_stats$Last_Update)
#states_covid_stats has extra territories that states_policy doesn't have like the Diamond Princess cruise ship,
#so we'll remove these extra territories
states_covid_stats<-states_covid_stats%>%
  rename(Date=Last_Update,State=Province_State)%>%
  filter(State %in% unique(states_policy$State)) 



#states_complete joins states_policy and states_covid_states on the Date and State columns
#Each (date,state) pair has its own row in states_complete
#So each row contains all the policy information and covid stats for a particular state on a particular day
states_complete<-inner_join(states_covid_stats,states_policy,by=c("Date","State"))%>%
  select(-c(CountryCode,Country_Region)) #CountryCode and Country_Region are both 'USA' for every row so we don't need these columns

save(states_complete,file="../output/states_complete.RData")
setwd(original_wd) #set working directory to what it was originally
