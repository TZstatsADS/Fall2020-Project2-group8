packages.used <- c('tidyverse','lubridate')
packages.needed <- setdiff(packages.used, intersect(installed.packages()[,1], packages.used))
if(length(packages.needed) > 0)
{
  install.packages(packages.needed, dependencies = TRUE)
}

library(tidyverse)
library(lubridate)

#set working directory to current file (To Do: find a better way to use relative paths)
original_wd<-getwd()
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


#The Oxford dataset has policy information for each state from January 1 to today in a single csv
states_policy<-read_csv('../data/Oxford Data/data/OxCGRT_US_latest.csv')
states_policy<-states_policy%>%
  filter(!is.na(RegionName))%>%
  rename(State=RegionName)%>%
  filter(State!='Virgin Islands') #remove Virgin Islands since it's not a state
states_policy$Date<-ymd(as.character(states_policy$Date))



#The JHU dataset has covid statistics for each state like death rate, incident rate, test rate, etc starting from April 12,
#but it has one csv per day so we need to merge them all into one tibble
csv_names<-list.files('../data/JHU Data/csse_covid_19_data/csse_covid_19_daily_reports_us')
num_csv_names<-length(csv_names)-1 #-1 is to exclude readme file
csv_names<-csv_names[1:num_csv_names] #exclude readme

create_report_list<-function(i)
{
  daily_report<-read_csv(paste0('../data/JHU Data/csse_covid_19_data/csse_covid_19_daily_reports_us/',csv_names[i]))
  daily_report<-daily_report%>%
    mutate(Date=mdy(substr(csv_names[i],1,10))) #Make a date column from the name of the csv file
  return(list(daily_report))
}

daily_report_list<-map(as.list(c(1:num_csv_names)),create_report_list)
states_covid_stats<-reduce(daily_report_list,bind_rows)
states_covid_stats<-states_covid_stats%>%
  rename(State=Province_State)%>%
  filter(State %in% unique(states_policy$State)) #only keep data on states and not territories

#The problem with the daily JHU U.S. reports is that they only have data starting from April 12. The JHU worldwide reports have data starting from January 22 though, so
#we need to use the worldwide data and aggregate records for each state. However, the records from January 22 to April 11 only have counts of 
#confirmed cases and deaths. During this time recovered and active cases were not calculated for each state and were instead only calculated for the U.S. as a whole.
#Hence, we don't have recovered, active, testing, or hospitalization data of each state from January 22 to April 11.

#name, latitude, and longitude of each of the 50 states
state_lat_long<-states_covid_stats%>%
  select(State,Lat,Long_)%>%
  head(50)

#convert the confirmed US cases time series csv from wide to long so each (State,Date) pair is a row
#only keep the records before April 12 since we already have the JHU U.S. reports for April 12th till today
US_confirmed<-read_csv('../data/JHU Data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv')
date_list<-lapply(colnames(US_confirmed)[12:length(colnames(US_confirmed))],mdy)
date_numeric<-as.numeric(unlist(do.call(c,date_list)))
colnames(US_confirmed)[12:length(colnames(US_confirmed))]<-date_numeric
US_confirmed<-pivot_longer(US_confirmed,cols=as.character(date_numeric[1]:date_numeric[length(date_numeric)]),names_to='Date',values_to='Confirmed')%>%
  mutate(Date=as.Date(as.numeric(Date),origin='1970-01-01'))%>%
  rename(State=Province_State)%>%
  filter(Date<'2020-04-12')%>%
  arrange(State)%>%
  group_by(State,Date)%>%
  summarise(Confirmed=sum(Confirmed))%>%
  filter(State %in% unique(states_policy$State))

#convert the confirmed US deaths time series csv from wide to long so each (State,Date) pair is a row
US_deaths<-read_csv('../data/JHU Data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv')
date_list<-lapply(colnames(US_deaths)[13:length(colnames(US_deaths))],mdy)
date_numeric<-as.numeric(unlist(do.call(c,date_list)))
colnames(US_deaths)[13:length(colnames(US_deaths))]<-date_numeric
US_deaths<-pivot_longer(US_deaths,cols=as.character(date_numeric[1]:date_numeric[length(date_numeric)]),names_to='Date',values_to='Deaths')%>%
  mutate(Date=as.Date(as.numeric(Date),origin='1970-01-01'))%>%
  rename(State=Province_State)%>%
  filter(Date<'2020-04-12')%>%
  arrange(State)%>%
  group_by(State,Date)%>%
  summarise(Deaths=sum(Deaths),Population=sum(Population))%>%
  filter(State %in% unique(states_policy$State))

#join the confirmed cases tibble and deaths tibble
jan_april_stats<-inner_join(US_confirmed,US_deaths,by=c('State','Date'))%>%
  right_join(state_lat_long,by='State')%>%
  mutate(Incident_Rate=Confirmed/(Population/100000))%>% #Cases per 100k people
  mutate(Mortality_Rate=Deaths*100/Confirmed)%>%
  mutate(Mortality_Rate=ifelse(!is.na(Mortality_Rate),Mortality_Rate,0))%>%
  select(-c(Population))

#bind the January-April data with states_covid_stats by row to create an updated states_covid_stats tibble
states_covid_stats<-bind_rows(jan_april_stats,states_covid_stats)%>%
  arrange(State,Date)



#states_complete joins states_policy and states_covid_stats on the Date and State columns
#each (date,state) pair has its own row in states_complete
#so each row contains all the policy information and covid stats for a particular state on a particular day
states_complete<-inner_join(states_covid_stats,states_policy,by=c("Date","State"))%>%
  select(-c(CountryCode,Country_Region,ISO3,CountryName,Jurisdiction,FIPS,Last_Update,RegionCode)) #Remove redundant columns

save(states_complete,file="../output/states_complete.RData")
setwd(original_wd) #set working directory to what it was originally
