packages.used <- c('tidyverse','lubridate','readr')
packages.needed <- setdiff(packages.used, intersect(installed.packages()[,1], packages.used))
if(length(packages.needed) > 0)
{
  install.packages(packages.needed, dependencies = TRUE)
}

library(readr)
library(tidyverse)
library(lubridate)

#set working directory to current file (To Do: find a better way to use relative paths)
#original_wd<-getwd()
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))


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

#convert the confirmed US cases time series csv from wide to long
#only keep the records before April 12 since we already have the JHU U.S. reports for April 12th till today
US_confirmed<-read_csv('../data/JHU Data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv')
date_list<-lapply(colnames(US_confirmed)[12:length(colnames(US_confirmed))],mdy)
date_numeric<-as.numeric(unlist(do.call(c,date_list)))
colnames(US_confirmed)[12:length(colnames(US_confirmed))]<-date_numeric
US_confirmed<-pivot_longer(US_confirmed,cols=as.character(date_numeric[1]:date_numeric[length(date_numeric)]),names_to='Date',values_to='Confirmed')%>%
  mutate(Date=as.Date(as.numeric(Date),origin='1970-01-01'))%>%
  rename(State=Province_State)%>%
  filter(State %in% unique(states_policy$State))

#totals for each state i.e. collapse counties
US_confirmed_states<-US_confirmed%>%
  filter(Date<'2020-04-12')%>%
  arrange(State)%>%
  group_by(State,Date)%>%
  summarise(Confirmed=sum(Confirmed))

#convert the confirmed US deaths time series csv from wide to long
US_deaths<-read_csv('../data/JHU Data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv')
date_list<-lapply(colnames(US_deaths)[13:length(colnames(US_deaths))],mdy)
date_numeric<-as.numeric(unlist(do.call(c,date_list)))
colnames(US_deaths)[13:length(colnames(US_deaths))]<-date_numeric
US_deaths<-pivot_longer(US_deaths,cols=as.character(date_numeric[1]:date_numeric[length(date_numeric)]),names_to='Date',values_to='Deaths')%>%
  mutate(Date=as.Date(as.numeric(Date),origin='1970-01-01'))%>%
  rename(State=Province_State)%>%
  filter(State %in% unique(states_policy$State))

#totals for each state i.e. collapse counties
US_deaths_states<-US_deaths%>%
  filter(Date<'2020-04-12')%>%
  arrange(State)%>%
  group_by(State,Date)%>%
  summarise(Deaths=sum(Deaths),Population=sum(Population))

#join the confirmed cases tibble and deaths tibble
jan_april_stats<-inner_join(US_confirmed_states,US_deaths_states,by=c('State','Date'))%>%
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

# Recode the Policy variables
states_complete = states_complete%>% 
  mutate(
    #Containment and closure policies
    C1_School_closing = factor(case_when(
      `C1_School closing` == 0 ~ "0 - no measures",
      `C1_School closing` == 1 ~ "1 - recommend closing",
      `C1_School closing` == 2 ~ "2 - require closing (only some levels or categories, eg just high school, or just public schools)",
      `C1_School closing` == 3 ~ "3 - require closing all levels")),
    C2_Workplace_closing = factor(case_when(
      `C2_Workplace closing` == 0 ~ "0 - no measures",
      `C2_Workplace closing` == 1 ~ "1 - recommend closing (or recommend work from home)",
      `C2_Workplace closing` == 2 ~ "2 - require closing (or work from home) for some sectors or categories of workers",
      `C2_Workplace closing` == 3 ~ "3 - require closing (or work from home) for all-but-essential workplaces (eg grocery stores, doctors)")),
    C3_Cancel_public_events	 = factor(case_when(
      `C3_Cancel public events` == 0 ~ "0 - no measures",
      `C3_Cancel public events` == 1 ~ "1 - recommend cancelling",
      `C3_Cancel public events` == 2 ~ "2 - require cancelling")),
    C4_Restrictions_on_gatherings = factor(case_when(
      `C4_Restrictions on gatherings` == 0 ~ "0 - no measures",
      `C4_Restrictions on gatherings` == 1 ~ "1 - restrictions on very large gatherings (the limit is above 1000 people)",
      `C4_Restrictions on gatherings` == 2 ~ "2 - restrictions on gatherings between 101-1000 people",
      `C4_Restrictions on gatherings` == 3 ~ "3 - restrictions on gatherings between 11-100 people",
      `C4_Restrictions on gatherings` == 4 ~ "4 - restrictions on gatherings of 10 people or less")),
    C5_Close_public_transport = factor(case_when(
      `C5_Close public transport` == 0 ~ "0 - no measures",
      `C5_Close public transport` == 1 ~ "1 - recommend closing (or significantly reduce volume/route/means of transport available)",
      `C5_Close public transport` == 2 ~ "2 - require closing (or prohibit most citizens from using it)")),
    C6_Stay_at_home_requirements = factor(case_when(
      `C6_Stay at home requirements` == 0 ~ "0 - no measures",
      `C6_Stay at home requirements` == 1 ~ "1 - recommend not leaving house",
      `C6_Stay at home requirements` == 2 ~ "2 - require not leaving house with exceptions for daily exercise, grocery shopping, and 'essential' trips",
      `C6_Stay at home requirements` == 3 ~ "3 - require not leaving house with minimal exceptions (eg allowed to leave once a week, or only one person can leave at a time, etc)")),
    C7_Restrictions_on_internal_movement = factor(case_when(
      `C7_Restrictions on internal movement` == 0 ~ "0 - no measures",
      `C7_Restrictions on internal movement` == 1 ~ "1 - recommend not to travel between regions/cities",
      `C7_Restrictions on internal movement` == 2 ~ "2 - internal movement restrictions in place")),
    C8_International_travel_controls = factor(case_when(
      `C8_International travel controls` == 0 ~ "0 - no measures",
      `C8_International travel controls` == 1 ~ "1 - screening arrivals",
      `C8_International travel controls` == 2 ~ "2 - quarantine arrivals from some or all regions",
      `C8_International travel controls` == 3 ~ "3 - ban arrivals from some regions",
      `C8_International travel controls` == 4 ~ "4 - ban on all regions or total border closure")),
    #Economic policies
    E1_Income_support = factor(case_when(
      `E1_Income support` == 0 ~ "0 - no income support",
      `E1_Income support` == 1 ~ "1 - government is replacing less than 50% of lost salary (or if a flat sum, it is less than 50% median salary)",
      `E1_Income support` == 2 ~ "2 - government is replacing 50% or more of lost salary (or if a flat sum, it is greater than 50% median salary)")),
    E2_Debt_contract_relief = factor(case_when(
      `E2_Debt/contract relief` == 0 ~ "0 - no debt/contract relief",
      `E2_Debt/contract relief` == 1 ~ "1 - narrow relief, specific to one kind of contract",
      `E2_Debt/contract relief` == 2 ~ "2 - broad debt/contract relief")),
    E3_Fiscal_measures = `E3_Fiscal measures`, # didn't recode this part bc Record monetary value in USD of fiscal stimuli, includes any spending or tax cuts NOT included in E4, H4 or H5
    E4_International_support= `E4_International support` ,# Record monetary value in USD
    # Health system policies
    H1_Public_information_campaigns = factor(case_when(
      `H1_Public information campaigns` == 0 ~ "0 - no Covid-19 public information campaign",
      `H1_Public information campaigns` == 1 ~ "1 - public officials urging caution about Covid-19",
      `H1_Public information campaigns` == 2 ~ "2- coordinated public information campaign (eg across traditional and social media)")),
    H2_Testing_policy = factor(case_when(
      `H2_Testing policy` == 0 ~ "0 - no testing policy",
      `H2_Testing policy` == 1 ~ "1 - only those who both (a) have symptoms AND (b) meet specific criteria (eg key workers, admitted to hospital, came into contact with a known case, returned from overseas)",
      `H2_Testing policy` == 2 ~ "2 - testing of anyone showing Covid-19 symptoms",
      `H2_Testing policy` == 3 ~ "3 - open public testing (eg drive through testing available to asymptomatic people)")),
    H3_Contact_tracing	 = factor(case_when(
      `H3_Contact tracing` == 0 ~ "0 - no contact tracing",
      `H3_Contact tracing` == 1 ~ "1 - limited contact tracing; not done for all cases",
      `H3_Contact tracing` == 2 ~ "2 - comprehensive contact tracing; done for all identified cases")),
    H4_Emergency_investment_in_healthcare = `H4_Emergency investment in healthcare`, # Record monetary value in USD
    H5_Investment_in_vaccines = `H5_Investment in vaccines` #Record monetary value in USD
  )%>%
  select(-c(`C1_School closing`,`C2_Workplace closing`,`C3_Cancel public events`,`C4_Restrictions on gatherings`,
            `C5_Close public transport`,`C6_Stay at home requirements`,`C7_Restrictions on internal movement`,
            `C8_International travel controls`,`E1_Income support`,`E2_Debt/contract relief`,`E3_Fiscal measures`,
            `E4_International support`,`H1_Public information campaigns`,`H2_Testing policy`,`H3_Contact tracing`,
            `H4_Emergency investment in healthcare`,`H5_Investment in vaccines`,
            C1_Flag,C1_Notes,C2_Flag,C2_Notes,C3_Flag,C3_Notes,C4_Flag,C4_Notes,
            C5_Flag,C5_Notes,C6_Flag,C6_Notes,C7_Flag,C7_Notes,C8_Notes,E1_Flag,E1_Notes,
            E2_Notes,E3_Notes,E4_Notes,H1_Flag,H1_Notes,H2_Notes,H3_Notes,H4_Notes,H5_Notes,
            M1_Notes,M1_Wildcard,UID,E4_International_support,H4_Emergency_investment_in_healthcare,
            H5_Investment_in_vaccines,E3_Fiscal_measures))



save(states_complete,file="../output/states_complete.RData")
write_csv(states_complete,"../output/states_complete.csv")



#create county data
US_confirmed_counties<-US_confirmed
US_deaths_counties<-US_deaths
county_covid_stats<-inner_join(US_confirmed_counties,US_deaths_counties)%>%
  mutate(Incident_Rate=Confirmed/(Population/100000))%>% #Cases per 100k people
  mutate(Mortality_Rate=Deaths*100/Confirmed)%>%
  mutate(Mortality_Rate=ifelse(!is.na(Mortality_Rate),Mortality_Rate,0))%>%
  mutate(Combined_Key=str_sub(Combined_Key,end=-5))%>%
  select(-c(UID,iso2,iso3,code3,FIPS,Country_Region))%>%
  rename(County=Admin2)

county_complete<-inner_join(county_covid_stats,states_policy)%>%
  select(-c(CountryCode,RegionCode,Jurisdiction,CountryName))%>%
  #only a few rows have infinite mortality rate and they're caused by having counts of deaths without
  #counts of confirmed so in this case we just let mortality rate be 0 as their preceding mortality rate is 0
  mutate(Mortality_Rate=ifelse(!is.na(Mortality_Rate)&is.finite(Mortality_Rate),Mortality_Rate,0))%>%
  #remove rows with 0 population (rows with 0 population are unusable as they all have lat=long=0 
  #and are labeled as 'Unassigned' or 'Out of State' counties with the exception of three counties in Utah and one county in Massachusetts)
  filter(Population!=0)

#Recode policy variables
county_complete = county_complete%>% 
  mutate(
    #Containment and closure policies
    C1_School_closing = factor(case_when(
      `C1_School closing` == 0 ~ "0 - no measures",
      `C1_School closing` == 1 ~ "1 - recommend closing",
      `C1_School closing` == 2 ~ "2 - require closing (only some levels or categories, eg just high school, or just public schools)",
      `C1_School closing` == 3 ~ "3 - require closing all levels")),
    C2_Workplace_closing = factor(case_when(
      `C2_Workplace closing` == 0 ~ "0 - no measures",
      `C2_Workplace closing` == 1 ~ "1 - recommend closing (or recommend work from home)",
      `C2_Workplace closing` == 2 ~ "2 - require closing (or work from home) for some sectors or categories of workers",
      `C2_Workplace closing` == 3 ~ "3 - require closing (or work from home) for all-but-essential workplaces (eg grocery stores, doctors)")),
    C3_Cancel_public_events	 = factor(case_when(
      `C3_Cancel public events` == 0 ~ "0 - no measures",
      `C3_Cancel public events` == 1 ~ "1 - recommend cancelling",
      `C3_Cancel public events` == 2 ~ "2 - require cancelling")),
    C4_Restrictions_on_gatherings = factor(case_when(
      `C4_Restrictions on gatherings` == 0 ~ "0 - no measures",
      `C4_Restrictions on gatherings` == 1 ~ "1 - restrictions on very large gatherings (the limit is above 1000 people)",
      `C4_Restrictions on gatherings` == 2 ~ "2 - restrictions on gatherings between 101-1000 people",
      `C4_Restrictions on gatherings` == 3 ~ "3 - restrictions on gatherings between 11-100 people",
      `C4_Restrictions on gatherings` == 4 ~ "4 - restrictions on gatherings of 10 people or less")),
    C5_Close_public_transport = factor(case_when(
      `C5_Close public transport` == 0 ~ "0 - no measures",
      `C5_Close public transport` == 1 ~ "1 - recommend closing (or significantly reduce volume/route/means of transport available)",
      `C5_Close public transport` == 2 ~ "2 - require closing (or prohibit most citizens from using it)")),
    C6_Stay_at_home_requirements = factor(case_when(
      `C6_Stay at home requirements` == 0 ~ "0 - no measures",
      `C6_Stay at home requirements` == 1 ~ "1 - recommend not leaving house",
      `C6_Stay at home requirements` == 2 ~ "2 - require not leaving house with exceptions for daily exercise, grocery shopping, and 'essential' trips",
      `C6_Stay at home requirements` == 3 ~ "3 - require not leaving house with minimal exceptions (eg allowed to leave once a week, or only one person can leave at a time, etc)")),
    C7_Restrictions_on_internal_movement = factor(case_when(
      `C7_Restrictions on internal movement` == 0 ~ "0 - no measures",
      `C7_Restrictions on internal movement` == 1 ~ "1 - recommend not to travel between regions/cities",
      `C7_Restrictions on internal movement` == 2 ~ "2 - internal movement restrictions in place")),
    C8_International_travel_controls = factor(case_when(
      `C8_International travel controls` == 0 ~ "0 - no measures",
      `C8_International travel controls` == 1 ~ "1 - screening arrivals",
      `C8_International travel controls` == 2 ~ "2 - quarantine arrivals from some or all regions",
      `C8_International travel controls` == 3 ~ "3 - ban arrivals from some regions",
      `C8_International travel controls` == 4 ~ "4 - ban on all regions or total border closure")),
    #Economic policies
    E1_Income_support = factor(case_when(
      `E1_Income support` == 0 ~ "0 - no income support",
      `E1_Income support` == 1 ~ "1 - government is replacing less than 50% of lost salary (or if a flat sum, it is less than 50% median salary)",
      `E1_Income support` == 2 ~ "2 - government is replacing 50% or more of lost salary (or if a flat sum, it is greater than 50% median salary)")),
    E2_Debt_contract_relief = factor(case_when(
      `E2_Debt/contract relief` == 0 ~ "0 - no debt/contract relief",
      `E2_Debt/contract relief` == 1 ~ "1 - narrow relief, specific to one kind of contract",
      `E2_Debt/contract relief` == 2 ~ "2 - broad debt/contract relief")),
    E3_Fiscal_measures = `E3_Fiscal measures`, # didn't recode this part bc Record monetary value in USD of fiscal stimuli, includes any spending or tax cuts NOT included in E4, H4 or H5
    E4_International_support= `E4_International support` ,# Record monetary value in USD
    # Health system policies
    H1_Public_information_campaigns = factor(case_when(
      `H1_Public information campaigns` == 0 ~ "0 - no Covid-19 public information campaign",
      `H1_Public information campaigns` == 1 ~ "1 - public officials urging caution about Covid-19",
      `H1_Public information campaigns` == 2 ~ "2- coordinated public information campaign (eg across traditional and social media)")),
    H2_Testing_policy = factor(case_when(
      `H2_Testing policy` == 0 ~ "0 - no testing policy",
      `H2_Testing policy` == 1 ~ "1 - only those who both (a) have symptoms AND (b) meet specific criteria (eg key workers, admitted to hospital, came into contact with a known case, returned from overseas)",
      `H2_Testing policy` == 2 ~ "2 - testing of anyone showing Covid-19 symptoms",
      `H2_Testing policy` == 3 ~ "3 - open public testing (eg drive through testing available to asymptomatic people)")),
    H3_Contact_tracing	 = factor(case_when(
      `H3_Contact tracing` == 0 ~ "0 - no contact tracing",
      `H3_Contact tracing` == 1 ~ "1 - limited contact tracing; not done for all cases",
      `H3_Contact tracing` == 2 ~ "2 - comprehensive contact tracing; done for all identified cases")),
    H4_Emergency_investment_in_healthcare = `H4_Emergency investment in healthcare`, # Record monetary value in USD
    H5_Investment_in_vaccines = `H5_Investment in vaccines` #Record monetary value in USD
  )%>%
  select(-c(`C1_School closing`,`C2_Workplace closing`,`C3_Cancel public events`,`C4_Restrictions on gatherings`,
            `C5_Close public transport`,`C6_Stay at home requirements`,`C7_Restrictions on internal movement`,
            `C8_International travel controls`,`E1_Income support`,`E2_Debt/contract relief`,`E3_Fiscal measures`,
            `E4_International support`,`H1_Public information campaigns`,`H2_Testing policy`,`H3_Contact tracing`,
            `H4_Emergency investment in healthcare`,`H5_Investment in vaccines`,
            C1_Flag,C1_Notes,C2_Flag,C2_Notes,C3_Flag,C3_Notes,C4_Flag,C4_Notes,
            C5_Flag,C5_Notes,C6_Flag,C6_Notes,C7_Flag,C7_Notes,C8_Notes,E1_Flag,E1_Notes,
            E2_Notes,E3_Notes,E4_Notes,H1_Flag,H1_Notes,H2_Notes,H3_Notes,H4_Notes,H5_Notes,
            M1_Notes,M1_Wildcard,E4_International_support,H4_Emergency_investment_in_healthcare,
            H5_Investment_in_vaccines,E3_Fiscal_measures))

#writing to a csv takes about 30 seconds, and saving it as .RData takes 6 seconds
#so try to only save/load using the counties RData file to save on runtime
save(county_complete,file="../output/county_complete.RData")



#turn Date from rows into columns
data <- states_complete
Colnames <- names(data)

for (i in c(3,4,7:length(names(data)))){
  df_temp <- data[,c(1:2,i)]
  name_temp <- str_split(Colnames[i], "/")
  name_converted <- reduce(unlist(name_temp),paste0)
  df_pivot <- df_temp %>% pivot_wider(names_from = `Date`,values_from = Colnames[i])
  write_csv(df_pivot,paste0("../output/",name_converted,".csv"))
}




#---------geojson data----------
states <- geojsonio::geojson_read("../data/gz_2010_us_040_00_20m.json", what = "sp")
counties<- geojsonio::geojson_read('../data/gz_2010_us_050_00_20m.json',what='sp')

Confirmed <- read_csv("../output/Confirmed.csv")
Deaths <- read_csv("../output/Deaths.csv")
date_choices <- names(Confirmed)[-1]


confirmed_county_data<-read.csv('../data/JHU Data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv')
deaths_county_data<-read.csv('../data/JHU Data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv')

names(confirmed_county_data) <- c(names(confirmed_county_data)[1:11],date_choices[1:260])
names(deaths_county_data) <- c(names(deaths_county_data)[1:12],date_choices[1:260])

cdata_temp<-left_join(confirmed_county_data,data.frame(states),by=c('Province_State'='NAME'),keep=TRUE)
ddata_temp<-left_join(deaths_county_data,data.frame(states),by=c('Province_State'='NAME'),keep=TRUE)

df_getstate<-read_csv('../data/JHU Data/csse_covid_19_data/csse_covid_19_daily_reports_us/04-12-2020.csv')
df_getloc <- data.frame(df_getstate %>% dplyr::select(NAME = Province_State,Lat = Lat,Long = Long_))



#---convert unreadable characters such as '\xf1'----------------------------
convert_xf1 <- function(str){
  return(reduce(unlist(str_split(str,'\xf1')),paste0))
}

#setwd(original_wd) #set working directory to what it was originally
