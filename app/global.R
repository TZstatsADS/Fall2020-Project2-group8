packages.used <- c('tidyverse','readr')
packages.needed <- setdiff(packages.used, intersect(installed.packages()[,1], packages.used))
if(length(packages.needed) > 0)
{
  install.packages(packages.needed, dependencies = TRUE)
}

library(readr)
library(tidyverse)

#---------geojson data----------
states <- geojsonio::geojson_read("./output/gz_2010_us_040_00_20m.json", what = "sp")
counties<- geojsonio::geojson_read('./output/gz_2010_us_050_00_20m.json',what='sp')

Confirmed <- read_csv("./output/Confirmed.csv")
Deaths <- read_csv("./output/Deaths.csv")
C1_School_closing <- read_csv("./output/C1_School_closing.csv")
C2_Workplace_closing<-read_csv('./output/C2_Workplace_closing.csv')
C3_Cancel_public_events<-read_csv('./output/C3_Cancel_public_events.csv')
C4_Restrictions_on_gatherings<-read_csv('./output/C4_Restrictions_on_gatherings.csv')
C5_Close_public_transport<-read_csv('./output/C5_Close_public_transport.csv')
C6_Stay_at_home_requirements<-read_csv('./output/C6_Stay_at_home_requirements.csv')
C7_Restrictions_on_internal_movement<-read_csv('./output/C7_Restrictions_on_internal_movement.csv')
C8_International_travel_controls<-read_csv('./output/C8_International_travel_controls.csv')
E1_Income_support<-read_csv('./output/E1_Income_support.csv')
E2_Debt_contract_relief<-read_csv('./output/E2_Debt_contract_relief.csv')
H1_Public_information_campaigns<-read_csv('./output/H1_Public_information_campaigns.csv')
H2_Testing_policy<-read_csv('./output/H2_Testing_policy.csv')
H3_Contact_tracing<-read_csv('./output/H3_Contact_tracing.csv')


date_choices <- names(Confirmed)[-1]


confirmed_county_data<-read.csv('./output/time_series_covid19_confirmed_US.csv')
deaths_county_data<-read.csv('./output/time_series_covid19_deaths_US.csv')

names(confirmed_county_data) <- c(names(confirmed_county_data)[1:11],date_choices[1:260])
names(deaths_county_data) <- c(names(deaths_county_data)[1:12],date_choices[1:260])

cdata_temp<-left_join(confirmed_county_data,data.frame(states),by=c('Province_State'='NAME'),keep=TRUE)
ddata_temp<-left_join(deaths_county_data,data.frame(states),by=c('Province_State'='NAME'),keep=TRUE)

df_getstate<-read_csv('./output/04-12-2020.csv')
df_getloc <- data.frame(df_getstate %>% dplyr::select(NAME = Province_State,Lat = Lat,Long = Long_))



#---convert unreadable characters such as '\xf1'----------------------------
convert_xf1 <- function(str){
  return(reduce(unlist(str_split(str,'\xf1')),paste0))
}