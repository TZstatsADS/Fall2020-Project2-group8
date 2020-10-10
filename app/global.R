library(readr)
library(tidyverse)

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
