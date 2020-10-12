packages.used <- c("shiny", "shinydashboard", "leaflet", "shinyWidgets","plotly","shinythemes","wordcloud2", "DT")
packages.needed <- setdiff(packages.used, intersect(installed.packages()[,1], packages.used))
if(length(packages.needed) > 0)
{
  install.packages(packages.needed, dependencies = TRUE)
}
library(dplyr)
library(DT)
library(leaflet)
library(plotly)
library(readr)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(stringr)
library(tibble)
library(tidyverse)
library(wordcloud2)



load('../output/states_complete.RData')
load('../output/county_complete.RData')

dashboardPage(
  skin = "blue", 
  dashboardHeader(title = "Covid State Policy Tracker"),
  dashboardSidebar(sidebarMenu(
    menuItem("Home", tabName = "Home", icon = icon("dashboard")),
    menuItem("Map", tabName = "Map", icon = icon("compass"),startExpanded = TRUE,
             menuSubItem("US Map",tabName="US_Map",icon=icon("globe-americas")), 
             menuSubItem("State Map",tabName="State_Map",icon=icon("map-marked"))),
    menuItem("Report", tabName = "Report", icon = icon("chart-line"),startExpanded = TRUE,
             menuSubItem("State Comparison",tabName="State_Comparison",icon=icon("users")), 
             menuSubItem("County Comparison",tabName="County_Comparison",icon=icon("user"))
             ),
    menuItem("Reference", tabName = "Reference", icon = icon("th"))
    )),
  dashboardBody(fill = FALSE,tabItems(
    #home --------------------------------------------------------------------------------------------------------
    tabItem(tabName = "Home",
            fluidPage(
              fluidRow(
                box(width = 15, title = "Introduction", status = "primary",
                    solidHeader = TRUE, h3("Covid State Policy Tracker"),
                    h4("By Zihan Chen, Xujie Ma, Rohan Uppuluri & Jiaqi Yuan"),
                    h5("Covid-19 outbreaks affect every country in the world. However, the magnitude of the impact varied among countries, as some countries have been successful in limiting the spreading of disease. There are many explanations of why some countries have fewer cases than others. One of them is that the government policy response."),
                    h5("In this project, we built a policy tracker to explore the interaction of state government policy response and Covid-19 statistics overtime (from 01/22/20 to 10/07/20). In particular, we want to see how some key Covid-19 statistics (confirmed cases, death, infection rate, mortality rate, positive test rate, hospitalization rate) change overtime and how the state governments are publishing corresponding containment and closure policies, health system policies, and economic policies."))),
              fluidRow(box(width = 15, title = "User Group", status = "primary",
                           solidHeader = TRUE, h3("Why Did We Develop this App?"),
                           h5("Policy responses to Cover-19 are complex, context-specific and rapidly changing. Documenting the policies and the stringency can help policy makers to understand and assess government responses to Covid-19 over time. "),
                           h5("Our project provides the interactive plot of state government policy and Covid-19 statistics, and it could help to answer questions such as:"),
                           tags$div(tags$ul(
                             tags$li("1. How did policy stringency change with the evolving situation"),
                             tags$li("2. How Covid-19 statistics change after policy stringency change")
                           )))),
              fluidRow(box(width = 15, title = "User Guide", status = "primary",
                           solidHeader = TRUE, h3("What Does This App Do?"),
                           tags$div(tags$ul(
                             tags$li("Map: This map contains 2 U.S. geological graph, one at state level and another at county level. Each graph records the confirmed cases and death. User can select the state, the Covid-19 Statistics and time point"),
                             tags$li("Report : This part contains time series plot on when did each state/county’s enforced new policies and how key Covid-19 measures changes overtime. User can select up to 3 states/counties at one time.")
                           )))),
              fluidRow(box(width = 15, title = "Policies and Covid-19 statistics", status = "primary",
                           solidHeader = TRUE, h3("What Policies and Covid-19 Statistics Are Included?"),
                           h4("Dataset & Data Processing"),
                           h5("We used JHU datasets(csse_covid_19_daily_reports_us & time_series_covid19_confirmed_US.csv) and OxCGRT(OxCGRT_US_latest.csv) to construct our dataset. The JHU datasets contain information on the Covid-19 statistics, detailed below. The OxCGRT systematically collects information on several different common policy responses governments have taken, records these policies on a scale to reflect the extent of government action, and aggregates these scores into a suite of policy indices."),
                           
                           h4("Potential Data Collection Biases"),
                           h5("There are also some potential biases for Covid-19 statistics data collection, as there could be under-ascertainment of mild cases and time lags."),
                           
                           h4("Data quality"),
                           h5("There are some limitations of our dataset, including:"),
                           tags$div(tags$ul(
                             tags$li("1. Missing values: Hospitalization rate and testing rate are only available starting on 04/12/20 at state level."),
                             tags$li("2. Dropped variables: we excluded some variables from the original OxCGRT for analysis, as 80% of the data are missing values. The variables excluded are Fiscal measures, International support, Emergency investment in healthcare, Investment in vaccines."),
                             tags$li("3. Some states have irregular Covid-19 statistics update schedule: "),
                             tags$div(tags$ul(
                               tags$li("1. Rhode Island: Not updating case, death, or recovered data on the weekends. Releasing county level cases and deaths once per week."),
                               tags$li("2. Conneticut: Not updating case, death, or recovered data on the weekends."),
                               tags$li("3. Illinois: Releasing probable cases once per week. "),
                               tags$li("4. Louisiana: Not updating on the weekends."),
                               tags$li("5. Michigan: No case data provided for August 21."),
                               tags$li("6. Kansas: No data for the weekend of August 22-23."),
                               tags$li("7. Michigan: Not providing death data on Sundays.")
                               ))
                             )
                           ),
                           
                           h4("In this project, we have 3 main policy areas and 6 Covid-19 Statistics"),
                           h5("Policy Area 1: Containment and Closure Policies"),
                           tags$div(tags$ul(
                             tags$li("1. Closing of schools and universities"),
                             tags$li("2. Closing of workplaces"),
                             tags$li("3. Cancelling public events"),
                             tags$li("4. Limits on private gatherings"),
                             tags$li("5. Closing of public transport"),
                             tags$li("6. Orders to shelter-in-place and otherwise confine to the home"),
                             tags$li("7. Restrictions on internal movement between cities/regions"),
                             tags$li("8. Restrictions on international travel for foreign travelers"))
                             ),
                           
                           h5("Policy Area 2: Health System Policies"),
                           tags$div(tags$ul(
                             tags$li("1. Presence of public info campaigns"),
                             tags$li("2. Government policy on who has access to testing (Note: this records policies about testing for current infection (PCR tests) not testing for immunity (antibody test))"),
                             tags$li("3. Government policy on contact tracing after a positive diagnosis (Note: we are looking for policies that would identify all people potentially exposed to Covid-19; voluntary bluetooth apps are unlikely to achieve this)"),
                             tags$li("4. Announced short term spending on healthcare system, eg hospitals, masks, etc. Note: only record amount additional to previously announced spending"),
                             tags$li("5. Announced public spending on Covid-19 vaccine development Note: only record amount additional to previously announced spending"))
                           ),
                           
                           h5("Policy Area 3:Economic Policies"),
                           tags$div(tags$ul(
                             tags$li("1. If the government is providing direct cash payments to people who lose their jobs or cannot work (Note: only includes payments to firms if explicitly linked to payroll/salaries)"),
                             tags$li("2. If the government is freezing financial obligations for households (eg stopping loan repayments, preventing services like water from stopping, or banning evictions)"))
                           ),
                           
                           h4("Covid-19 Measures"),
                           tags$div(tags$ul(
                             tags$li("1. Total Confirmed Cases"),
                             tags$li("2. Total Number of Death"),
                             tags$li("3. Incidence Rate: cases per 100,000 persons"),
                             tags$li("4. Mortality Rate: Number recorded deaths * 100/ Number confirmed cases"),
                             tags$li("5. Testing Rate: Total test results (positive + negative) per 100,000 persons"),
                             tags$li("6. Hospitalization Rate: Total number hospitalized / Number cases"))
                           )
                           )))),
    #home end --------------------------------------------------------------------------------------------------------
    
    
    #map --------------------------------------------------------------------------------------------------------
    tabItem(tabName = "US_Map",
            h4("This map tracks the cases and deaths of each state of US. If you click on the 'play' button on the left, you could see the evolution of the covid-19 situation in every 3 days."),
            h4("Get started by choosing a input date"),
            tags$div(id='my_div',
                     class='my_class',
                     selectInput(inputId='stats_dropdown',label='Select Covid-19 Statistics',
                                 choices=c('Cases','Deaths'))),
            leafletOutput("map", width = "100%", height = "1200"),
            absolutePanel(id = "control", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
                          top = 500, left = 260, right = "auto", bottom = "auto", width = 250, height = "auto",
                          sliderInput('date_map','Input Date:',
                                      #first day of data recording
                                      min = as.Date(date_choices[1]),
                                      #present day of data recording
                                      max = as.Date(tail(date_choices,1)),
                                      value = as.Date('2020-01-22','%Y-%m-%d'),
                                      timeFormat = "%Y-%m-%d",
                                      animate = TRUE, step = 3),
                          style = "opacity: 0.80")
    ),
    
    tabItem(tabName = "State_Map",
            h4("This map gives a closer look at situations of counties in each state. You can select as many states as you like and find how it is going in the counties."),
            h4("Get started by choosing a input date"),
            tags$div(id='my_div1',
                     class='my_class',
                     selectInput(inputId='county_name',label='Select State',
                                 choices=(states_complete%>%filter(Date=='2020-10-03'))$State,
                                 selected = "New York", multiple = T)
                                 ),
            tags$div(id='my_div2',
                     class='my_class',
                     selectInput(inputId='county_stats_dropdown',label='Select Covid-19 Statistics',
                                 choices=c('Cases','Deaths'))),
            tags$style(type="text/css", '.my_class .selectize-control .selectize-dropdown {position: static !important;}'),
            ## county map
            leafletOutput("county_map", width = "100%", height = "1200"),  
            absolutePanel(id = "control", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
                          top = 510, left = 260, right = "auto", bottom = "auto", width = 250, height = "auto",
                          sliderInput('county_date_map','Input Date:',
                                      #first day of data recording
                                      min = as.Date(date_choices[1]),
                                      #present day of data recording
                                      max = as.Date(tail(date_choices,1)),
                                      value = as.Date('2020-01-22','%Y-%m-%d'),
                                      timeFormat = "%Y-%m-%d",
                                      animate = TRUE, step = 3),
                          style = "opacity: 0.80")
    ),
    #map end --------------------------------------------------------------------------------------------------------
    
    
    #report --------------------------------------------------------------------------------------------------------
    tabItem(tabName = "State_Comparison",
            fluidRow(column(12,
                            h3("Interactive Dashboard on State and Policy"),
                            "The following line plots show how the incident rate, mortality rate, testing rate, and hospitalization rate change
                            over time and with a chosen policy being enforced for up to three states. Note that testing rate and hospitalization rate information is only
                            available starting from April 12, 2020.")),
            br(),
            pickerInput(inputId="state_dropdown",label='Select up to Three States',
                        choices=unique(states_complete$State),multiple=TRUE,
                        options=list(`max-options`=3),
                        selected='Alabama'
            ),
            selectInput(inputId='policy_dropdown',label='Select Policy',
                        choices=colnames(states_complete)[27:39]),
            plotlyOutput("state_line_plot",height="800px",width="100%"),
            br(),
            br()
    ),
    
    tabItem(tabName="County_Comparison",
            fluidRow(column(12,
                            h3("Interactive Dashboard on County and Policy"),
                            "The following line plots show how the incident rate and mortality rate change
                            over time and with a chosen policy being enforced for up to three counties. Note that testing rate and hospitalization rate information is not
                            available at the county level. Also, policy information is defined at the state level and not at the county level, so all counties 
                            in the same state have the same policy levels enforced for any given date.")),
            br(),
            pickerInput(inputId="county_dropdown",label='Select up to Three Counties',
                        choices=split((county_complete%>%filter(Date=='2020-10-03'))$Combined_Key,(county_complete%>%filter(Date=='2020-10-03'))$State),
                        multiple=TRUE,
                        options=list(`max-options`=3),
                        selected='Autauga, Alabama',
                        width='fit'
            ),
            selectInput(inputId='policy_dropdown_2',label='Select Policy',
                        choices=colnames(county_complete)[24:36]),
            plotlyOutput("county_line_plot",height="400px",width="100%"),
            br(),
            br()
            
            
    ),
    #report end --------------------------------------------------------------------------------------------------------
    tabItem(tabName = "Reference",
            fluidPage(
              fluidRow(
                box(width = 15, title = "Raw Dataset", status = "primary",
                    solidHeader = TRUE,"The raw dataset for this project is from",
                    tags$a(href="https://github.com/OxCGRT/USA-covid-policy/tree/master/data","Oxford Covid-19 Government Response Tracker"),
                    "and",
                    tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data","JHU CSSE COVID-19 Dataset"),
                    ".",
                    "We use", 
                    tags$a(href="https://eric.clst.org/tech/usgeojson/", "geojson data"),
                    "for the United States to draw US map and State map."
                    )),
              fluidRow(
                box(width = 15, title = "Project Code", status = "primary",
                    solidHeader = TRUE,"The code for this project can be found on",
                    tags$a(href="https://github.com/TZstatsADS/Fall2020-Project2-group8","our Github")
                    ))
            )
    )
  )
)
)