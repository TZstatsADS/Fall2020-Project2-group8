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
                    h5("Covid-19 outbreaks affect every country in the world. However, the magnitude of the impacts varied among countries, as some of them have been successful in limiting the spreading of disease. There are many explanations of why some countries have less cases than others. One of the reasons is that the government policy response."),
                    h5("In this project, we built a policy tracker (01/22/20 - 10/07/20) to look at how state governments responded to the evolving situation and how the Covid-19 situation changes with the controlling measures overtime.  In particular, we want to see how some key indicators (infection rate, mortality rate, positive test rate, hospitalization rate) changes as the state government is publishing corresponding containment and closure policies, health system policies, and economic policies. We used the JHU dataset, as well as the Oxford Coronavirus Government Response Tracker(OxCGRT)."))),
              fluidRow(box(width = 15, title = "User Group", status = "primary",
                           solidHeader = TRUE, h3("Why Did We Develop this Map?"),
                           h5("Policy responses to Cover-19 are complex, context-specific and rapidly changing. Documenting the policies and the stringency can help policy makers to understand and assess government responses to Covid-19 over time. "),
                           h5("Our project provides the interactive plot of state government policy and Covid-19 Statistics, and it could help to answer questions such as:"),
                           tags$div(tags$ul(
                             tags$li("1. How did policy stringency change with the evolving situation"),
                             tags$li("2. How Covid-19 statistics change after policy stringency change")
                           )))),
              fluidRow(box(width = 15, title = "User Guide", status = "primary",
                           solidHeader = TRUE, h3("What Does This Map Do?"),
                           tags$div(tags$ul(
                             tags$li("Map: This map contains 2 U.S. geological graph, one at state level and another at county level. Each graph records the confirmed cases and death. User can select the state, the Covid-19 Statistics and time point"),
                             tags$li("Report : This part contains time series plot on when did each state/county’s enforced new policies and how key Covid-19 measures changes overtime. User can select up to 3 states/counties at one time ")
                           )))),
              fluidRow(box(width = 15, title = "Policies and Covid-19 statistics", status = "primary",
                           solidHeader = TRUE, h3("What Policies and Covid-19 Statistics Are Included?"),
                           h5("In this project, we have 3 main policy areas and 6 Covid-19 Statistics"),
                           
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
                                      value = as.Date('2020-04-01','%Y-%m-%d'),
                                      timeFormat = "%Y-%m-%d",
                                      animate = TRUE, step = 3),
                          style = "opacity: 0.80")
    ),
    
    tabItem(tabName = "State_Map",
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
                                      value = as.Date('2020-04-01','%Y-%m-%d'),
                                      timeFormat = "%Y-%m-%d",
                                      animate = TRUE, step = 3),
                          style = "opacity: 0.80")
    ),
    #map end --------------------------------------------------------------------------------------------------------
    
    
    #report --------------------------------------------------------------------------------------------------------
    tabItem(tabName = "State_Comparison",
            fluidRow(column(12,
                            h3("Interactive Dashboard on State and Policy"),
                            "The following line plots show how the incident rate and mortality changes overtime and with certain policies being enforced")),
            pickerInput(inputId="state_dropdown",label='Select up to Three States',
                        choices=unique(states_complete$State),multiple=TRUE,
                        options=list(`max-options`=3),
                        selected='Alabama'
            ),
            selectInput(inputId='policy_dropdown',label='Select Policy',
                        choices=colnames(states_complete)[27:39]),
            #plotlyOutput("incident_rate_plot"),
            #plotlyOutput("mortality_rate_plot"),
            #plotlyOutput("testing_rate_plot"),
            #plotlyOutput("hospitalization_rate_plot"),
            plotlyOutput("state_line_plot",height="800px",width="1500px"),
            br(),
            br()
    ),
    
    tabItem(tabName="County_Comparison",
            fluidRow(column(12,
                            h3("Interactive Dashboard on County and Policy"),
                            "The following line plots show how the incident rate and mortality changes overtime and with certain policies being enforced")),
            pickerInput(inputId="county_dropdown",label='Select up to Three Counties',
                        choices=split((county_complete%>%filter(Date=='2020-10-03'))$Combined_Key,(county_complete%>%filter(Date=='2020-10-03'))$State),
                        multiple=TRUE,
                        options=list(`max-options`=3),
                        selected='Autauga, Alabama',
                        width='fit'
            ),
            selectInput(inputId='policy_dropdown_2',label='Select Policy',
                        choices=colnames(county_complete)[24:36]),
            plotlyOutput("incident_rate_plot_2"),
            plotlyOutput("mortality_rate_plot_2")
            
            
    ),
    #report end --------------------------------------------------------------------------------------------------------
    tabItem(tabName = "Reference",
            fluidPage(
              fluidRow(
                box(width = 15, title = "Raw Dataset", status = "primary",
                    solidHeader = TRUE,"The raw dataset for this project is from",
                    tags$a(href="https://github.com/OxCGRT/USA-covid-policy/tree/master/data","Oxford Covid-19 Government Response Tracker"),
                    "and",
                    tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data","JHU CSSE COVID-19 Dataset")
                    )),
              fluidRow(
                box(width = 15, title = "Project Code", status = "primary",
                    solidHeader = TRUE,"The code for this project can be find on",
                    tags$a(href="https://github.com/TZstatsADS/Fall2020-Project2-group8","our Github")
                    ))
            )
    )
  )
)
)