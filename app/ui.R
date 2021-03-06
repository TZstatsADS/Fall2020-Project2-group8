packages.used <- c("shiny", "shinydashboard", "leaflet", "shinyWidgets","plotly","shinythemes","tidyverse")
packages.needed <- setdiff(packages.used, intersect(installed.packages()[,1], packages.used))
if(length(packages.needed) > 0)
{
  install.packages(packages.needed, dependencies = TRUE)
}
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


load('./output/states_complete.RData')
load('./output/county_complete.RData')

dashboardPage(
  title="U.S. Covid-19 Policy Tracker",
  skin = "blue", 
  dashboardHeader(title = span("U.S. Covid-19 Policy Tracker",style="font-size: 16px")),
  dashboardSidebar(sidebarMenu(
    menuItem("Home", tabName = "Home", icon = icon("dashboard")),
    menuItem("Interactive Map", tabName = "Interactive Map", icon = icon("compass"),startExpanded = TRUE,
             menuSubItem("State Level Map",tabName="State_Level_Map",icon=icon("globe-americas")), 
             menuSubItem("County Level Map",tabName="County_Level_Map",icon=icon("map-marked"))),
    menuItem("Interactive Trend Plots", tabName = "Interactive Trend Plots", icon = icon("chart-line"),startExpanded = TRUE,
             menuSubItem("State Level Comparison",tabName="State_Level_Comparison",icon=icon("globe-americas")), 
             menuSubItem("County Level Comparison",tabName="County_Level_Comparison",icon=icon("map-marked"))
             ),
    menuItem("Reference", tabName = "Reference", icon = icon("th"))
    )),
  dashboardBody(fill = FALSE,tabItems(
    #home --------------------------------------------------------------------------------------------------------
    tabItem(tabName = "Home",
            fluidPage(
              fluidRow(
                box(width = 15, title = "Introduction", status = "primary",
                    solidHeader = TRUE, h3("U.S. Covid-19 Policy Tracker"),
                    h4("By Zihan Chen, Xujie Ma, Rohan Uppuluri & Jiaqi Yuan"),
                    h5("Covid-19 outbreaks have affected every region in the world. However, the magnitude of the impact has varied, as some regions have been successful in limiting the spread of the disease. There are many explanations of why some places have fewer cases than others. One of them is the government policy response."),
                    h5("In this project, we built a policy tracker for policy makers in the U.S. to explore the interaction of state government policy response and Covid-19 statistics overtime (from 01/22/20 to 10/12/20). In particular, we want to see how some key Covid-19 statistics (confirmed cases, death, infection rate, mortality rate, positive test rate, hospitalization rate) change overtime and how the state governments are publishing corresponding containment and closure policies, health system policies, and economic policies."))),
              fluidRow(box(width = 15, title = "User Group", status = "primary",
                           solidHeader = TRUE, h3("Why Did We Develop this App?"),
                           h5("Policy responses to Covid-19 are complex, context-specific and rapidly changing. Documenting the policies and the stringency can help policy makers to understand and assess government responses to Covid-19 over time. "),
                           h5("Our project provides the interactive plot of state government policy and Covid-19 statistics, and it could help to answer questions such as:"),
                           tags$div(tags$ul(
                             tags$li("1. How did policy stringency change with the evolving situation?"),
                             tags$li("2. How Covid-19 statistics change after policy stringency change?")
                           )),
                           h5("However, it's important to note that this app should mainly be used as a first step in deciding on policy implementation levels. Correlations are not causations, so 
                              any correlations between policy levels and Covid-19 statistics found through this app should be studied further by the user to determine if it's actually a causation before making any policy changes."))),
              fluidRow(box(width = 15, title = "App Contents", status = "primary",
                           solidHeader = TRUE, h3("What Does This App Include?"),
                           tags$div(tags$ul(
                             tags$li("Interactive Map: This map contains two U.S. geological graphs, one at state level and another at county level. Each graph records the confirmed cases and deaths. The user can select the state, Covid-19 Statistics, and time point"),
                             tags$li("Interactive Trend Plots : This tab contains time series plots on when did each state/county’s enforced new policies and how key Covid-19 measures changes overtime. The user can select up to three states/counties at one time."),
                             tags$li("Reference: This tab provides details on what datasets we used and our project code.")
                             
                           )))),
              fluidRow(box(width = 15, title = "Policies and Covid-19 statistics", status = "primary",
                           solidHeader = TRUE, h3("What Policies and Covid-19 Statistics Are Included?"),
                           h4("Data Description"),
                           h5("In this project, we used JHU datasets and OxCGRT dataset (details in the Reference tab). We have three main policy areas and six Covid-19 statistics."),
                           h5("The policy areas include:"),
                           tags$div(tags$ul(
                             tags$li("Policy Area 1: Containment and Closure Policies"),
                             tags$div(tags$ul(
                               tags$li("1. Closing of schools and universities"),
                               tags$li("2. Closing of workplaces"),
                               tags$li("3. Cancelling public events"),
                               tags$li("4. Limits on private gatherings"),
                               tags$li("5. Closing of public transport"),
                               tags$li("6. Orders to shelter-in-place and otherwise confine to the home"),
                               tags$li("7. Restrictions on internal movement between cities/regions"),
                               tags$li("8. Restrictions on international travel for foreign travelers"))
                             ),br(),
                             tags$li("Policy Area 2: Economic Policies"),
                             tags$div(tags$ul(
                               tags$li("1. If the government is providing direct cash payments to people who lose their jobs or cannot work (Note: only includes payments to firms if explicitly linked to payroll/salaries)"),
                               tags$li("2. If the government is freezing financial obligations for households (eg stopping loan repayments, preventing services like water from stopping, or banning evictions)"))
                             ),br(),
                             
                             tags$li("Policy Area 3: Health System Policies"),
                             tags$div(tags$ul(
                                tags$li("1. Presence of public info campaigns"),
                                tags$li("2. Government policy on who has access to testing (Note: this records policies about testing for current infection (PCR tests) not testing for immunity (antibody test))"),
                                tags$li("3. Government policy on contact tracing after a positive diagnosis (Note: we are looking for policies that would identify all people potentially exposed to Covid-19; voluntary bluetooth apps are unlikely to achieve this)"))
                               ),br(),

                           )),
                           
                           h5("The Covid-19 statistics include:"),
                           tags$div(tags$ul(
                             tags$li("1. Total Confirmed Cases"),
                             tags$li("2. Total Number of Deaths"),
                             tags$li("3. Incidence Rate: cases per 100,000 people"),
                             tags$li("4. Mortality Rate: Number recorded deaths * 100/ Number confirmed cases"),
                             tags$li("5. Testing Rate: Total test results (positive + negative) per 100,000 people"),
                             tags$li("6. Hospitalization Rate: Total number hospitalized / Number cases"))
                           ),br(),br(),
                           h4("Data Quality"),
                           h5(""),
                           h5("There are some potential data collection biases for Covid-19 statistics data collection, as there could be under-ascertainment of mild cases and time lags."),
                           h5("There are some limitations of our dataset, including:"),
                           tags$div(tags$ul(
                             tags$li("1. Missing values: Hospitalization rate and testing rate are only available starting on 04/12/20 at state level, and they are not available for any date at the county level."),
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

                ))
              )
              )
              
              
              )),
    #home end --------------------------------------------------------------------------------------------------------
    
    
    #map --------------------------------------------------------------------------------------------------------
    tabItem(tabName = "State_Level_Map",
            h3("Interactive State Level Map"),
            h5("This map tracks the Covid-19 statistics and policy levels of each state of US."),
            h5("A darker color means more cases, deaths, or stricter policy levels were implemented relative to other states on that day. If you click on the 'play' button on the left, you could see the evolution of the covid-19 situation in every 3 days."),
            h5("Note that testing rate and hospitalization rate information is only available starting from April 12, 2020."),
            h5("Let's get started by selecting a input date!"),
            tags$div(id='my_div',
                     class='my_class',
                     selectInput(inputId='stats_dropdown',label='Select Covid-19 Statistics or Policy',
                                 choices=c('Cases','Deaths','C1_School_closing','C2_Workplace_closing','C3_Cancel_public_events','C4_Restrictions_on_gatherings','C5_Close_public_transport',
                                           'C6_Stay_at_home_requirements','C7_Restrictions_on_internal_movement','C8_International_travel_controls','E1_Income_support','E2_Debt_contract_relief',
                                           'H1_Public_information_campaigns','H2_Testing_policy','H3_Contact_tracing'))),
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
    
    tabItem(tabName = "County_Level_Map",
            h3("Interactive County Level Map"),
            h5("This map gives a closer look at situations of counties in each state."),
            h5("You can select as many states as you like and find how it is going in the counties. In this map, we exclude the policies measures, testing rates, and hospitalization rates as they are only available at the state level."),
            h5("Let's get started by selecting a input date!"),
            
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
    
    
    #trend plot --------------------------------------------------------------------------------------------------------
    tabItem(tabName = "State_Level_Comparison",
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
    
    tabItem(tabName="County_Level_Comparison",
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
    #trend plot end --------------------------------------------------------------------------------------------------------
    tabItem(tabName = "Reference",
            fluidPage(
              fluidRow(
                box(width = 15, title = "Raw Datasets", status = "primary",
                    solidHeader = TRUE,"The raw datasets for this project are from",
                    tags$a(href="https://github.com/OxCGRT/USA-covid-policy/tree/master/data","Oxford Covid-19 Government Response Tracker"),
                    "and",
                    tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data","JHU CSSE COVID-19 Dataset"),
                    ".",
                    "We used", 
                    tags$a(href="https://eric.clst.org/tech/usgeojson/", "geojson data"),
                    "to plot the U.S. geographic map, which we used in the state map and county map.",br(),br(),
                    "We used the JHU datasets(csse_covid_19_daily_reports_us & time_series_covid19_confirmed_US.csv) and OxCGRT(OxCGRT_US_latest.csv) to construct our dataset. The JHU datasets contain information on the Covid-19 statistics from sources like the U.S. Centers for Disease Control and Prevention, which have a set of consistent standards for working with state and local health departments to get data. The OxCGRT systematically collects publicly available information on several different common policy responses governments have taken, records these policies on a scale to reflect the extent of government action, and aggregates these scores into a suite of policy indices."
                    )),
              fluidRow(
                box(width = 15, title = "Project Code", status = "primary",
                    solidHeader = TRUE,"The code for this project can be found on",
                    tags$a(href="https://github.com/TZstatsADS/Fall2020-Project2-group8","our Github.")
                    ))
              
            )
    )
  )
)
)