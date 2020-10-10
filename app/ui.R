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
  skin = "purple", #we don't have to use these colors, titles, and icons
  dashboardHeader(title = "Covid State Policy Tracker"),
  dashboardSidebar(sidebarMenu(
    menuItem("Home", tabName = "Home", icon = icon("dashboard")),
    menuItem("Map", tabName = "Map", icon = icon("compass")),
    menuItem("Report", tabName = "Report", icon = icon("chart-line"),startExpanded = TRUE,
             menuSubItem("State Comparison",tabName="State_Comparison",icon=icon("users")), #find better icons
             menuSubItem("County Comparison",tabName="County_Comparison",icon=icon("user"))
             )
  )),
  dashboardBody(fill = FALSE,tabItems(
    #home --------------------------------------------------------------------------------------------------------
    tabItem(tabName = "Home"
    ),   
    #home end --------------------------------------------------------------------------------------------------------
    
    
    #map --------------------------------------------------------------------------------------------------------
    tabItem(tabName = "Map",
            leafletOutput("map", width = "100%", height = "1200"),
            absolutePanel(id = "control", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
                          top = 300, left = 20, right = "auto", bottom = "auto", width = 250, height = "auto",
                          sliderInput('date_map','Input Date:',
                                      #first day of data recording
                                      min = as.Date(date_choices[1]),
                                      #present day of data recording
                                      max = as.Date(tail(date_choices,1)),
                                      value = as.Date('2020-04-01','%Y-%m-%d'),
                                      timeFormat = "%Y-%m-%d",
                                      animate = TRUE, step = 5),
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
            plotlyOutput("incident_rate_plot"),
            plotlyOutput("mortality_rate_plot"),
            plotlyOutput("testing_rate_plot"),
            plotlyOutput("hospitalization_rate_plot")
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
            )
    #report end --------------------------------------------------------------------------------------------------------
  )
  )
)