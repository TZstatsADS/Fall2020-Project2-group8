packages.used <- c("shiny", "shinydashboard", "leaflet", "shinyWidgets","plotly","shinythemes","wordcloud2", "DT")
packages.needed <- setdiff(packages.used, intersect(installed.packages()[,1], packages.used))
if(length(packages.needed) > 0)
{
  install.packages(packages.needed, dependencies = TRUE)
}

library(plotly)
library(shinythemes)
library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyWidgets)
library(wordcloud2)
library(DT)

load('../output/states_complete.RData')

dashboardPage(
  skin = "purple", #we don't have to use these colors, titles, and icons
  dashboardHeader(title = "Covid State Policy Tracker"),
  dashboardSidebar(sidebarMenu(
    menuItem("Home", tabName = "Home", icon = icon("dashboard")),
    menuItem("Map", tabName = "Map", icon = icon("compass")),
    menuItem("Report", tabName = "Report", icon = icon("pencil-ruler"))
  )),
  dashboardBody(fill = FALSE,tabItems(
    #home --------------------------------------------------------------------------------------------------------
    tabItem(tabName = "Home"
    ),   
    #home end --------------------------------------------------------------------------------------------------------
    
    
    #map --------------------------------------------------------------------------------------------------------
    tabItem(tabName = "Map"
    ),
    #map end --------------------------------------------------------------------------------------------------------
    
    
    #report --------------------------------------------------------------------------------------------------------
    tabItem(tabName = "Report",
            fluidRow(column(12,
                            h3("Interactive Dashboard on State and Policy"),
                            "The following line plots show how the incident rate changes overtime and with certain policies being enforced")),
            selectInput(inputId="state_dropdown",label='Select State',
                        choices=unique(states_complete$State)),
            selectInput(inputId='policy_dropdown',label='Select Policy',
                        choices=colnames(states_complete)[16:58][!endsWith(colnames(states_complete)[16:60],'Notes') & !endsWith(colnames(states_complete)[16:60],'Flag')][1:17]),
            plotOutput("plot")
    )
    #report end --------------------------------------------------------------------------------------------------------
  )
  )
)