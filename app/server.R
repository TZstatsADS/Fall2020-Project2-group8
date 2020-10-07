packages.used <- c("shiny","leaflet", "wordcloud2", "DT", "stringr", "dplyr", "tidyverse", "tibble")
packages.needed <- setdiff(packages.used, intersect(installed.packages()[,1], packages.used))
if(length(packages.needed) > 0)
{
  install.packages(packages.needed, dependencies = TRUE)
}


library(shiny)
library(leaflet)
library(readr)
library(wordcloud2)
library(DT)
library(stringr)
library(tidyverse)
library(dplyr)
library(tibble)

shinyServer(function(input,output, session){
  load('../output/states_complete.RData')
  #map --------------------------------------------------------------------------------------------------------
  
  #map end --------------------------------------------------------------------------------------------------------
  
  
  #report --------------------------------------------------------------------------------------------------------
  
  #input$state_dropdown and input$policy_dropdown will give the values that the user inputted for the plot
  d <- reactive({
    req(input$state_dropdown) #don't display plot if nothing is selected
    filtered <- states_complete%>%
      filter(State%in%input$state_dropdown)
      
  }) 
  # Line Plot
  output$incident_rate_plot=renderPlotly({
    ggplotly(ggplot(d(),aes(x=Date, y=Incident_Rate,color=factor(get(input$policy_dropdown)),group=factor(get(input$policy_dropdown)),label=State,
                            text=paste('Date:',Date,
                                       '<br>Incident Rate:',format(round(Incident_Rate,3)),
                                       paste0('<br>',as.character(input$policy_dropdown),':'),factor(get(input$policy_dropdown)),
                                       '<br>State:',State))) +
              geom_point()+
              theme_bw() +
              xlab("Time") +
              ylab("Incident Rate") +
              ggtitle("Incident Rate Over Time")+
              scale_colour_manual(values = c("plum1", "plum2", "plum3","plum4","mediumorchid4"))+
              labs(color="Chosen Policy"),
            tooltip='text')
    })
  
  output$mortality_rate_plot=renderPlotly({
    ggplotly(ggplot(d(),aes(x=Date, y=Mortality_Rate,color=factor(get(input$policy_dropdown)),group=factor(get(input$policy_dropdown)),label=State,
                            text=paste('Date:',Date,
                                       '<br>Mortality Rate:',format(round(Mortality_Rate,3)),
                                       paste0('<br>',as.character(input$policy_dropdown),':'),factor(get(input$policy_dropdown)),
                                       '<br>State:',State))) +
               geom_point()+
               theme_bw() +
               xlab("Time") +
               ylab("Mortality Rate") +
               ggtitle("Mortality Rate Over Time")+
               scale_colour_manual(values = c("plum1", "plum2", "plum3","plum4","mediumorchid4"))+
               labs(color="Chosen Policy"),
             tooltip='text')
  })
  
  #report end --------------------------------------------------------------------------------------------------------
  
  
})