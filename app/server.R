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
    filtered <- states_complete
    if (length(input$state_dropdown)==1)
    {
      filtered%>%filter(State==input$state_dropdown)
    }
    else if (length(input$state_dropdown)==2)
    {
      filtered%>%filter(State==input$state_dropdown[1]|State==input$state_dropdown[2])
    }
    else
    {
      filtered%>%filter(State==input$state_dropdown[1]|State==input$state_dropdown[2]|State==input$state_dropdown[3])
    }
      
  }) 
  # Line Plot
  output$plot=renderPlotly({
    ggplotly(ggplot(d(),aes(x=Date, y=Incident_Rate,color=factor(get(input$policy_dropdown)),label=State)) +
      geom_point()+
      geom_text(aes(label=ifelse(Date==max(Date),as.character(State),'')))+
      theme_bw() +
      xlab("Time") +
      ylab("Incident Rate") +
      ggtitle("Incident Rate Over Time")+
      scale_colour_manual(values = c("plum1", "plum2", "plum3","plum4","mediumorchid4"))+
      labs(color="Chosen Policy"))
    })
  
  #report end --------------------------------------------------------------------------------------------------------
  
  
})