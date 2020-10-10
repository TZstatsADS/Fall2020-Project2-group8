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

#---------------------------------------Loading the processed data---------------------------------------------
load('../output/states_complete.RData')
load('../output/county_complete.RData')
source("global.R")

shinyServer(function(input,output, session){
  #map --------------------------------------------------------------------------------------------------------
  
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    map <- leaflet(states) %>%
      setView(-96, 37.8, 4) %>%
      addTiles()

  })

  observe({
    if(!is.null(input$date_map)){
      select_date <- format.Date(input$date_map,'%Y-%m-%d')
    }


    confirmed_at_today <- Confirmed %>% dplyr::select(State,select_date)
    confirmed_with_order <- data.frame(State = states$NAME) %>% left_join(confirmed_at_today)
    confirmed_number <- as.numeric(unlist(confirmed_with_order[select_date]))

    bins <- c(0,exp(0:as.integer(log(max(confirmed_number,na.rm = TRUE)))),Inf)
    map_pal <- colorBin("YlOrRd", domain = states$STATE, bins = bins)
    states$STATE <- confirmed_number

    labels <- sprintf(
      "<strong>%s</strong><br/>%g people confirmed",
      states$NAME, states$STATE
    ) %>% lapply(htmltools::HTML)

    leafletProxy("map", data = states)%>%
      addPolygons(
        fillColor = ~map_pal(STATE),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))

  })


  
  output$map2 <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    map2 <- leaflet(states) %>%
      setView(-96, 37.8, 4) %>%
      addTiles()
    
  })
  
  observe({
    if(!is.null(input$date_map)){
      select_date <- format.Date(input$date_map,'%Y-%m-%d')
    }
    
    
    confirmed_at_today <- Confirmed %>% dplyr::select(State,select_date)
    confirmed_with_order <- data.frame(State = states$NAME) %>% left_join(confirmed_at_today)
    confirmed_number <- as.numeric(unlist(confirmed_with_order[select_date]))
    
    bins <- c(0,exp(0:as.integer(log(max(confirmed_number,na.rm = TRUE)))),Inf)
    map_pal <- colorBin("YlOrRd", domain = states$STATE, bins = bins)
    states$STATE <- confirmed_number
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g people confirmed",
      states$NAME, states$STATE
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy("map2", data = states)%>%
      addPolygons(
        fillColor = ~map_pal(STATE),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) 
    
  })
  
  #map end --------------------------------------------------------------------------------------------------------
  
  
  #report --------------------------------------------------------------------------------------------------------
  
  #input$state_dropdown and input$policy_dropdown will give the values that the user inputted for the plot
  d_state <- reactive({
    req(input$state_dropdown) #don't display plot if nothing is selected
    
    filtered <- states_complete%>%
      filter(State%in%input$state_dropdown)
    filtered[[input$policy_dropdown]]<-factor(filtered[[input$policy_dropdown]],levels=unique(states_complete[[input$policy_dropdown]]))
    #reorder factor levels alphabetically (maybe do this in data_processing.R instead)
    levels(filtered[[input$policy_dropdown]])<-levels(filtered[[input$policy_dropdown]])[order(levels(filtered[[input$policy_dropdown]]))]
    return(filtered)
    
  }) 
  # Line Plot
  output$incident_rate_plot=renderPlotly({
    ggplotly(ggplot(d_state(),aes(x=Date, y=Incident_Rate,color=str_wrap(factor(get(input$policy_dropdown)),20),
                                  text=paste('Date:',Date,
                                             '<br>Incident Rate:',format(round(Incident_Rate,3)),
                                             str_wrap(paste0('<br>',as.character(input$policy_dropdown),': ',factor(get(input$policy_dropdown))),60),
                                             '<br>State:',State))) +
               geom_point()+
               theme_bw() +
               xlab("Time") +
               ylab("Incident Rate") +
               ggtitle("Incident Rate Over Time")+
               #scale_colour_brewer(palette='Blues',drop=FALSE)+
               scale_colour_manual(values = c("plum1", "plum2", "plum3","plum4","mediumorchid4"),drop=FALSE)+
               labs(color=as.character(input$policy_dropdown)),
             tooltip='text')
  })
  
  output$mortality_rate_plot=renderPlotly({
    ggplotly(ggplot(d_state(),aes(x=Date, y=Mortality_Rate,color=str_wrap(factor(get(input$policy_dropdown)),20),label=State,
                                  text=paste('Date:',Date,
                                             '<br>Mortality Rate:',format(round(Mortality_Rate,3)),
                                             str_wrap(paste0('<br>',as.character(input$policy_dropdown),': ',factor(get(input$policy_dropdown))),60),
                                             '<br>State:',State))) +
               geom_point()+
               theme_bw() +
               xlab("Time") +
               ylab("Mortality Rate") +
               ggtitle("Mortality Rate Over Time")+
               scale_colour_manual(values = c("plum1", "plum2", "plum3","plum4","mediumorchid4"),drop=FALSE)+
               labs(color=as.character(input$policy_dropdown)),
             tooltip='text')
  })
  
  output$testing_rate_plot=renderPlotly({
    ggplotly(ggplot(d_state(),aes(x=Date, y=Testing_Rate,color=str_wrap(factor(get(input$policy_dropdown)),20),label=State,
                                  text=paste('Date:',Date,
                                             '<br>Testing Rate:',format(round(Testing_Rate,3)),
                                             str_wrap(paste0('<br>',as.character(input$policy_dropdown),': ',factor(get(input$policy_dropdown))),60),
                                             '<br>State:',State))) +
               geom_point()+
               theme_bw() +
               xlab("Time") +
               ylab("Testing Rate") +
               ggtitle("Testing Rate Over Time")+
               scale_colour_manual(values = c("plum1", "plum2", "plum3","plum4","mediumorchid4"),drop=FALSE)+
               labs(color=as.character(input$policy_dropdown)),
             tooltip='text')
  })
  
  output$hospitalization_rate_plot=renderPlotly({
    ggplotly(ggplot(d_state(),aes(x=Date, y=Hospitalization_Rate,color=str_wrap(factor(get(input$policy_dropdown)),20),label=State,
                                  text=paste('Date:',Date,
                                             '<br>Hospitalization Rate:',format(round(Hospitalization_Rate,3)),
                                             str_wrap(paste0('<br>',as.character(input$policy_dropdown),': ',factor(get(input$policy_dropdown))),60),
                                             '<br>State:',State))) +
               geom_point()+
               theme_bw() +
               xlab("Time") +
               ylab("Hospitalization Rate") +
               ggtitle("Hospitalization Rate Over Time")+
               scale_colour_manual(values = c("plum1", "plum2", "plum3","plum4","mediumorchid4"),drop=FALSE)+
               labs(color=as.character(input$policy_dropdown)),
             tooltip='text')
  })
  
  
  
  
  d_county <- reactive({
    req(input$county_dropdown) #don't display plot if nothing is selected
    
    filtered <- county_complete%>%
      filter(Combined_Key%in%input$county_dropdown)
    #filtered[[input$policy_dropdown_2]]<-factor(filtered[[input$policy_dropdown_2]],levels=unique(states_complete[[input$policy_dropdown]]))
    #reorder factor levels alphabetically (maybe do this in data_processing.R instead)
    levels(filtered[[input$policy_dropdown_2]])<-levels(filtered[[input$policy_dropdown_2]])[order(levels(filtered[[input$policy_dropdown_2]]))]
    return(filtered)
    
  }) 
  # Line Plot
  output$incident_rate_plot_2=renderPlotly({
    ggplotly(ggplot(d_county(),aes(x=Date, y=Incident_Rate,color=str_wrap(factor(get(input$policy_dropdown_2)),20),label=Combined_Key,
                                   text=paste('Date:',Date,
                                              '<br>Incident Rate:',format(round(Incident_Rate,3)),
                                              str_wrap(paste0('<br>',as.character(input$policy_dropdown_2),': ',factor(get(input$policy_dropdown_2))),60),
                                              '<br>County:',Combined_Key))) +
               geom_point()+
               theme_bw() +
               xlab("Time") +
               ylab("Incident Rate") +
               ggtitle("Incident Rate Over Time")+
               scale_colour_manual(values = c("plum1", "plum2", "plum3","plum4","mediumorchid4"),drop=FALSE)+
               labs(color=as.character(input$policy_dropdown_2)),
             tooltip='text')
  })
  
  output$mortality_rate_plot_2=renderPlotly({
    ggplotly(ggplot(d_county(),aes(x=Date, y=Mortality_Rate,color=str_wrap(factor(get(input$policy_dropdown_2)),20),label=Combined_Key,
                                   text=paste('Date:',Date,
                                              '<br>Mortality Rate:',format(round(Mortality_Rate,3)),
                                              str_wrap(paste0('<br>',as.character(input$policy_dropdown_2),': ',factor(get(input$policy_dropdown_2))),60),
                                              '<br>County:',Combined_Key))) +
               geom_point()+
               theme_bw() +
               xlab("Time") +
               ylab("Mortality Rate") +
               ggtitle("Mortality Rate Over Time")+
               scale_colour_manual(values = c("plum1", "plum2", "plum3","plum4","mediumorchid4"),drop=FALSE)+
               labs(color=as.character(input$policy_dropdown_2)),
             tooltip='text')
  })
  
  #report end --------------------------------------------------------------------------------------------------------
  
  
})