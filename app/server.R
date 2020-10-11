packages.used <- c("shiny","leaflet", "wordcloud2", "DT", "stringr", "dplyr", "tidyverse", "tibble","RColorBrewer")
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
library(RColorBrewer)

#---------------------------------------Loading the processed data---------------------------------------------
load('../output/states_complete.RData')
load('../output/county_complete.RData')
source("global.R")

shinyServer(function(input,output, session){
  #map --------------------------------------------------------------------------------------------------------
  
  date <- reactive({
    if(!is.null(input$date_map)){
      return(format.Date(input$date_map,'%Y-%m-%d')) 
    }
  })
  
  get_df <- reactive({
    if(!is.null(input$stats_dropdown)){
      if (input$stats_dropdown == "Cases"){
        return(Confirmed)
      }
      else{
        return(Deaths)
      } 
    }
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    map <- leaflet(states) %>%
      setView(-96, 37.8, 4) %>%
      addTiles()

  })

  observe({
    select_date <- date()
    df_temp <- get_df()
    confirmed_at_today <- df_temp %>% dplyr::select(State,select_date)
    confirmed_with_order <- data.frame(State = states$NAME) %>% left_join(confirmed_at_today)
    confirmed_number <- as.numeric(unlist(confirmed_with_order[select_date]))
   
    num_temp <- confirmed_number
    bins <- c(0,exp(0:as.integer(log(1+max(num_temp,na.rm = TRUE)))),Inf)
    map_pal <- colorBin("YlOrRd", domain = states$STATE, bins = bins)
    states$STATE <- num_temp
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g %s",
      states$NAME, states$STATE,input$stats_dropdown
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

  #----county map ------------------------------------
  
  county_date <- reactive({
    if(!is.null(input$date_map)){
      return(format.Date(input$county_date_map,'%Y-%m-%d')) 
    }
  })
  
  find_county_name <- reactive({
    if(!is.null(input$county_name)){
      return(input$county_name) 
    }
  })
  
  county_get_df <- reactive({
    if(!is.null(input$county_stats_dropdown)){
      if (input$county_stats_dropdown == "Cases"){
        return(cdata_temp)
      }
      else{
        return(ddata_temp)
      } 
    }
  })
  
  
  output$county_map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    county_map <- leaflet() %>%
      setView(-96, 37.8, 4) %>%
      addTiles()
    
  })
  
  
  observe({
    
    county_select_date <- county_date()
    county_df_temp <- county_get_df()
    county_name_array <- find_county_name()
    
    
    for (i in 1:length(county_name_array)){
      county_name <- county_name_array[i]
      
      
      cdata<-county_df_temp[,c('Admin2','Province_State','STATE',county_select_date)]
      cdata_temp2<-left_join(data.frame(counties),cdata,by=c('STATE'='STATE','NAME'='Admin2'))
      counties$Confirmed<-cdata_temp2[,county_select_date]
      #-----------find_state_code
      df_find_statecode <- data.frame(cdata %>% group_by(Province_State) %>% summarize(code = first(STATE)))
      county_number <- df_find_statecode[df_find_statecode$Province_State==county_name,2]
      #-----------find_state_location
      county_loc <- c(df_getloc[df_getloc$NAME==county_name,2],df_getloc[df_getloc$NAME==county_name,3])
      
      cdata_selected_county <- counties[counties$STATE==county_number,c('NAME',"Confirmed")]
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%g %s",
        unlist(map(cdata_selected_county$NAME,convert_xf1)), cdata_selected_county$Confirmed,input$county_stats_dropdown
      ) %>% lapply(htmltools::HTML)
      
      find_max <- max(county_df_temp[,c("2020-10-01")])
      bins <- c(0,exp(0:as.integer(log(1+find_max))),Inf)
      county_map_pal <- colorBin("YlOrRd", domain = cdata_selected_county$Confirmed, bins = bins)
      
      leafletProxy("county_map", data = cdata_selected_county)%>%addTiles()%>%
        addPolygons(
          fillColor = ~county_map_pal(Confirmed),
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
      
    }
    
  })
  
  
  

  
  #map end --------------------------------------------------------------------------------------------------------
  
  
  #report --------------------------------------------------------------------------------------------------------
  
  #input$state_dropdown and input$policy_dropdown will give the values that the user inputted for the plot
  d_state <- reactive({
    req(input$state_dropdown) #don't display plot if nothing is selected
    
    filtered <- states_complete%>%
      filter(State%in%input$state_dropdown)%>%
      mutate(State_Policy=paste(State,get(input$policy_dropdown),sep=": "))%>%
      mutate(State_Policy=factor(State_Policy))
    #filtered[[input$policy_dropdown]]<-factor(filtered[[input$policy_dropdown]],levels=unique(states_complete[[input$policy_dropdown]]))
    #reorder factor levels alphabetically (maybe do this in data_processing.R instead)
    levels(filtered$State_Policy)<-levels(filtered$State_Policy)[order(levels(filtered$State_Policy))]
    
    policy_lengths<-rle(unlist(map(strsplit(levels(filtered$State_Policy),'[:]'),1)))$lengths
    policy_lengths<-append(policy_lengths,rep(3,3-length(policy_lengths))) #if num states < 3, fill rest of vector with 3s (minimum number of colors for a palette)
    custom_colors<-c(brewer.pal(name="Blues",n=policy_lengths[1]),brewer.pal(name="Greens",n=policy_lengths[2]),brewer.pal(name="Purples",n=policy_lengths[3]))
    
    return(list(filtered,custom_colors))
    
  }) 
  # Line Plot
  output$state_line_plot=renderPlotly({
    p1<-ggplotly(ggplot(d_state()[[1]],aes(x=Date, y=Incident_Rate,color=str_wrap(factor((State_Policy)),20),
                                           text=paste('Date:',Date,
                                                      '<br>Incident Rate:',format(round(Incident_Rate,3)),
                                                      str_wrap(paste0('<br>',as.character(input$policy_dropdown),': ',factor(get(input$policy_dropdown))),60),
                                                      '<br>State:',State)))+
                   geom_point()+
                   theme_bw() +
                   xlab("Time") +
                   ylab("Incident Rate") +
                   scale_colour_manual(values=d_state()[[2]],drop=FALSE)+
                   theme(legend.title = element_blank())+
                   labs(color=as.character(input$policy_dropdown)),
                 tooltip='text')%>%
      add_annotations(
        text = "Incident Rate Over Time",
        x = 0,
        y = 1,
        yref = "paper",
        xref = "paper",
        xanchor = "left",
        yanchor = "top",
        yshift = 20,
        showarrow = FALSE,
        font = list(size = 15)
      )
    p2<-ggplotly(ggplot(d_state()[[1]],aes(x=Date, y=Mortality_Rate,color=str_wrap(factor((State_Policy)),20),label=State,
                                           text=paste('Date:',Date,
                                                      '<br>Mortality Rate:',format(round(Mortality_Rate,3)),
                                                      str_wrap(paste0('<br>',as.character(input$policy_dropdown),': ',factor(get(input$policy_dropdown))),60),
                                                      '<br>State:',State)))+
                   geom_point()+
                   theme_bw() +
                   xlab("Time") +
                   ylab("Mortality Rate") +
                   scale_colour_manual(values=d_state()[[2]],drop=FALSE)+
                   theme(legend.title = element_blank())+
                   labs(color=as.character(input$policy_dropdown)),
                 tooltip='text')%>%
      add_annotations(
        text = "Mortality Rate Over Time",
        x = 0,
        y = 1,
        yref = "paper",
        xref = "paper",
        xanchor = "left",
        yanchor = "top",
        yshift = 20,
        showarrow = FALSE,
        font = list(size = 15)
      )
    p3<-ggplotly(ggplot(d_state()[[1]],aes(x=Date, y=Testing_Rate,color=str_wrap(factor((State_Policy)),20),label=State,
                                           text=paste('Date:',Date,
                                                      '<br>Testing Rate:',format(round(Testing_Rate,3)),
                                                      str_wrap(paste0('<br>',as.character(input$policy_dropdown),': ',factor(get(input$policy_dropdown))),60),
                                                      '<br>State:',State)))+
                   geom_point()+
                   theme_bw() +
                   xlab("Time") +
                   ylab("Testing Rate") +
                   scale_colour_manual(values=d_state()[[2]],drop=FALSE)+
                   theme(legend.title = element_blank())+
                   labs(color=as.character(input$policy_dropdown)),
                 tooltip='text')%>%
      add_annotations(
        text = "Testing Rate Over Time",
        x = 0,
        y = 1,
        yref = "paper",
        xref = "paper",
        xanchor = "left",
        yanchor = "top",
        yshift = 20,
        showarrow = FALSE,
        font = list(size = 15)
      )
    p4<-ggplotly(ggplot(d_state()[[1]],aes(x=Date, y=Hospitalization_Rate,color=str_wrap(factor((State_Policy)),20),label=State,
                                           text=paste('Date:',Date,
                                                      '<br>Hospitalization Rate:',format(round(Hospitalization_Rate,3)),
                                                      str_wrap(paste0('<br>',as.character(input$policy_dropdown),': ',factor(get(input$policy_dropdown))),60),
                                                      '<br>State:',State)))+
                   geom_point()+
                   theme_bw() +
                   xlab("Time") +
                   ylab("Hospitalization Rate") +
                   scale_colour_manual(values=d_state()[[2]],drop=FALSE)+
                   theme(legend.title = element_blank())+
                   labs(color=as.character(input$policy_dropdown)),
                 tooltip='text')%>%
      add_annotations(
        text = "Hospitalization Rate Over Time",
        x = 0,
        y = 1,
        yref = "paper",
        xref = "paper",
        xanchor = "left",
        yanchor = "top",
        yshift = 20,
        showarrow = FALSE,
        font = list(size = 15)
      )
    
    subplot(style(p1,showlegend=F),style(p2,showlegend=F),style(p3,showlegend=F),style(p4,showlegend=T),nrows=2,shareX=F,shareY=F,titleX=T,titleY=T,margin=0.065)%>%
      layout(paper_bgcolor='transparent')
    
  })
  
  
  
  d_county <- reactive({
    req(input$county_dropdown) #don't display plot if nothing is selected
    
    filtered <- county_complete%>%
      filter(Combined_Key%in%input$county_dropdown)%>%
      mutate(County_Policy=paste(County,get(input$policy_dropdown_2),sep=": "))%>%
      mutate(County_Policy=factor(County_Policy))
    #reorder factor levels alphabetically (maybe do this in data_processing.R instead)
    levels(filtered$County_Policy)<-levels(filtered$County_Policy)[order(levels(filtered$County_Policy))]
    
    policy_lengths<-rle(unlist(map(strsplit(levels(filtered$County_Policy),'[:]'),1)))$lengths
    policy_lengths<-append(policy_lengths,rep(3,3-length(policy_lengths))) #if num states < 3, fill rest of vector with 3s (minimum number of colors for a palette)
    custom_colors<-c(brewer.pal(name="Blues",n=policy_lengths[1]),brewer.pal(name="Greens",n=policy_lengths[2]),brewer.pal(name="Purples",n=policy_lengths[3]))
    
    return(list(filtered,custom_colors))
    
  }) 
  # Line Plot
  output$incident_rate_plot_2=renderPlotly({
    ggplotly(ggplot(d_county()[[1]],aes(x=Date, y=Incident_Rate,color=str_wrap(factor((County_Policy)),20),label=Combined_Key,
                                   text=paste('Date:',Date,
                                              '<br>Incident Rate:',format(round(Incident_Rate,3)),
                                              str_wrap(paste0('<br>',as.character(input$policy_dropdown_2),': ',factor(get(input$policy_dropdown_2))),60),
                                              '<br>County:',Combined_Key))) +
               geom_point()+
               theme_bw() +
               xlab("Time") +
               ylab("Incident Rate") +
               ggtitle("Incident Rate Over Time")+
               scale_colour_manual(values=d_county()[[2]],drop=FALSE)+
               labs(color=as.character(input$policy_dropdown_2)),
             tooltip='text')
  })
  
  output$mortality_rate_plot_2=renderPlotly({
    ggplotly(ggplot(d_county()[[1]],aes(x=Date, y=Mortality_Rate,color=str_wrap(factor((County_Policy)),20),label=Combined_Key,
                                   text=paste('Date:',Date,
                                              '<br>Mortality Rate:',format(round(Mortality_Rate,3)),
                                              str_wrap(paste0('<br>',as.character(input$policy_dropdown_2),': ',factor(get(input$policy_dropdown_2))),60),
                                              '<br>County:',Combined_Key))) +
               geom_point()+
               theme_bw() +
               xlab("Time") +
               ylab("Mortality Rate") +
               ggtitle("Mortality Rate Over Time")+
               scale_colour_manual(values=d_county()[[2]],drop=FALSE)+
               labs(color=as.character(input$policy_dropdown_2)),
             tooltip='text')
  })
  
  #report end --------------------------------------------------------------------------------------------------------
  
  
})