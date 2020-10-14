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
load('./output/states_complete.RData')
load('./output/county_complete.RData')
source('global.R')

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
      else if (input$stats_dropdown == "Deaths"){
        return(Deaths)
      } 
      else if (input$stats_dropdown=='C1_School_closing'){
        return(C1_School_closing)
      }
      else if (input$stats_dropdown=='C2_Workplace_closing'){
        return(C2_Workplace_closing)
      }
      else if (input$stats_dropdown=='C3_Cancel_public_events'){
        return(C3_Cancel_public_events)
      }
      else if (input$stats_dropdown=='C4_Restrictions_on_gatherings'){
        return(C4_Restrictions_on_gatherings)
      }
      else if (input$stats_dropdown=='C5_Close_public_transport'){
        return(C5_Close_public_transport)
      }
      else if (input$stats_dropdown=='C6_Stay_at_home_requirements'){
        return(C6_Stay_at_home_requirements)
      }
      else if (input$stats_dropdown=='C7_Restrictions_on_internal_movement'){
        return(C7_Restrictions_on_internal_movement)
      }
      else if (input$stats_dropdown=='C8_International_travel_controls'){
        return(C8_International_travel_controls)
      }
      else if (input$stats_dropdown=='E1_Income_support'){
        return(E1_Income_support)
      }
      else if (input$stats_dropdown=='E2_Debt_contract_relief'){
        return(E2_Debt_contract_relief)
      }
      else if (input$stats_dropdown=='H1_Public_information_campaigns'){
        return(H1_Public_information_campaigns)
      }
      else if (input$stats_dropdown=='H2_Testing_policy'){
        return(H2_Testing_policy)
      }
      else {
        return(H3_Contact_tracing)
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
    
    if(input$stats_dropdown=='Cases'|input$stats_dropdown=='Deaths')
    {
      confirmed_number <- as.numeric(unlist(confirmed_with_order[select_date]))
      rates_with_order <- data.frame(State = states$NAME) %>% left_join(states_complete %>% filter(Date == select_date) %>% dplyr::select(State,Date,Incident_Rate,Mortality_Rate,Testing_Rate,Hospitalization_Rate))
      num_temp <- confirmed_number
      bins <- c(0,exp(0:as.integer(log(1+max(num_temp,na.rm = TRUE)))),Inf)
      map_pal <- colorBin("YlOrRd", domain = states$STATE, bins = bins)
      
      states$STATE <- num_temp
      states$Incident_Rate <- rates_with_order$Incident_Rate
      states$Mortality_Rate <- rates_with_order$Mortality_Rate
      states$Testing_Rate <- rates_with_order$Testing_Rate
      states$Hospitalization_Rate <- rates_with_order$Hospitalization_Rate
      
      if (select_date < format.Date("2020-04-12",'%Y-%m-%d')){
        labels <- sprintf(
          "<strong>%s</strong><br/>%s %s </br> Incident Rate: %g </br> Mortality Rate: %g",
          states$NAME, states$STATE,input$stats_dropdown,states$Incident_Rate,states$Mortality_Rate
        ) %>% lapply(htmltools::HTML)
      }
      else{
        labels <- sprintf(
          "<strong>%s</strong><br/>%s %s </br> Incident Rate: %g </br> Mortality Rate: %g </br> Testing Rate: %g </br> Hospitalization Rate: %g",
          states$NAME, states$STATE,input$stats_dropdown,states$Incident_Rate,states$Mortality_Rate,states$Testing_Rate,states$Hospitalization_Rate
        ) %>% lapply(htmltools::HTML)
      }
    }
    
    else #policy is chosen
    {
      confirmed_number <- as.character(unlist(confirmed_with_order[select_date]))
      rates_with_order <- data.frame(State = states$NAME) %>% left_join(states_complete %>% filter(Date == select_date) %>% dplyr::select(State,Date,Incident_Rate,Mortality_Rate,Testing_Rate,Hospitalization_Rate))
      num_temp <- confirmed_number
      map_pal <- colorFactor(brewer.pal(name="YlOrRd",n=length(levels(states_complete[[input$stats_dropdown]]))), 
                             domain = states$STATE, levels=levels(states_complete[[input$stats_dropdown]])[order(levels(states_complete[[input$stats_dropdown]]))])
      
      states$STATE <- num_temp
      states$Incident_Rate <- rates_with_order$Incident_Rate
      states$Mortality_Rate <- rates_with_order$Mortality_Rate
      states$Testing_Rate <- rates_with_order$Testing_Rate
      states$Hospitalization_Rate <- rates_with_order$Hospitalization_Rate
      
      if (select_date < format.Date("2020-04-12",'%Y-%m-%d')){
        labels <- sprintf(
          "<strong>%s</strong><br/>%s: %s </br> Incident Rate: %g </br> Mortality Rate: %g",
          states$NAME,input$stats_dropdown,states$STATE,states$Incident_Rate,states$Mortality_Rate
        ) %>% lapply(htmltools::HTML)
      }
      else{
        labels <- sprintf(
          "<strong>%s</strong><br/>%s: %s </br> Incident Rate: %g </br> Mortality Rate: %g </br> Testing Rate: %g </br> Hospitalization Rate: %g",
          states$NAME,input$stats_dropdown,states$STATE,states$Incident_Rate,states$Mortality_Rate,states$Testing_Rate,states$Hospitalization_Rate
        ) %>% lapply(htmltools::HTML)
      }
    }

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
      leafletProxy("county_map", data = states)%>%addTiles()%>%clearShapes()
      return(input$county_name) 
    }
    else{
      leafletProxy("county_map", data = states)%>%addTiles()%>%clearShapes()
      return(NULL)
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
      setView(-96, 17.8, 4) %>%
      addTiles()
    
  })
  

  observe({
    
    county_select_date <- county_date()
    county_df_temp <- county_get_df()
    county_name_array <- find_county_name()
    
    if(!is.null(county_name_array)){
      rates_county <- county_complete %>% filter(Date == county_select_date) %>% dplyr::select(County,State,Incident_Rate,Mortality_Rate)
      rates_county_temp <- left_join(rates_county,data.frame(states),by=c('State'='NAME'))
      rates_county_with_order <- left_join(data.frame(counties),rates_county_temp,by=c('STATE'='STATE','NAME'='County'))
      
      counties$Incident_Rate<-rates_county_with_order$Incident_Rate
      counties$Mortality_Rate<-rates_county_with_order$Mortality_Rate
      
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
        
        cdata_selected_county <- counties[counties$STATE==county_number,c('NAME',"Confirmed","Incident_Rate","Mortality_Rate")]
        
        labels <- sprintf(
          "<strong>%s</strong><br/>%g %s </br> Incident Rate: %g </br> Mortality Rate: %g",
          unlist(map(cdata_selected_county$NAME,convert_xf1)), cdata_selected_county$Confirmed,input$county_stats_dropdown , cdata_selected_county$Incident_Rate, cdata_selected_county$Mortality_Rate
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
    }
    
    
  })

  #map end --------------------------------------------------------------------------------------------------------
  
  
  #report --------------------------------------------------------------------------------------------------------
  
  d_state <- reactive({
    req(input$state_dropdown) #don't display plot if nothing is selected
    
    filtered <- states_complete%>%
      filter(State%in%input$state_dropdown)%>%
      mutate(State_Policy=paste(State,get(input$policy_dropdown),sep=": "))%>%
      mutate(State_Policy=factor(State_Policy))
    
    policy_lengths<-rle(unlist(map(strsplit(levels(filtered$State_Policy),'[:]'),1)))$lengths
    policy_lengths<-append(policy_lengths,rep(3,3-length(policy_lengths)))
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
                   geom_point(size=1.25)+
                   theme_bw() +
                   xlab("Time") +
                   ylab("Incident Rate") +
                   scale_colour_manual(values=d_state()[[2]],drop=FALSE)+
                   theme(legend.title = element_blank())+
                   labs(color=as.character(input$policy_dropdown)),
                 tooltip='text')%>%
      add_annotations(
        text = "Incident Rate Over Time",
        x = 0.30,
        y = 1,
        yref = "paper",
        xref = "paper",
        xanchor = "left",
        yanchor = "top",
        yshift = 23,
        showarrow = FALSE,
        font = list(size = 15)
      )
    p2<-ggplotly(ggplot(d_state()[[1]],aes(x=Date, y=Mortality_Rate,color=str_wrap(factor((State_Policy)),20),label=State,
                                           text=paste('Date:',Date,
                                                      '<br>Mortality Rate:',format(round(Mortality_Rate,3)),
                                                      str_wrap(paste0('<br>',as.character(input$policy_dropdown),': ',factor(get(input$policy_dropdown))),60),
                                                      '<br>State:',State)))+
                   geom_point(size=1.25)+
                   theme_bw() +
                   xlab("Time") +
                   ylab("Mortality Rate") +
                   scale_colour_manual(str_wrap(as.character(input$policy_dropdown),8),values=d_state()[[2]],drop=FALSE)+
                   labs(color=as.character(input$policy_dropdown))+
                   theme(legend.title = element_blank()),
                 tooltip='text')%>%
      add_annotations(
        text = "Mortality Rate Over Time",
        x = 0.30,
        y = 1,
        yref = "paper",
        xref = "paper",
        xanchor = "left",
        yanchor = "top",
        yshift = 23,
        showarrow = FALSE,
        font = list(size = 15)
      )
    p3<-ggplotly(ggplot(d_state()[[1]],aes(x=Date, y=Testing_Rate,color=str_wrap(factor((State_Policy)),20),label=State,
                                           text=paste('Date:',Date,
                                                      '<br>Testing Rate:',format(round(Testing_Rate,3)),
                                                      str_wrap(paste0('<br>',as.character(input$policy_dropdown),': ',factor(get(input$policy_dropdown))),60),
                                                      '<br>State:',State)))+
                   geom_point(size=1.25)+
                   theme_bw() +
                   xlab("Time") +
                   ylab("Testing Rate") +
                   scale_colour_manual(values=d_state()[[2]],drop=FALSE)+
                   theme(legend.title = element_blank())+
                   labs(color=as.character(input$policy_dropdown)),
                 tooltip='text')%>%
      add_annotations(
        text = "Testing Rate Over Time",
        x = 0.30,
        y = 1,
        yref = "paper",
        xref = "paper",
        xanchor = "left",
        yanchor = "top",
        yshift = 23,
        showarrow = FALSE,
        font = list(size = 15)
      )
    p4<-ggplotly(ggplot(d_state()[[1]],aes(x=Date, y=Hospitalization_Rate,color=str_wrap(factor((State_Policy)),20),label=State,
                                           text=paste('Date:',Date,
                                                      '<br>Hospitalization Rate:',format(round(Hospitalization_Rate,3)),
                                                      str_wrap(paste0('<br>',as.character(input$policy_dropdown),': ',factor(get(input$policy_dropdown))),60),
                                                      '<br>State:',State)))+
                   geom_point(size=1.25)+
                   theme_bw() +
                   xlab("Time") +
                   ylab("Hospitalization Rate") +
                   scale_colour_manual(values=d_state()[[2]],drop=FALSE)+
                   theme(legend.title = element_blank())+
                   labs(color=as.character(input$policy_dropdown)),
                 tooltip='text')%>%
      add_annotations(
        text = "Hospitalization Rate Over Time",
        x = 0.30,
        y = 1,
        yref = "paper",
        xref = "paper",
        xanchor = "left",
        yanchor = "top",
        yshift = 23,
        showarrow = FALSE,
        font = list(size = 15)
      )
    
    subplot(style(p1,showlegend=F),style(p2,showlegend=T),style(p3,showlegend=F),style(p4,showlegend=F),nrows=2,shareX=F,shareY=F,titleX=T,titleY=T,margin=0.065)%>%
      layout(paper_bgcolor='transparent')
    
  })
  
  
  
  d_county <- reactive({
    req(input$county_dropdown) #don't display plot if nothing is selected
    
    filtered <- county_complete%>%
      filter(Combined_Key%in%input$county_dropdown)%>%
      mutate(County_Policy=paste(County,get(input$policy_dropdown_2),sep=": "))%>%
      mutate(County_Policy=factor(County_Policy))
    levels(filtered$County_Policy)<-levels(filtered$County_Policy)[order(levels(filtered$County_Policy))]
    
    policy_lengths<-rle(unlist(map(strsplit(levels(filtered$County_Policy),'[:]'),1)))$lengths
    policy_lengths<-append(policy_lengths,rep(3,3-length(policy_lengths)))
    custom_colors<-c(brewer.pal(name="Blues",n=policy_lengths[1]),brewer.pal(name="Greens",n=policy_lengths[2]),brewer.pal(name="Purples",n=policy_lengths[3]))
    
    return(list(filtered,custom_colors))
    
  }) 
  # Line Plot
  
  output$county_line_plot=renderPlotly({
    q1<-ggplotly(ggplot(d_county()[[1]],aes(x=Date, y=Incident_Rate,color=str_wrap(factor((County_Policy)),20),label=Combined_Key,
                                          text=paste('Date:',Date,
                                                     '<br>Incident Rate:',format(round(Incident_Rate,3)),
                                                     str_wrap(paste0('<br>',as.character(input$policy_dropdown_2),': ',factor(get(input$policy_dropdown_2))),60),
                                                     '<br>County:',Combined_Key))) +
                 geom_point(size=1.25)+
                 theme_bw() +
                 xlab("Time") +
                 ylab("Incident Rate") +
                 scale_colour_manual(values=d_county()[[2]],drop=FALSE)+
                 theme(legend.title = element_blank())+
                 labs(color=as.character(input$policy_dropdown_2)),
               tooltip='text')%>%
      add_annotations(
        text = "Incident Rate Over Time",
        x = 0.30,
        y = 1,
        yref = "paper",
        xref = "paper",
        xanchor = "left",
        yanchor = "top",
        yshift = 23,
        showarrow = FALSE,
        font = list(size = 15)
      )
    q2<-ggplotly(ggplot(d_county()[[1]],aes(x=Date, y=Mortality_Rate,color=str_wrap(factor((County_Policy)),20),label=Combined_Key,
                                          text=paste('Date:',Date,
                                                     '<br>Mortality Rate:',format(round(Mortality_Rate,3)),
                                                     str_wrap(paste0('<br>',as.character(input$policy_dropdown_2),': ',factor(get(input$policy_dropdown_2))),60),
                                                     '<br>County:',Combined_Key))) +
                 geom_point(size=1.25)+
                 theme_bw() +
                 xlab("Time") +
                 ylab("Mortality Rate") +
                 scale_colour_manual(values=d_county()[[2]],drop=FALSE)+
                 theme(legend.title = element_blank())+
                 labs(color=as.character(input$policy_dropdown_2)),
               tooltip='text')%>%
      add_annotations(
        text = "Mortality Rate Over Time",
        x = 0.30,
        y = 1,
        yref = "paper",
        xref = "paper",
        xanchor = "left",
        yanchor = "top",
        yshift = 23,
        showarrow = FALSE,
        font = list(size = 15)
      )
    
    subplot(style(q1,showlegend=F),style(q2,showlegend=T),nrows=1,shareX=F,shareY=F,titleX=T,titleY=T,margin=0.065)%>%
      layout(paper_bgcolor='transparent')
    
  })
  
  #report end --------------------------------------------------------------------------------------------------------
  
  
})