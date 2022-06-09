library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(ggpubr)
library(lubridate)
library(reshape2)
library(viridis)
library(scales)
library(plotly)
library(data.table)

spotify_data <- read.csv("C:/Users/Diogo/Desktop/Data Visualization/Proj4/Assignment4/data.csv", sep="#")
#Aux Fucntions

yearSth <- function(yearOfSth){
  spotify_data_cpy <- spotify_data
  spotify_data_cpy$Date <- substring(spotify_data_cpy$Date,1,4)
  aux <- spotify_data_cpy[strtoi(spotify_data_cpy$Date) == yearOfSth,]
  final <- aux %>%
    group_by(Track.Name,Artist) %>%
    summarize(Streams = sum(Streams, na.rm = TRUE))
  out <- final[ which.max(final$Streams),]
  return(out)
}

yearCount <- function(yc){
  spotify_data_cpy <- spotify_data
  spotify_data_cpy$Date <- substring(spotify_data_cpy$Date,1,4)
  ax <- spotify_data_cpy[strtoi(spotify_data_cpy$Date) == yc,]
  r <- ax%>%
    group_by(Artist) %>%
    tally()
  mx <- r[ which.max(r$n),]
  return(mx)
}

artistsEv <- function(dateEv){
  spotify_data_cpy <- spotify_data
  spotify_data_cpy$Date <- substring(spotify_data_cpy$Date,1,7)
  a <- spotify_data_cpy[strtoi(substring(spotify_data_cpy$Date,1,4)) == dateEv,]
  
  artists <- a %>%
    group_by(Artist,Date) %>%
    summarize(Streams = sum(Streams, na.rm = TRUE))
  
  artists <- artists[artists$Artist %in% c('The Weeknd', 'Taylor Swift', 'Drake','Harry Styles','Ed Sheeran'),]
  artists$Date <- substring(artists$Date,6,7)
  artists$Date <- month.abb[as.integer(artists$Date)]
  return(artists)
  
}

genresAux <- function(genre,ym){
  spotify_data_cpy <- spotify_data
  spotify_data_cpy$Date <- strtoi(paste(substring(spotify_data_cpy$Date,1,4),substring(spotify_data_cpy$Date,6,7),sep=""))
  res <- spotify_data_cpy[(spotify_data_cpy$Genre %like% genre) & spotify_data_cpy$Date== ym,]
  return(res)
}

genreEv <- function(ym){
  
  pop_date <- genresAux("pop",ym)
  
  pop <- pop_date%>%
    group_by(Date) %>%
    tally()
  
  pop <- cbind(pop, Genre = "pop")
  
  rock_date <- genresAux("rock",ym)
  
  rock <- rock_date%>%
    group_by(Date) %>%
    tally()
  
  rock <- cbind(rock, Genre = "rock")
  
  rap_date <- genresAux("rap",ym)
  
  rap <- rap_date%>%
    group_by(Date) %>%
    tally()
  
  rap <- cbind(rap, Genre = "rap")
  
  hiphop_date <- genresAux("hip hop",ym)
  
  hiphop <- hiphop_date%>%
    group_by(Date) %>%
    tally()
  
  hiphop <- cbind(hiphop, Genre = "hip hop")
  
  reggaeton_date <- genresAux("reggaeton",ym)
  
  reggaeton <- reggaeton_date%>%
    group_by(Date) %>%
    tally()
  
  reggaeton<- cbind(reggaeton, Genre = "reggaeton")
  
  final <- bind_rows(pop,rap,rock,hiphop,reggaeton)
  final_ordered <- final[order(-final$n),]
  
  return(final_ordered)
}

clickData <- function(dt){
  x <- select(dt,Genre)
  x_T <- as.data.frame(t(x))
  return(x_T)
}

#SERVER

shinyServer(function(input, output) {
  
  #Function 1 
  count <- reactive({ 
    year = input$Year
    result <- yearSth(year)
    return(result)
  })
  
  #Function2
  chart <- reactive({
    ev = input$Evolution
    res <- artistsEv(ev)
    return(res)
  }) 
  
  #Function3
  bars <- reactive({
    m = input$Month_Bar
    y = input$Year_Bar
    if(strtoi(m) >=10){
      ym <- strtoi(paste(y,m,sep=""))
    }
    if(strtoi(m)<10){
      ym <- strtoi(paste(y,m,sep="0"))
    }
    r <- genreEv(ym)
    return(r)
  })
  
  #Function4
  showData <- reactive({
    m = input$Month_Bar
    y = input$Year_Bar
    if(strtoi(m) >=10){
      ym <- strtoi(paste(y,m,sep=""))
    }
    if(strtoi(m)<10){
      ym <- strtoi(paste(y,m,sep="0"))
    }
    aux <- genresAux(global$selectedBar,ym)
    show <- select(aux,c(Artist,Track.Name,Streams))
    return(show)
  })
  
  #Function5
  count2 <- reactive({ 
    year = input$Year2
    result <- yearCount(year)
    return(result)
  })
  
  
  #Counter Songs
  output$streams <- renderValueBox({valueBox(count()$Streams, "Number of Streams",color = "purple",icon = icon("headphones"))})
  
  output$Song <- renderText({
    count()$Track.Name
  })
  
  output$Art <- renderText({
    count()$Artist
  })
  
  #Counter times in top200 
  
  output$numberSongs <- renderValueBox({valueBox(count2()$n, "Number of Different Songs on the Top200",color = "yellow",icon = icon("chart-line"))})
  
  output$topArtist <- renderText({
    count2()$Artist
  })
  
  #Data
  output$table1 = DT::renderDataTable(spotify_data, server = TRUE, selection = 'single')
  
  
  #Data table row interaction
  selected <-0
  observeEvent(input$table1_rows_selected,{
    selectedTrack<-spotify_data[input$table1_rows_selected,2]
    
    spotify_data = spotify_data %>% 
      mutate(Date = ymd(Date)) %>% 
      mutate_at(vars(Date), funs(year, month, day))
    
    
    selectedYear <-spotify_data[input$table1_rows_selected,7]
    
    #Getting all the rows of the selected track
    spotify_data_cpy <- spotify_data
    spotify_musics <- spotify_data_cpy %>% filter(Track.Name == selectedTrack & grepl(selectedYear,Date)) 
    
    
    #View(spotify_musics)
    
    
    #output$heatmap <- renderPlotly({ggplot(spotify_musics, aes(month, day, fill= Streams)) + ggtitle(selectedTrack)+ scale_fill_gradient(high = "green", low = "white") + scale_x_continuous(breaks = seq(from = 1, to = 13, by = 1)) + scale_y_continuous(breaks = seq(from = 1, to = 31, by = 1))
    
    # })
    
    
    #output$heatmap <-  renderPlotly({ggplotly(ggplot(data=spotify_musics, aes(x=month, y=Streams, group=1)) +
    #                                                  geom_line(color="red")+
    #                                                 geom_point()+
    #                                                 scale_x_discrete(limits = month.abb)+
    #                                                scale_y_continuous(labels = comma_format(big.mark = ".",
    #                                                                                  decimal.mark = ","))+
    #                                                scale_color_brewer(palette = "Set1")+
    #                                               xlab("Month") + ylab("Streams")+
    #                                          theme_minimal())
    
    
    
    output$heatmap <- renderPlotly({ggplot(spotify_musics, aes(x = day, y = month, color = Streams)) +
        geom_point()+
        ggtitle(selectedTrack)+
        scale_y_discrete(limits = month.abb)+
        scale_x_continuous(name = "", breaks = seq(1, 31, 1), limits = c(1, 31))+
        # scale_color_continuous(
        #                      limits = c(0, 2000000),
        #                      breaks = c(0, 100000, 300000, 500000, 800000, 2000000))+
        xlab("Days") + ylab("Months")+
        theme_minimal()
      
      
      
    })
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    
  #First Visualization (evolution artists)
  
  output$linechart  <- renderPlotly({ggplotly(ggplot(chart(), aes(x=Date, y=Streams , colour=Artist , group = Artist)) +
      geom_point()+
      geom_line() +
      scale_x_discrete(limits = month.abb)+
      scale_y_continuous(labels = comma_format(big.mark = ".",
                                               decimal.mark = ","))+
      scale_color_brewer(palette = "Set1")+
      xlab("Month") + ylab("Streams")+
      labs(color = "Artists")+
      theme_minimal()
    )})
  
  #Second Visualization (evolution genres) 
  #FIX: 2021 June forward eliminate on the choices no data available
  
  output$barchart  <- renderPlot({ggplot(bars(), aes(x=reorder(Genre,desc(n)),y=n,fill=Genre)) +
                                               geom_bar(stat="identity")+
                                               scale_fill_brewer(palette = "Dark2")+
                                               ylab("Number of songs in the top 200") +
                                               xlab("Genre")+
                                               theme_minimal()
  })
  
  
  #Third Visualization (bar_chart table)
  
  global <- reactiveValues(selectedBar = "pop")
  
  observeEvent(eventExpr = input$bar_click, {
    View(bars())
    global$selectedBar <- clickData(bars())[1,round(input$bar_click$x)]
  })
  
  output$table2 <- DT::renderDataTable(showData(), server = TRUE)
  
  #Uni Logo
  
  #output$uni <- renderImage({
   #     src = "images/PP_Logo.png"
  #  }, deleteFile = FALSE)
  
})
