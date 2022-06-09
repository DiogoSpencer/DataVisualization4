library(shiny)
library(shinydashboard)
library(bubblyr)
library(ggsci)
library(plotly)

dashboardPage(
  skin="green",
  dashboardHeader(title = span(icon("spotify"),"Spotify Analysis")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview",tabName = "overview",icon=icon("globe")),
      menuItem("Genre Analysis", tabName = "genreAnalysis", icon=icon("music")),
      menuItem("Artist Analysis", tabName = "artistAnalysis", icon=icon("users")),
      menuItem("Data", tabName = "rawdata",icon=icon("database")),
      menuItem("About",tabName= "about",icon=icon("info"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("overview",
              h2("Overview"),
              p("Here you can check the most popular song and artist in the spotify Top 200 (2017-2021) database!"),
              fluidRow(#BOX2
                box(h4('Most popular Song'),width = 6,status = "success",
                    valueBoxOutput("streams",width = 12),
                    h5(strong("Song:"),textOutput("Song"),strong("Artist: "),textOutput("Art")),
                    selectInput("Year","Year:",
                                choices = list("2017" = 2017,
                                               "2018" = 2018,
                                               "2019" = 2019,
                                               "2020" = 2020,
                                               "2021" = 2021),
                                selected = 2017)
                ),
                box(h4('Most popular Artist'),width = 6,status = "success",
                    valueBoxOutput("numberSongs",width = 12),
                    h5(strong("Artist:"),textOutput("topArtist")),
                    selectInput("Year2","Year:",
                                choices = list("2017" = 2017,
                                               "2018" = 2018,
                                               "2019" = 2019,
                                               "2020" = 2020,
                                               "2021" = 2021),
                                selected = 2017)
                )
              ),
              fluidRow(
                
              )
        
      ),
      tabItem("artistAnalysis",
              h2("Artist Analysis"),
              fluidRow(
              #BOX1                
              box(title = "Artists Development through the years", status = "success", width = 12 , 
                  plotlyOutput("linechart"),
                  sliderInput("Evolution", "Years:",
                              min = 2017, max = 2021,
                              value = 2017, step = 1,
                              sep = "",
                              animate = TRUE
                              )
                )
              )
            ),
        tabItem("genreAnalysis",
                h2("Genre Analysis"),
              fluidRow(
              #BOX2
              box(h4('Genre Tendecies'),width = 6,status = "success",
                  plotOutput("barchart",click= "bar_click"),
                  selectInput("Month_Bar","Month:",
                              choices = list("January" = 1,
                                             "February" = 2,
                                             "March" = 3,
                                             "April" = 4,
                                             "May" = 5,
                                             "June"= 6,
                                             "July"= 7,
                                             "August"= 8,
                                             "September"= 9,
                                             "October"= 10,
                                             "November"= 11,
                                             "December"= 12
                                             ),
                              selected = 1),
                  selectInput("Year_Bar","Year:",
                              choices = list("2017" = 2017,
                                             "2018" = 2018,
                                             "2019" = 2019,
                                             "2020" = 2020
                                              ),
                              selected = 2017)
                  
                  
                ),
              #BOX3
              box(h4('List of Songs corresponding to the selected Bar'),width = 6,status = "success",
                  DT::dataTableOutput("table2")
              )
            )
        ),
      tabItem("rawdata",
               box(width = 14,status = "primary",
                   plotlyOutput("heatmap"),
                   h3('Streams statistics of the selected music track in the selected year')
                   
                   
                   
               ),
               fluidRow(DT::dataTableOutput("table1")),
               verbatimTextOutput("rawtable"),
               tags$a('Download csv.', href = "https://www.kaggle.com/datasets/ivannatarov/spotify-daily-top-200-songs-with-genres-20172021/download",class = "btn", icon("download"))
      ),
      tabItem("about",
              
              h3(' Spotify interative dashboard                               
              made by:
                 Afonso Boucho ER1206 and 
                 Diogo Ramos ER1202')
              
              )
    )
  )
)


