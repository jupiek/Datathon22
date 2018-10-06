## app.R ##

library(tidyverse)
library(plyr)
library(shiny)
library(rsconnect)
library(ggmap)
library(maps)
library(sp)
library(plotly)
library(scales)
library(gridExtra)
library(shinydashboard)

#save("draft.RData")
load("draft.RData")


# SHINY APP CODE
# ===


ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Spotify Music Streaming"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("dashboard")),
      menuItem("Overview", tabName = "overview", icon = icon("th")),
      menuItem("Top Songs", icon = icon("th"),
               menuSubItem("Based on Streams", tabName = "top_song_stream"),
               menuSubItem("Based on Days in Top 5", tabName = "top_song_day")),
      menuItem("Country vs Region", tabName = "country_v_region", icon = icon("th")),
      menuItem("Streaming Over Time", icon = icon("th"),
               menuSubItem("By Region", tabName = "time_by_region"),
               menuSubItem("Compare Two Countries", tabName = "time_by_country"),
               menuSubItem("Artist on Top 100 Daily List", tabName = "time_200_list")),
      menuItem("Artist Song Proportions", tabName = "artist_prop_song", 
               icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "introduction",
              h2("Introduction"),
              br(),
              p("Music streaming has become the new way of listening to music 
                and Spotify has become one of the most successful platforms 
                for such a service. With so many people on a single platform 
                comes the possibility to analyze listening behaviors to study
                the spread of music."),
              br(),
              p("In this project, we attempt to answer some questions such 
                as:"),
              p("  - How do artists do in a particular country as compared
                to that continent?"),
              p("  - How does the stream amount of a particular artist's song 
                changed over time between two countries?"),
              p("  - In a specific region, how many days have the top songs
                stayed at the top?"),
              p("  - In a specific region, what songs comprise of the artist's
                total stream and what proportion does each song take?"),
              p("  - In a specific region, what are the top songs by stream
                count?"),
              p("  - How has the total amount of streams changed over time in a 
                particular region."),
              p("  - How has an artist's top songs changed in their position
                throughout the year."),
              br(),
              p("Dataset from: ", 
                a("https://www.kaggle.com/edumucelli/spotifys-worldwide-daily-song-ranking")),
              br(),
              p("By: Julie Kim, Austin Yu, Joshua Huang, Bryan Yan")
              ),
      
      # First tab content
      tabItem(tabName = "overview",
              h2("Overview"),
              p("A map overview of the countries that we have data for and
                the amount of streams, tracks, and artists for those 
                countries"),
              p("Countries that have larger numbers of
                artists and tracks have more variety in their top Spotify 
                charts, while countries with smaller numbers tend listen to
                similar songs and artists"),
              
              fluidPage(
                inputPanel(
                  radioButtons("variable", label = "Fill Variable:",
                               choiceNames = c("Number of Streams", 
                                               "Number of Tracks", 
                                               "Number of Artists"),
                               choiceValues = c("Total_Streams", "N_Tracks", 
                                                "N_Artists"))
                ),
                plotlyOutput("world_map")
              )
              ),
      
      # Second tab content
      tabItem(tabName = "country_v_region",
              h2("Top 15 Artists: Country vs. Region"),
              p("A comparison of top 15 artists in selected country compared
                with their respective region."),
              p("The purpose of this graphic is to investigate how much
                countries tend to differentiate from their regions in music
                ratings"),
              p("Top 15 artists are calculated by number of appearances on
                the rankings database"),
              fluidPage(
                inputPanel(
                  selectInput("country", label = "Country:",
                              choices = sort(names(countries)[countries != "global"]), 
                              selected = "Asia")
                ),
                plotOutput("cplot")
              )
              ),
      
      # Third tab content
      tabItem(tabName = "time_by_country",
              h2("Song Streams Over Time by Country"),
              p("Now that you've seen how song streaming changes by region, we
                also wanted to see how song streams change by country and for
                specific songs. This gives us some insight as to what
                songs different countries are listening to and how this compares
                to other countries."),
              br(),
              p("Selecting an artist will change which songs you can
                select from. Depending on the song/artist selection along with the
                countries selected, you may only get results for 1 country 
                (if no results for the other country were found) or no results at 
                all for both countries."),
              fluidPage(
                inputPanel(
                  selectInput("country1", label = "Country",
                              choices = sort(unique(data$Country)),
                              selected = "USA"),
                  
                  selectInput("country2", label = "Country 2",
                              choices = sort(unique(data$Country)),
                              selected = "Canada"),
                  
                  # textInput("songTime", label = "Song Name",
                  #           value = "rockstar"),
                  # selectInput("artistTime", label = "Artist Name",
                  #             choices = unique(artists),
                  #             selected = "Post Malone"),
                  htmlOutput("artistTime"),
                  htmlOutput("songTime"),
                  br()
                ),
                plotlyOutput("streams_over_time")
              )
              ),
      
      # Fourth tab content
      tabItem(tabName = "top_song_day",
              h2("Top Songs by Number of Days in Top 5"),
              p("Here we see the top songs of a region in terms of how many 
                days they remained in the top 5."),
              br(),
              p("You can pick the region that you are interested in and adjust
                the top n songs that you would like displayed."),
              fluidPage(
                inputPanel(
                  selectInput("region", label = "Region:",
                              choices = c("North America", "South America", "Central America",
                                          "Asia", "Europe", "Oceania"), selected = "North America"),
                  
                  sliderInput("top_n", label = "Top n songs by days on top 5",
                              min = 5, max = 50, value = 10, step = 5)
                ),
                plotOutput("top_songs_plot")
              )
              ),
      
      # Fifth tab content
      tabItem(tabName = "artist_prop_song",
              h2("Artist Song Proportions"),
              p("On this page we can view what songs comprise of the artist's
                total stream and what proportion does each song take"),
              br(),
              p("When changing the region, the dropdown options for artists
                will automatically change to only include those that are in that
                subset of the data."),
              fluidPage(
                inputPanel(
                  selectInput("region5", label = "Region:",
                              choices = c("North America" = "na", "South America" = "sa",
                                          "Central America" = "ca", "Asia" = "asia",
                                          "Europe" = "europe", "Oceania" = "oceania"),
                              selected = "na"),
                  
                  selectInput("artist5", label = "Artist:",
                              choices = unique(top.songs.na.grouped$Artist),
                              selected = "Ed Sheeran")
                  
                ),
                plotlyOutput("top_artist_prop_plot")
              )
              ),
      
      # Sixth tab content
      tabItem(tabName = "top_song_stream",
              h2("Top Songs by Country"),
              p("On this page we can see the most popular songs for each region. 
                You can choose the region of the world, as the nubmer of songs you want to see.
                "),
              fluidPage(
                inputPanel(
                  selectInput("region6", label = "Region",
                              choices = c("North America", "South America",
                                          "Central America",
                                          "Asia", "Europe", "Oceania"),
                              selected = "North America"),
                  sliderInput("top_n2", label = "Top n songs for each country",
                              min = 5, max = 50, value = 10, step = 5)),
                plotOutput("top_songs")
              )
              ),
      
      # Seventh tab content
      tabItem(tabName = "time_by_region",
              h2("Streaming Time by Region"),
              p("On this page we can see overall streaming nubmers for each region. 
                You can choose the region of the world, as well as the size of the line."),
              fluidPage(
                inputPanel(
                  selectInput("region7", label = "Region",
                              choices = c("North America", "South America",
                                          "Central America",
                                          "Asia", "Europe", "Oceania"),
                              selected = "North America"),
                  sliderInput("size", label = "Line Size",
                              min = 1, max = 5, value = 2, step = 0.1)),
                plotOutput("songs_time")
              )
              ),
      
      # Eighth tab content
      tabItem(tabName = "time_200_list",
              h2("Artist Performance Over Time"),
              p("Now that we've seen how overall streaming numbers were, let's focus
                on the artists. How long did specific songs stay on the Top 100
                list? We'll look at how artists have performed on this list and
                maybe see if we can find reasons for spikes during specific
                times on the charts. If the artist was never on the Top 100 list,
                then you'll receive no visual results."),
              fluidPage(
                inputPanel(
                  selectInput("country8", label = "Country",
                              choices = sort(unique(data$Country)),
                              selected = "USA"),
                  
                  selectInput("artist8", label = "Artist Name",
                              choices = sort(unique(data$Artist)),
                              selected = "Ed Sheeran")
                ),
                plotlyOutput("artistSpec")
              )
              )
      
      
      )
              )
  )

server <- function(input, output, session) {
  
  options(warn = -1)
  
  # Graph 1
  output$world_map <- renderPlotly({
    
    rplot = ggplot() + 
      geom_polygon(data = coords, aes(x = long, y = lat, group = group), 
                   fill = "white") +
      geom_polygon(data = country.data, aes_string(x = "long", y = "lat", 
                                                   group = "group",
                                                   fill = input$variable)) + 
      scale_fill_distiller(palette = "Spectral") +
      labs(title = "Top 30 Ranking Spotify Tracks Worldwide",
           x = "", 
           y = "") +
      theme(panel.background = element_rect(fill = "darkgrey"),
            panel.grid.major = element_line(color = "darkgrey"),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank())
    
    if(input$variable == "Total_Streams") 
      rplot = rplot + labs(fill = "Number of Streams")
    if(input$variable == "N_Tracks") 
      rplot = rplot + labs(fill = "Number of Tracks")
    if(input$variable == "N_Artists") 
      rplot = rplot + labs(fill = "Number of Artists")
    
    ggplotly(rplot)
    
  })
  
  # Graph 2
  output$cplot <- renderPlot({
    country.to.continent = function(country) {
      country = as.character(country)
      if (country %in% asia) return("Asia")
      if (country %in% europe) return("Europe")
      if (country %in% northamerica) return("North America")
      if (country %in% centralamerica) return("Central America")
      if (country %in% southamerica) return("South America")
      if (country %in% oceania) return("Oceania")
      return(NA)
    }
    
    top15 = function(area, type) {
      if(type == "country") 
        cy = artists.by.country[which(artists.by.country$Country == area),]
      if(type == "cont") 
        cy = artists.by.cont[which(artists.by.cont$Region == area),]
      flipped.cy = data.frame(t(cy[-1]))
      colnames(flipped.cy) = cy[, 1]
      
      artists = rev(rownames(flipped.cy)[order(flipped.cy[,1], decreasing = TRUE)][1:15])
      appearances = rev(flipped.cy[order(flipped.cy[,1], decreasing = TRUE),][1:15])
      
      return(data.frame(artists, appearances))
    }
    
    cy = top15(input$country, "country")
    cy$artists = factor(cy$artists, levels = cy$artists)
    
    rplot1 = ggplot(cy, aes(x = artists, y = appearances)) +
      geom_bar(stat = "identity", fill = "green4") +
      coord_flip() +
      labs(title = input$country,
           x = "Artist", y = "Number of Appearances") + 
      scale_y_continuous(expand = c(0, 0)) + 
      theme(axis.ticks.y = element_blank())
    
    cont = country.to.continent(input$country)
    ct = top15(cont, "cont")
    ct$artists = factor(ct$artists, levels = ct$artists)
    
    rplot2 = ggplot(ct, aes(x = artists, y = appearances)) +
      geom_bar(stat = "identity", fill = "green4") +
      coord_flip() +
      labs(title = cont,
           x = "Artist", y = "Number of Appearances") +
      scale_y_continuous(expand = c(0, 0)) + 
      theme(axis.ticks.y = element_blank())
    
    p <- grid.arrange(grobs = list(rplot1, rplot2), nrow = 1,
                      top = "Top 15 Artists Spotify by Country\ncompared with region")
    print(p)
  })
  
  # Graph 3
  
  output$artistTime <- renderUI({
    selectInput(
      inputId = "artistTime", 
      label = "Artist: ",
      choices = sort(unique(data$Artist)),
      selected = "Post Malone")
  })
  
  
  output$songTime <- renderUI({
    available <- unique(data[data$Artist == input$artistTime, "Track.Name"])
    selectInput(
      inputId = "songTime", 
      label = "Song: ",
      choices = available,
      selected = available[1])
  })
  
  dataSub <- reactive({
    if (is.null(input$songTime)) {
      subset(data, Track.Name == 'rockstar' & 
               Artist == input$artistTime &
               (Country == input$country1 | Country == input$country2))
    }
    else {
      chosenSong <- input$songTime
      subset(data, Track.Name == chosenSong &
               Artist == input$artistTime &
               (Country == input$country1 | Country == input$country2))
    }
  })
  
  output$streams_over_time <- renderPlotly({
    subsetOfData <- dataSub()
    if (nrow(subsetOfData) == 0) {
      p <- ggplot() +
        annotate("text",
                 x = 4, y = 25, size = 8, label = "No Results for 
                 Input Combination") +
        theme_void() +
        labs(x = NULL, y = NULL)
    }
    else {
      p <- ggplot(subsetOfData, aes(x = as.Date(Date), y = Streams,
                                    color = Country, group = Country)) +
        scale_x_date(date_labels = "%b %d %y") +
        scale_y_continuous(labels = comma) +
        geom_point() +
        geom_line() +
        labs(title = "Song Streams by Country Over Time",
             x = "Date",
             y = "Number of Streams") +
        theme(axis.title.y =
                element_text(margin = margin(r = 50)),
              axis.title.x =
                element_text(margin = margin(t = 20)))
    }
    ggplotly(p)
  })
  
  # Graph 4
  output$top_songs_plot <- renderPlot({
    p <- ggplot(eval(parse(text = paste("cum.table$", "'", input$region, "'",
                                        sep = "")))[1:input$top_n,], aes(x = Track.Name, y = freq)) +
      geom_bar(stat = "identity", fill = "green4") +
      labs(title = "Amount of Days Song has been in Top 5 Rankings in 2017",
           x = "Song Name", y = "Number of Days") + 
      coord_flip() +
      scale_y_continuous(expand = c(0, 0)) + 
      theme(axis.ticks.y = element_blank())
    
    p
  })
  
  # Graph 5
  observe({
    x <- input$region5
    updateSelectInput(session, "artist5",
                      choices = unique(eval(parse(text = paste("top.songs.",
                                                               input$region5, ".grouped$Artist", sep = "")))),
                      selected = "Ed Sheeran")
  })
  
  output$top_artist_prop_plot <- renderPlotly({
    p <- ggplot(eval(parse(text = paste("top.songs.", input$region5, ".grouped",
                                        sep = "")))[eval(parse(text = paste("top.songs.",
                                                                            input$region5, ".grouped$Artist", sep = ""))) == input$artist5,],
                aes(x = Track, y = Streams)) +
      geom_bar(aes(x = factor(1), fill = Track), stat = "identity",
               width = .5) + scale_y_continuous(labels = comma) +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank())
    ggplotly(p)
  })
  
  
  # Graph 6
  dataSub6 <- reactive({
    data.c = data[which(data$Region == input$region6),]
    data.c = data.c = aggregate(data.c$Streams, by=list(Name=data.c$Track.Name), FUN=sum)
    head(arrange(data.c,desc(data.c$x)), input$top_n2)
  })
  output$top_songs <- renderPlot({
    ggplot(dataSub6(), aes(x = Name, y = x)) +
      geom_bar(stat = 'identity', fill = "green4") +
      scale_y_continuous(labels = comma) +
      #scale_x_discrete(labels = function(Name) str_wrap(Name, width = 10)) + 
      labs(title = "Top Songs by Country",
           x = "Song",
           y = "Number of Streams")  + coord_flip()+
      theme(axis.ticks.y = 
              element_blank(),
            axis.title.y = 
              element_text(margin = margin(r = 20)),
            axis.title.x =
              element_text(margin = margin(t = 20))) +
      scale_y_continuous(expand = c(0, 0)) 
    
  })
  
  # Graph 7
  dataSub7 <- reactive({
    data.d = data[which(data$Region == input$region7),]
    aggregate(data.d$Streams, by=list(Category=data.d$Date), FUN=sum)
  })
  output$songs_time <- renderPlot({
    ggplot(dataSub7(), aes(x = as.Date(Category), y = x)) + geom_point(colour= "green4") +
      geom_line(colour= "green4", size = input$size) +
      scale_y_continuous(labels = comma) +
      labs(title = "Song Streams by Country Over Time",
           x = "Date",
           y = "Number of Streams") + 
      theme(axis.title.y = 
              element_text(margin = margin(r = 20)),
            axis.title.x =
              element_text(margin = margin(t = 20)))
  })
  
  # Graph 8
  
  artistData8 <- reactive({
    artist_daily <- data %>%
      filter(Country == input$country8, Artist == input$artist8, Position <= 100)
    artist_20 <- artist_daily %>%
      group_by(`Track.Name`) %>%
      summarise(n_daily = n()) %>%
      filter(n_daily >= 20) %>%
      select(`Track.Name`)
    artist_20 <- artist_20 %>% collect %>% .[["Track.Name"]]
    artist_daily %>% filter(`Track.Name` %in% artist_20)
  })
  
  output$artistSpec <- renderPlotly({
    subData <- artistData8()
    if (nrow(subData) == 0) {
      p <- ggplot() +
        annotate("text",
                 x = 4, y = 25, size = 8, label = "No Results") +
        theme_void() +
        labs(x = NULL, y = NULL)
    }
    else {
      subSubData <- subData[seq(1, nrow(subData), 2), ]
      p <- ggplot(subSubData, aes(x = as.Date(Date), y = Position, col = `Track.Name`)) +
        geom_point(alpha = 0.7, size = 1.5) +
        geom_line() +
        scale_y_reverse(breaks = seq(0, 100, 10)) +
        scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
        labs(title = "Artist on Top 100 Daily List in Country",
             x = "Date",
             col = "Track Name")
    }
    ggplotly(p)
  })
}

shinyApp(ui, server)




#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

