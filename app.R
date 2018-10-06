library(tidyverse)
library(shinydashboard)
library(plotly)
library(shiny)
library(plotly)
library(rsconnect)

load("draft.RData")

ui = dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Data Open 2018 - Group 27"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("dashboard")),
      menuItem("Contaminant Levels by County", tabName = "chemical", icon = icon("th"))
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
              br(),
              p("Dataset from: ", 
                a("https://www.kaggle.com/edumucelli/spotifys-worldwide-daily-song-ranking")),
              br(),
              p("By: Joshua Huang, Julie Kim, Clay Yoo, Sophia Yoo")
              ),
      
      # First tab content
      tabItem(tabName = "chemical",
              h2("Contaminant Levels by County"),
              p("An overview of contamination levels by county."),
              p(""),
              
              fluidPage(
                inputPanel(
                  selectInput("variable", label = "County:",
                               choices = sort(unique(chemicals$county)))
                ),
                plotlyOutput("chem.by.county")
              )
              )
              )
      )
    )

server = function(input, output, session) {
  
  options(warn = -1)
  
  # Graph 1
  output$chem.by.county = renderPlotly({
    
    rplot = chem.by.county(input$variable)
    ggplotly(rplot)
    
  })
}

shinyApp(ui, server)