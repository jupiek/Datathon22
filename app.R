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
              p("When droughts occur, all aspects of communities are affected, from economic
                and environmental to social repercussions. We leveraged the economic, water usage,
                earnings, droughts, and chemical datasets to explore ways communities can mitigate 
                effects from drought. We also tried to identify key variables that would predict drought
                and suggest possible preventative meausres for counties."),
              br(),
              p("In this project, we attempt to answer some questions such 
                as:"),
              p("  - How can counties minimize the loss of income due to droughts?"),
              p("  - What variables are strong predictors of incoming drought?"),
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