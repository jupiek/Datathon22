
library(tidyverse)
library(shinydashboard)
library(plotly)
library(shiny)
library(plotly)
library(ggplot2)
library(rsconnect)


load("draft.RData")
ui = dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Data Open 2018 - Group 27"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("dashboard")),
      menuItem("Process", tabName = "process", icon = icon("comment-alt")),
      menuItem("Contaminant Levels by County", tabName = "chemical", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "introduction",
              h2("Introduction"),
              br(),
              p("When droughts occur, all aspects of communities are affected, from economic
                and environmental to social. We leveraged the economic, water usage,
                earnings, droughts, and chemical datasets to explore ways communities can mitigate 
                effects from drought. We also tried to identify key variables that would predict drought
                and suggest possible preventative meausres for counties."),
              br(),
              p("In this project, we attempt to answer some questions such 
                as:"),
              p("  - How can counties minimize the loss of income due to droughts?"),
              p("  - What variables are strong predictors of incoming drought?"),
              p("  - Specifically, which vairables exhibit significant change before and after a period of drought?"),
              br(),
              p("By: Joshua Huang, Julie Kim, Clay Yoo, Sophia Yoo")
              ),
      
      tabItem(tabName = "process",
              h2("Process"),
              br(),
              p("We identified the annual top 3 states, based on number of days of drought
                 per year. The most notable observation was that Texas had the highest number of 
                 days of drought from 2011 to 2015, but did not even place in the top 3 in 2016."),
              p("Following this insight, we decided to investigate Texas further in depth to consider
                 what changed from 2015 to 2016 to produce these results."),
              p("We found that ... ")
      ),
      
      tabItem(tabName = "chemical",
              h2("Contaminant Levels by County"),
              p("An overview of contamination levels by county."),
              p(""),
              
              fluidPage(
                inputPanel(
                  selectInput("county1", label = "County:",
                               choices = sort(unique(chemicals$county)))
                ),
                plotlyOutput("chem.by.county")
              )
              ),
      
      tabItem(tabName = "conclusion",
              h2("Conclusion"),
              br(),
              p("We found that..."),
              br(),
              p("We would recommend further investigation of ... as we've identified an association, but it does not clearly imply causation."),
              br(),
              p("By: Joshua Huang, Julie Kim, Clay Yoo, Sophia Yoo")
              )
              )
      )
    )

server = function(input, output, session) {
  
  options(warn = -1)
  
  # Graph 1
  output$chem.by.county = renderPlotly({

      cty.dat = chemicals[which(chemicals$county == input$county1),]
      plot = ggplot(cty.dat, aes(x = year, y = value, group = 1)) + 
        geom_line() +
        facet_grid(~ chemical_species)
      ggplotly(plot)
    
  })
}

shinyApp(ui, server)