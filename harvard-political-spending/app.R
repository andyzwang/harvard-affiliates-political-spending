# import required libraries for shiny user interface

library(shiny)
library(shinythemes)
library(stringr)
library(janitor)
library(ggplot2)
library(usmap)
library(lubridate)
library(ggthemes)
library(broom)
library(tidyverse)
library(DT)

# import the main data set from separate file to minimize clutter.

source("data.R")

# define overall ui with navbar

ui <- navbarPage(

  # define theme
  theme = shinytheme("flatly"),
  "Contribution In and Out of the Classroom: Harvard Faculty Political Spending",

  # create first tab: introduction panel

  tabPanel(
    "Introduction",
    includeHTML("introduction.html")
  ),

  # second tap: top spenders

  tabPanel(
    "Top Spenders",
    titlePanel("Top Faculty Spenders"),
    p("Here's an aggregation of the sums of political spending by each individual faculty member from January 2017 until now."),
    p("The average spending across all faculty is", strong("$791.27"), "while the average spending for only those who spend is", strong("$2,896.57.")),
    DTOutput("individualspending")
  ),
  tabPanel(
    "Spending Recipients",
    tabsetPanel(
      tabPanel("Recipients Overview"),
      p("Here"),
      DTOutput("recipient_overview")
    )
  ),
  tabPanel(
    "Who Did My Professor Donate To?",
    titlePanel("About"),
    h3("Project Background and Motivations"),
    p("Hello, this is where I talk about my project."),
    h3("About Me"),
    p("My name is ______ and I study ______. 
             You can reach me at ______@college.harvard.edu.")
  ),
  tabPanel(
    "Modelling Spending Behavior",
    titlePanel("About"),
    h3("Project Background and Motivations"),
    p("Hello, this is where I talk about my project."),
    h3("About Me"),
    p("My name is ______ and I study ______. 
             You can reach me at ______@college.harvard.edu.")
  ),
  tabPanel(
    "Diversity in the Faculty",
    titlePanel("Diversity in the Faculty"),
    h3("Project Background and Motivations"),
    p("Hello, this is where I talk about my project."),
    h3("About Me"),
    p("My name is ______ and I study ______. 
             You can reach me at ______@college.harvard.edu.")
  ),
  tabPanel(
    "About",
    includeHTML("about.html")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # importing data source for top_spenders

  source("top_spenders.R")

  # rendering a data table based off of spending_by_individual, formatting to
  # have spending displayed as a dollar.

  output$individualspending <- renderDataTable({
    datatable(spending_by_individual,
      options = list(
        colnames = c(
          "Name",
          "Title",
          "School",
          "Department",
          "Total Spending"
        ),
        pageLength = 20
      )
    ) %>%
      formatCurrency("spending_sum", "$")
  })

  # including recipients data source

  source("recipients.R")

  # rendering a data table based off of spending_recipients, formatting to
  # have spending displayed as a dollar.

  output$recipient_overview <- renderDataTable({
    datatable(spending_recipients_ui,
      options = list(
        colnames = c(
          "Committee Name",
          "Spending Sum",
          "Address",
          "City",
          "State",
          "Party Affiliation"
        ),
        pageLength = 20
      )
    ) %>%
      formatCurrency("spending_sum", "$")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
