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
    titlePanel("Introduction"),
  ),
  tabPanel(
    "Faculty Spending",
    titlePanel("Discussion Title"),
    p("Tour of the modeling choices you made and 
              an explanation of why you made them")
  ),
  tabPanel(
    "Spending Recipients",
    titlePanel("About"),
    h3("Project Background and Motivations"),
    p("Hello, this is where I talk about my project."),
    h3("About Me"),
    p("My name is ______ and I study ______. 
             You can reach me at ______@college.harvard.edu.")
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
    "Modelling Donation Behavior",
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
server <- function(input, output) {
  output$line_plot <- renderPlot({
    # Generate type based on input$plot_type from ui
    
    ifelse(
      input$plot_type == "a",
      
      # If input$plot_type is "a", plot histogram of "waiting" column 
      # from the faithful dataframe
      
      x   <- faithful[, 2],
      
      # If input$plot_type is "b", plot histogram of "eruptions" column
      # from the faithful dataframe
      
      x   <- faithful[, 1]
    )
    
    # Draw the histogram with the specified number of bins
    
    hist(x, col = 'darkgray', border = 'white')
  })
}

# Run the application
shinyApp(ui = ui, server = server)
