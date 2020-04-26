#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("flatly"),
  "Contribution In and Out of the Classroom: Harvard Faculty Political Spending",
    tabPanel(
      "Introduction",
      fluidPage(
        titlePanel("Model Title"),
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "plot_type",
              "Plot Type",
              c("Option A" = "a", "Option B" = "b")
            )
          ),
          mainPanel(plotOutput("line_plot"))
        )
      )
    ),
    tabPanel(
      "Faculty Spenders",
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
      titlePanel("About"),
      h3("Project Background and Motivations"),
      p("Hello, this is where I talk about my project."),
      h3("About Me"),
      p("My name is ______ and I study ______. 
             You can reach me at ______@college.harvard.edu.")
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "darkgray", border = "white")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
