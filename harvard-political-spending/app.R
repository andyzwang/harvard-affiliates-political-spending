# import required libraries for shiny user interface

library(shiny)
library(shinythemes)
library(tidyverse)

# import the main data set 

# define overall ui with navbar

ui <- navbarPage(

  # define theme
  theme = shinytheme("flatly"),
  "Contribution In and Out of the Classroom: Harvard Faculty Political Spending",

  # create first tab: introduction panel

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
    column(
      6,
      h1("About Me"),
      p("My name is Andy Wang, and I'm currently a first-year at Harvard College. I plan on concentrating in Social Studies with a secondary in Computer Science. I'm interested in exploring the ways that people interact with the surroundings around them, in terms of institutions, cultures, people, and spaces."),
      p("You can find me at", tags$a(href = "mailto:azwang@college.harvard.edu", "azwang@college.harvard.edu,"), "check out the original code on", tags$a(href = "http://github.com/andyzwang", "my Github account,"), "or connect with me on", tags$a(href = "https://www.linkedin.com/in/theandywang", "LinkedIn."),
        p("Cheers,"),
        p("Andy"))
    ),
    column(
      6,
      h1("Data Sources"),
      h4(a(
        href = "https://www.fec.gov/data/browse-data/?tab=bulk-data",
        "Federal Elections Commission Bulk Data"
      )),
      p("I used the 2019-2020 committees bulk data from the FEC in order to match the receipt ID's of Harvard faculty political spending with committees and their names / addresses."),
      h4(a(
        href = "https://www.fec.gov/introduction-campaign-finance/how-to-research-public-records/individual-contributions/",
        "Federal Elections Commission Individual Contributions Data"
      )),
      p("I used FEC's online Individual Contributer Data tool to conduct a mass search of all individuals who report their employer as \"Harvard,\" filtering for data from 1 January 2017 - 31 December 2020, and recieving contribution information."),
      h4(a(
        href = "http://facultyfinder.harvard.edu/search",
        "Harvard Faculty Finder"
      )),
      p("I used Harvard's Faculty Finder website to find names, titles, and department affiliations of Harvard faculty members."),
      h4(
        "Individual Harvard Departmental Websites"
      ),
      p("I used individual departmental websites (too many to list here) in order to verify information from the Faculty Finder tool, as well as categorize the race and gender of faculty members based on my best guess.")
    )
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
