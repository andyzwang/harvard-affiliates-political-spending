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
    column(1),
    column(
      10,
      DTOutput("individualspending")
    ),
    column(1)
  ),
  tabPanel(
    "Spending Recipients",
    tabsetPanel(
      tabPanel(
        "Recipients Overview",
        h2("Recipient Overview"),
        p("Here's an overview of the committees to whom Harvard faculty donated between 2017-2020."),
        column(1),
        column(
          10,
          DTOutput("recipient_overview")
        ),
        column(1)
      ),
      tabPanel(
        "Political Parties",
        h2("Political Parties"),
        p("Harvard faculty very strongly prefer spending money on Democratic candidates and organizations. Note: the Party count is not completely accurate, due to some partisan organizations with significant amounts of spending (i.e. ActBlue) not registering as partisan with the FEC."),
        column(1),
        column(
          10,
          plotOutput("party_spending")
        ),
        column(1)
      ),
      tabPanel(
        "Party Spending Breakdown",
        h2("Party Spending Breakdown"),
        p("As evidenced by the individual party spending breakdown, Harvard faculty very much strongly prefer spending money on Democratic candidates and organizations. However, this table is very much incomplete: some partisan organizations do not register as such with the FEC, and as such, faculty who have spent on these organizations are not reflected in this chart."),
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "party_spending_party",
              "Select A Party",
              c(
                "Democratic" = "dem",
                "Republican" = "gop",
                "Democratic-Farmer-Labor" = "dfl",
                "Independent" = "ind",
                "Libertarian" = "lib"
              )
            )
          ),
          mainPanel(DTOutput("party_breakdown"))
        )
      ),
      tabPanel(
        "Geographic Spending",
        h2("Geographic Spending"),
        p("The political spending of Harvard faculty is dispersed throughout the entire continential U.S., but is seemed to be concentrated in a few key states: California, Illinois, Texas, and Massachusetts. Note that ActBlue has its headquarters in Mass, which significantly distorts the map. Select a region to get a better sense of spending within each region."),
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "region_selection",
              "Select A Region",
              c(
                "Entire U.S." = "us",
                "New England" = "ne",
                "Mid Atlantic" = "ma",
                "East North Central" = "enc",
                "South Atlantic" = "sa",
                "East South Central" = "esc",
                "West South Central" = "wsc",
                "Mountain" = "m",
                "Pacific" = "p"
              )
            )
          ),
          mainPanel(plotOutput("states"))
        )
      ),
      tabPanel(
        "Spending Over Time",
        h2("Spending Over Time"),
        p("Political spending by Harvard faculty seemed to pick up starting in January of 2017 (shortly before President Trump's inaguration) to a peak in November of 2018 (midterm elections). From there, it's been steadily rising again as we approach the 2020 elections."),
        column(1),
        column(
          10,
          plotOutput("spending_over_time")
        ),
        column(1)
      )
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
        pageLength = 20
      )
    ) %>%
      formatCurrency("Sum of Spending", "$")
  })

  # including recipients data source

  source("recipients.R")

  # rendering a data table based off of spending_recipients, formatting to
  # have spending displayed as a dollar.

  output$recipient_overview <- renderDataTable({
    datatable(spending_recipients_ui,
      options = list(
        pageLength = 20
      )
    ) %>%
      formatCurrency("Sum of Spending", "$")
  })

  # rendering a ggplot of party spending
  output$party_spending <- renderPlot({
    ggplot(parties, aes(x = party, y = total_spending)) +
      geom_col(fill = "lightblue") +
      theme_classic() +
      scale_y_continuous(labels = scales::dollar) +
      labs(
        title = "Faculty Spendings to Each Party",
        x = "Party",
        y = "Sum of Spendings"
      )
  })

  # render an adjustable table of party donations (responsive to input)
  output$party_breakdown <- renderDataTable({

    # setting the filter depending on what the input is

    if (input$party_spending_party == "dem") {
      donations_party_render <- individual_donations_party %>%
        filter(Party == "DEM")
    }
    else if (input$party_spending_party == "gop") {
      donations_party_render <- individual_donations_party %>%
        filter(Party == "REP")
    }
    else if (input$party_spending_party == "dfl") {
      donations_party_render <- individual_donations_party %>%
        filter(Party == "DFL")
    }
    else if (input$party_spending_party == "ind") {
      donations_party_render <- individual_donations_party %>%
        filter(Party == "IND")
    }
    else if (input$party_spending_party == "lib") {
      donations_party_render <- individual_donations_party %>%
        filter(Party == "LIB")
    }

    datatable(donations_party_render,
      options = list(
        pageLength = 20
      )
    ) %>%
      formatCurrency("Sum of Spending", "$")
  })

  # creating a map of spending data

  output$states <- renderPlot({

    # setting the region depending on what the input is

    if (input$region_selection == "us") {
      region_map <- plot_usmap(data = states, values = "total_spending")
    }
    else if (input$region_selection == "ne") {
      region_map <- plot_usmap(
        data = states,
        values = "total_spending",
        include = .new_england
      )
    }
    else if (input$region_selection == "ma") {
      region_map <- plot_usmap(
        data = states,
        values = "total_spending",
        include = .mid_atlantic
      )
    }
    else if (input$region_selection == "enc") {
      region_map <- plot_usmap(
        data = states,
        values = "total_spending",
        include = .east_north_central
      )
    }
    else if (input$region_selection == "wnc") {
      region_map <- plot_usmap(
        data = states,
        values = "total_spending",
        include = .west_north_central
      )
    }
    else if (input$region_selection == "sa") {
      region_map <- plot_usmap(
        data = states,
        values = "total_spending",
        include = .south_atlantic
      )
    }
    else if (input$region_selection == "esc") {
      region_map <- plot_usmap(
        data = states,
        values = "total_spending",
        include = .east_south_central
      )
    }
    else if (input$region_selection == "wsc") {
      region_map <- plot_usmap(
        data = states,
        values = "total_spending",
        include = .west_south_central
      )
    }
    else if (input$region_selection == "m") {
      region_map <- plot_usmap(
        data = states,
        values = "total_spending",
        include = .mountain
      )
    }
    else if (input$region_selection == "p") {
      region_map <- plot_usmap(
        data = states,
        values = "total_spending",
        include = .pacific
      )
    }

    # plot final map

    region_map +
      scale_fill_continuous(
        low = "white", high = "blue",
        name = "Dollars Spent", label = scales::dollar
      ) +
      theme(legend.position = "right") +
      labs(
        title = "States Recieving Harvard Political Spending",
        subtitle = "Using FEC Data from 2017-2020"
      )
  })
  
  # rendering a ggplot of spending over time
  
  output$spending_over_time <- renderPlot({
    ggplot(spending_over_time, aes(x = month, y = donations)) +
      geom_line() +
      labs(
        x = "Date",
        y = "Total Spending",
        title = "Total Harvard Political Spending Per Month"
      ) +
      theme_classic() +
      scale_y_continuous(label = scales::dollar) 
  })
}

# Run the application
shinyApp(ui = ui, server = server)
