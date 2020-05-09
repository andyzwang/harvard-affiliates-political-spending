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
library(gt)
library(shinyWidgets)
library(plotly)

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
    p(
      "The average spending across all faculty is",
      strong("$791.27"),
      "while the average spending for only those who spend is",
      strong("$2,896.57.")
    ),
    column(1),
    column(
      10,
      DTOutput("individualspending")
    ),
    column(1)
  ),

  # third panel: breakdown of the recipients of Harvard spending

  tabPanel(
    "Spending Recipients",

    # Using subtabs at the top

    tabsetPanel(

      # Tab one: receipient overview

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

      # tab two: the political parties

      tabPanel(
        "Political Parties",
        h2("Political Parties"),
        p("Harvard faculty very strongly prefer spending money on Democratic candidates and organizations. Note: the party count is not completely accurate, due to some partisan organizations with significant amounts of spending (i.e. ActBlue) not registering as partisan with the FEC."),
        column(1),
        column(
          10,
          plotlyOutput("party_spending")
        ),
        column(1)
      ),

      # Tab three: party spending breakdown

      tabPanel(
        "Party Spending Breakdown",
        h2("Party Spending Breakdown"),
        p("As evidenced by the individual party spending breakdown, Harvard faculty very much strongly prefer spending money on Democratic candidates and organizations. However, this table is very much incomplete: some partisan organizations do not register as such with the FEC, and as such, faculty who have spent on these organizations are not reflected in this chart."),
        sidebarLayout(

          # sidebar for selecting party

          sidebarPanel(
            selectInput(
              "party_spending_party",
              "Select A Party",
              c(
                "Democratic" = "DEM",
                "Republican" = "REP",
                "Democratic-Farmer-Labor" = "DFL",
                "Independent" = "IND",
                "Libertarian" = "LIB"
              )
            )
          ),
          mainPanel(DTOutput("party_breakdown"))
        )
      ),

      # Tab four: geographic spending

      tabPanel(
        "Geographic Spending",
        h2("Geographic Spending"),
        p("The political spending of Harvard faculty is dispersed throughout the entire continential U.S., but is seemed to be concentrated in a few key states: California, Illinois, Texas, and Massachusetts. Note that ActBlue has its headquarters in Mass, which significantly distorts the map. Select a region to get a better sense of spending within each region."),
        sidebarLayout(

          # sidebar for selecting region

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
          mainPanel(plotlyOutput("states"))
        )
      ),

      # tab five: donations over time

      tabPanel(
        "Spending Over Time",
        h2("Spending Over Time"),
        p("Political spending by Harvard faculty seemed to pick up starting in January of 2017 (shortly before President Trump's inaguration) to a peak in November of 2018 (midterm elections). From there, it's been steadily rising again as we approach the 2020 elections."),
        column(1),
        column(
          10,
          plotlyOutput("spending_over_time")
        ),
        column(1)
      )
    )
  ),

  # four: modelling spending behavior

  tabPanel(
    "Modelling Spending",

    # Using subtabs at the top

    tabsetPanel(

      # first tab panel: exploratory data analysis

      tabPanel(
        "Demographic Exploration",
        h2("Data Exploration by Demographic"),
        sidebarLayout(
          sidebarPanel(
            p("This data allows you to plot how the three variables interact with each other within the university's different schools."),

            # pick which axis and color variables for scatterplot

            pickerInput(
              inputId = "dem_plot_variables",
              label = "Choose A Plot",
              choices = c(
                "Race Vs Gender Scatterplot",
                "Gender Vs Race Scatterplot",
                "Demographic Histogram",
                "Race Histogram",
                "Gender Histogram"
              ),
              multiple = F
            ),
            p("I wanted to examine if the variables of race and gender would also act on each other to become interactive variables. Given the large differences by both race, gender, and the interaction thereof, I concluded that I should model for the interaction of these terms."),
            gt_output("uni_race_gender")
          ),

          # display both the scatterplot as well as the output

          mainPanel(
            h3("Plot Display"),
            plotlyOutput("dem_plot"),
          )
        )
      ),


      # Tab two: model by school

      tabPanel(
        "By School",
        h2("Modeling Behavior By School"),
        sidebarLayout(
          sidebarPanel(
            p("Choose a variable to compare spending data by, within schools of your choice"),

            # pick which color variables for scatterplot

            pickerInput(
              inputId = "school_plot_variables",
              label = "Choose A Variable",
              choices = c(
                "Race" = "race",
                "Gender" = "gender"
              ),
              multiple = F,
              selected = "race"
            ),

            # select relevant schools

            pickerInput(
              inputId = "school_plot_schools",
              label = "Choose Schools",
              choices = c(
                "Harvard Dental School" = "DENT",
                "Faculty of Arts and Sciences" = "FAS",
                "Graduate School of Design" = "GSD",
                "Graduate School of Education" = "GSE",
                "Harvard Business School" = "HBS",
                "Harvard Kennedy School" = "HKS",
                "Harvard Law School" = "HLS",
                "Harvard Medical School" = "HMS",
                "Harvard School of Public Health" = "HSPH",
                "Radcliffe" = "RAD",
                "School of Engineering and Applied Sciences" = "SEAS",
                "University (Prof and Admin)" = "UNI"
              ),
              multiple = T,
              selected = c(
                "DENT",
                "FAS",
                "GSD",
                "GSE",
                "HBS",
                "HKS",
                "HLS",
                "HMS",
                "HSPH",
                "RAD",
                "SEAS",
                "UNI"
              )
            ),

            p(strong("Predict a Professor's Political Spendings")),
            p("Configure a professor of your choosing, and our model will estimate how much they're likely to spend in the same time period (2017-2020) on political contributions."),

            # picker input for make your professor

            pickerInput(
              inputId = "school_race",
              label = "Choose A Race",
              choices = c(
                "White",
                "African American",
                "Asian"
              ),
              multiple = F,
              selected = "White"
            ),
            pickerInput(
              inputId = "school_gender",
              label = "Choose A Gender",
              choices = c(
                "Male" = "M",
                "Female" = "F"
              ),
              multiple = F,
              selected = "Male"
            ),
            pickerInput(
              inputId = "school_school",
              label = "Choose A School",
              choices = c(
                "Harvard Dental School" = "DENT",
                "Faculty of Arts and Sciences" = "FAS",
                "Graduate School of Design" = "GSD",
                "Graduate School of Education" = "GSE",
                "Harvard Business School" = "HBS",
                "Harvard Kennedy School" = "HKS",
                "Harvard Law School" = "HLS",
                "Harvard Medical School" = "HMS",
                "Harvard School of Public Health" = "HSPH",
                "Radcliffe" = "RAD",
                "School of Engineering and Applied Sciences" = "SEAS",
                "University (Prof and Admin)" = "UNI"
              ),
              multiple = F,
              selected = "HLS"
            ),
          ),

          # display both the scatterplot as well as the output

          mainPanel(
            h3("Plot Display"),
            plotlyOutput("school_plot"),
            verbatimTextOutput("school_model_text"),
            p("Model created by linear regression of three categorical variables, with interaction term between race and gender.")
          )
        )
      ),

      # tab three: subjects

      tabPanel(
        "By Subjects",
        h2("Modeling Behavior By Subjects within FAS"),
        sidebarLayout(
          sidebarPanel(
            p("Choose a variable to compare spending data by, within subjets of your choice"),

            # pick which color variables for scatterplot

            pickerInput(
              inputId = "subject_plot_variables",
              label = "Choose A Variable",
              choices = c(
                "Race" = "race",
                "Gender" = "gender"
              ),
              multiple = F,
              selected = "race"
            ),

            # select relevant schools

            pickerInput(
              inputId = "subject_plot_subjects",
              label = "Choose Schools",
              choices = c(
                "Engineering",
                "Social Sciences",
                "Math/Science",
                "Humanities",
                "Admin"
              ),
              multiple = T,
              selected = c(
                "Engineering",
                "Social Sciences",
                "Math/Science",
                "Humanities",
                "Admin"
              )
            ),

            p(strong("Predict a Professor's Political Spendings")),
            p("Configure a professor of your choosing, and our model will estimate how much they're likely to spend in the same time period (2017-2020) on political contributions."),

            # picker input for make your professor

            pickerInput(
              inputId = "subject_race",
              label = "Choose A Race",
              choices = c(
                "White",
                "African American",
                "Asian"
              ),
              multiple = F,
              selected = "White"
            ),
            pickerInput(
              inputId = "subject_gender",
              label = "Choose A Gender",
              choices = c(
                "Male" = "M",
                "Female" = "F"
              ),
              multiple = F,
              selected = "Male"
            ),
            pickerInput(
              inputId = "subject_subject",
              label = "Choose A School",
              choices = c(
                "Engineering",
                "Social Sciences",
                "Math/Science",
                "Humanities",
                "Admin"
              ),
              multiple = F,
              selected = "Engineering"
            ),
          ),

          # display both the scatterplot as well as the output

          mainPanel(
            h3("Plot Display"),
            plotlyOutput("subject_plot"),
            verbatimTextOutput("subject_model_text"),
            p("Model created by linear regression of three categorical variables, with interaction term between race and gender.")
          )
        )
      )
    )
  ),

  # last tab - self promo

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
  output$party_spending <- renderPlotly({
    party_spending_plot <- ggplot(parties, aes(x = party, y = total_spending)) +
      geom_col(fill = "lightblue") +
      theme_classic() +
      scale_y_continuous(labels = scales::dollar) +
      labs(
        title = "Faculty Spendings to Each Party",
        x = "Party",
        y = "Sum of Spendings"
      )
    ggplotly(party_spending_plot)
  })

  # render an adjustable table of party donations (responsive to input)
  output$party_breakdown <- renderDataTable({

    # setting the filter depending on what the input is

    donations_party_render <- individual_donations_party %>%
      filter(Party == input$party_spending_party)

    datatable(donations_party_render,
      options = list(
        pageLength = 20
      )
    ) %>%
      formatCurrency("Sum of Spending", "$")
  })

  # creating a map of spending data

  output$states <- renderPlotly({

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

    region_map <- region_map +
      scale_fill_continuous(
        low = "white", high = "blue",
        name = "Dollars Spent", label = scales::dollar
      ) +
      theme(legend.position = "right") +
      labs(
        title = "States Recieving Harvard Political Spending",
        subtitle = "Using FEC Data from 2017-2020"
      )
    ggplotly(region_map)
  })

  # rendering a ggplot of spending over time

  output$spending_over_time <- renderPlotly({
    time_plot <- ggplot(spending_over_time, aes(
      x = month,
      y = spending,
      color = as.factor(cycle)
    )) +
      geom_line() +
      labs(
        x = "Date",
        y = "Total Spending",
        title = "Total Harvard Political Spending Per Month",
        color = "Cycle"
      ) +
      theme_classic() +
      scale_y_continuous(label = scales::dollar) +
      geom_smooth(method = "glm", se = FALSE)
    ggplotly(time_plot)
  })

  # including source for FAS model

  source("models.R")

  # making gt for race and gender correlation

  output$uni_race_gender <- render_gt({
    uni_race_gender_breakdown %>%
      gt() %>%
      tab_header(title = "Mean Spending by Race, Gender") %>%
      cols_label(
        race = "Race",
        gender = "Gender",
        mean_spending = "Mean Spending"
      ) %>%
      fmt_currency(columns = vars(mean_spending))
  })

  output$dem_plot <- renderPlotly({

    # Gender and Race scatterplot

    if (input$dem_plot_variables == "Gender Vs Race Scatterplot") {
      dem_plotly <- ggplot(gender_race_school_data, aes(
        x = gender, y = spending_sum, color = race
      )) +
        geom_jitter(width = .25, height = .1) +
        scale_y_continuous(labels = scales::dollar) +
        theme_classic() +
        labs(
          title = "Political Spending: Gender Vs Race",
          y = "Sum of Political Spending Per Person",
          color = "Race"
        )
    }

    # race and gender scatterplot

    else if (input$dem_plot_variables == "Race Vs Gender Scatterplot") {
      dem_plotly <- ggplot(gender_race_school_data, aes(
        x = race, y = spending_sum, color = gender
      )) +
        geom_jitter(width = .25, height = .1) +
        scale_y_continuous(labels = scales::dollar) +
        theme_classic() +
        labs(
          title = "Political Spending: Race Vs Gender",
          y = "Sum of Political Spending Per Person",
          color = "Gender"
        )
    }

    # demographic histogram

    else if (input$dem_plot_variables == "Demographic Histogram") {
      dem_plotly <- ggplot(race_gender_data_hist, aes(
        x = spending_sum,
        fill = demographic
      )) +
        geom_histogram(bins = 100) +
        theme_classic() +
        scale_x_continuous(labels = scales::dollar) +
        labs(
          title = "Political Spending: Gender and Race",
          x = "Individual Political Spending",
          fill = "Demographic"
        )
    }

    # race histogram

    else if (input$dem_plot_variables == "Race Histogram") {
      dem_plotly <- ggplot(race_gender_data_hist, aes(
        x = spending_sum,
        fill = race
      )) +
        geom_histogram(bins = 100) +
        theme_classic() +
        scale_x_continuous(labels = scales::dollar) +
        labs(
          title = "Political Spending: Race",
          x = "Individual Political Spending",
          fill = "Race"
        )
    }

    # gender histogram

    else if (input$dem_plot_variables == "Gender Histogram") {
      dem_plotly <- ggplot(race_gender_data_hist, aes(
        x = spending_sum,
        fill = gender
      )) +
        geom_histogram(bins = 100) +
        theme_classic() +
        scale_x_continuous(labels = scales::dollar) +
        labs(
          title = "Political Spending: Gender",
          x = "Individual Political Spending",
          fill = "Gender"
        )
    }
    ggplotly(dem_plotly)
  })

  output$school_plot <- renderPlotly({

    # filtering for the schools listed

    school_plotly_data <- gender_race_school_data %>%
      filter(school %in% input$school_plot_schools)

    # graph
    if (input$school_plot_variables == "race") {
      school_plotly <- ggplot(school_plotly_data, aes(
        x = school, y = spending_sum, color = race
      ))
    }
    else if (input$school_plot_variables == "gender") {
      school_plotly <- ggplot(school_plotly_data, aes(
        x = school, y = spending_sum, color = gender
      ))
    }

    school_plotly <- school_plotly +
      geom_jitter(width = .25, height = .1) +
      scale_y_continuous(labels = scales::dollar) +
      theme_classic() +
      labs(
        title = str_to_title(
          paste("Political Spending by School,", input$school_plot_variables)
        ),
        y = "Sum of Political Spending Per Person"
      )

    ggplotly(school_plotly)
  })

  # working on the school plot "black box" output

  output$school_model_text <- renderText({

    # creating a data frame with out inputs

    school_model_data <- data.frame(
      gender = input$school_gender,
      school = input$school_school,
      race = input$school_race
    )

    # using the output

    school_model_output <- predict(gender_race_school_model,
      school_model_data,
      se.fit = TRUE
    )

    paste("A professor that is ",
      gender = input$school_gender,
      " and ",
      input$school_race,
      " working at ",
      input$school_school,
      " is predicted to have donated\n",
      scales::dollar(as.numeric(school_model_output[1])),
      ", with a standard error of ",
      scales::dollar(as.numeric(school_model_output[2])),
      ".",
      sep = ""
    )
  })
  output$subject_plot <- renderPlotly({

    # filtering for the subjects listed

    subject_plotly_data <- gender_race_subject_data %>%
      filter(field %in% input$subject_plot_subjects)

    # graph
    if (input$subject_plot_variables == "race") {
      subject_plotly <- ggplot(subject_plotly_data, aes(
        x = field, y = spending_sum, color = race
      ))
    }
    else if (input$subject_plot_variables == "gender") {
      subject_plotly <- ggplot(subject_plotly_data, aes(
        x = field, y = spending_sum, color = gender
      ))
    }

    subject_plotly <- subject_plotly +
      geom_jitter(width = .25, height = .1) +
      scale_y_continuous(labels = scales::dollar) +
      theme_classic() +
      labs(
        title = str_to_title(
          paste("Political Spending by Subject,", input$subject_plot_variables)
        ),
        y = "Sum of Political Spending Per Person"
      )

    ggplotly(subject_plotly)
  })

  # working on the subject plot "black box" output

  output$subject_model_text <- renderText({

    # creating a data frame with out inputs

    subject_model_data <- data.frame(
      gender = input$subject_gender,
      field = input$subject_subject,
      race = input$subject_race
    )

    # using the output

    subject_model_output <- predict(subject_race_gender_model,
      subject_model_data,
      se.fit = TRUE
    )

    paste("A professor that is ",
      gender = input$subject_gender,
      " and ",
      input$subject_race,
      " working in the ",
      input$subject_subject,
      " department is predicted to have donated\n",
      scales::dollar(as.numeric(subject_model_output[1])),
      ", with a standard error of ",
      scales::dollar(as.numeric(subject_model_output[2])),
      ".",
      sep = ""
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
