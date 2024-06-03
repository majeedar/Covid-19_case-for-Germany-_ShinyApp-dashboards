# DATA LOADING PROCESS


# Set working directory
setwd("C:/Git_files/corona_germany")

# Read the needed files( cases, vaccines and demorgraphics) to R environment
cases_df <- read.csv("covid_de.csv")
vac_df <- read.csv("covid_de_vaccines.csv")
demo_df <- read.csv("demographics_de.csv")

## Data Exploration and CLEANING

head(cases_df)
head(demo_df)
head(vac_df)

# check for missing data
# Load necessary library
library(dplyr)

# Function to count missing data per column
count_missing_data <- function(df) {
  missing_count <- colSums(is.na(df))
  return(missing_count)
}

## TREATING MISSING VALUES IN CASES DF


# Function to calculate the mode
calculate_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

# Calculate mode for gender and age_group in cases DF
mode_gender_cases <- calculate_mode(cases_df$gender)
mode_age_group_cases <- calculate_mode(cases_df$age_group)

# Replace NA values with the mode
cases_df$gender[is.na(cases_df$gender)] <- mode_gender_cases
cases_df$age_group[is.na(cases_df$age_group)] <- mode_age_group_cases

# check forNAs in the updated data frame
print(count_missing_data(cases_df))

#load lubridate library
library(lubridate)

# Convert the date column to Date type in cases and vaccines DFs
cases_df$date <- as.Date(cases_df$date)
vac_df$date <- as.Date(vac_df$date)

library(shiny)
library(ggplot2)
library(dplyr)


# Define UI
ui <- fluidPage(
  titlePanel("COVID-19 Total Cases and Fatality Rate for Germany"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select a State:", choices = NULL, 
                  selected = "Nation-wide"),
      dateRangeInput("dateRange", "Select Date Range:",
                     start = NULL,
                     end = NULL)
    ),
    mainPanel(
      plotOutput("casesPlot"),
      plotOutput("fatalityRatePlot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
 
  df <- cases_df
  # Convert the date column to Date type
  df$date <- as.Date(df$date)
  
  # Create a new column for month_year
  df$month_year <- format(df$date, "%Y-%m")
  
  # Group by state and month_year, then calculate total cases and deaths
  grouped_df <- df %>%
    group_by(state, month_year) %>%
    summarise(
      cases = sum(cases),
      deaths = sum(deaths)
    ) %>%
    ungroup()
  
  # Calculate case fatality rate
  grouped_df <- grouped_df %>%
    mutate(case_fatality_rate = deaths / cases)
  
  # Update the state selection input based on the data
  updateSelectInput(session, "state", choices = c("Nation-wide", unique(grouped_df$state)), selected = "All")
  
  # Initialize the date range input
  observe({
    updateDateRangeInput(session, "dateRange",
                         start = min(df$date),
                         end = max(df$date),
                         min = min(df$date),
                         max = max(df$date))
  })
  
  # Observe the date range input and filter data accordingly
  filtered_data <- reactive({
    req(input$dateRange)
    
    start_date <- as.Date(input$dateRange[1])
    end_date <- as.Date(input$dateRange[2])
    
    grouped_df %>%
      filter(as.Date(paste0(month_year, "-01")) >= start_date,
             as.Date(paste0(month_year, "-01")) <= end_date)
  })
  
  # Plot total cases over time
  output$casesPlot <- renderPlot({
    req(input$state)
    
    state_data <- filtered_data()
    
    if (input$state != "Nation-wide") {
      state_data <- state_data %>% filter(state == input$state)
    } else {
      state_data <- state_data %>%
        group_by(month_year) %>%
        summarise(cases = sum(cases), .groups = 'drop')
    }
    
    ggplot(state_data, aes(x = as.Date(paste0(month_year, "-01")), y = cases)) +
      geom_line(color = "blue") +
      labs(title = paste("Total Cases in", input$state),
           x = "Date", y = "Total Cases") +
      theme_minimal()
  })
  
  # Plot case fatality rate over time
  output$fatalityRatePlot <- renderPlot({
    req(input$state)
    
    state_data <- filtered_data()
    
    if (input$state != "Nation-wide") {
      state_data <- state_data %>% filter(state == input$state)
    } else {
      state_data <- state_data %>%
        group_by(month_year) %>%
        summarise(deaths = sum(deaths), cases = sum(cases), .groups = 'drop') %>%
        mutate(case_fatality_rate = deaths / cases)
    }
    
    ggplot(state_data, aes(x = as.Date(paste0(month_year, "-01")), y = case_fatality_rate)) +
      geom_line(color = "red") +
      labs(title = paste("Case Fatality Rate in", input$state),
           x = "Date", y = "Case Fatality Rate") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
