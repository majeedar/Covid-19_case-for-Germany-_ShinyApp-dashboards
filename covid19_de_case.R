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

# Check and display the count of missing data in cases_df
cat("Missing data count in cases_df:\n")
print(count_missing_data(cases_df))



# Check and display the count of missing data in demo_df
cat("Missing data count in demo_df:\n")
print(count_missing_data(demo_df))

# Check and display the count of missing data in vac_df
cat("Missing data count in vac_df:\n")
print(count_missing_data(vac_df))

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

## DATA VISUALISATION WITH SHINY APP

# Create shiny App object to display number of cases per region per year
#Load shiny app
library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("Covid19 Case by regions in Germany"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearRange",
                  "Select Year Range:",
                  min = min(year(cases_df$date)),
                  max = max(year(cases_df$date)),
                  value = c(min(year(cases_df$date)), max(year(cases_df$date))),
                  sep = ""),
      sliderInput("monthRange",
                  "Select Month Range:",
                  min = 1,
                  max = 12,
                  value = c(1, 12),
                  ticks = FALSE,
                  step = 1,
                  animate = TRUE,
                  pre = "Month: ",
                  sep = ""),
      selectInput("regionSelect",
                  "Select Region(s):",
                  choices = unique(cases_df$state),
                  multiple = TRUE,
                  selected = NULL)
    ),
    
    mainPanel(
      plotOutput("histogram")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$histogram <- renderPlot({
    # Filter data based on input ranges and selected regions
    filtered_data <- cases_df %>%
      mutate(year = year(date), month = month(date)) %>%
      filter(year >= input$yearRange[1],
             year <= input$yearRange[2],
             month >= input$monthRange[1],
             month <= input$monthRange[2],
             state %in% input$stateSelect) %>%
      group_by(year, month) %>%
      summarize(monthly_cases = sum(cases),
                monthly_deaths = sum(deaths)) # Calculate sum of deaths
    
    # Generate the histogram
    p <- ggplot(filtered_data, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = monthly_cases)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      geom_line(aes(y = monthly_deaths, group = 1), color = "red", size = 1.5) +  # Add line plot for deaths
      labs(x = "Date", y = "Number of Cases", title = "Monthly Number of Cases and Deaths Over Time") +
      scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    p
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
