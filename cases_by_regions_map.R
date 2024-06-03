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
library(leaflet)
library(sf)

# Load the geographical data for Germany states from shapefile
germany_shapefile <- st_read("de_state.shp")

# Check the column names in the shapefile
colnames(germany_shapefile)

#Reproject to WGS84
germany_shapefile <- st_transform(germany_shapefile, crs = 4326)

# Check column names in the shapefile
print(colnames(germany_shapefile))



germany_shapefile <- germany_shapefile %>%
  rename(state = all_of(state_column_name))

# Summarize cases by state
cases_summary <- cases_df %>%
  group_by(state) %>%
  summarize(total_cases = sum(cases, na.rm = TRUE))

# Merge the geographical data with the cases data
germany_shapefile <- germany_shapefile %>%
  left_join(cases_summary, by = "state")

# Define UI
ui <- fluidPage(
  titlePanel("COVID-19 Cases in Germany"),
  sidebarLayout(
    sidebarPanel(
      h3("Map of COVID-19 Cases by State"),
      dateRangeInput("dateRange",
                     label = "Select Date Range:",
                     start = min(cases_df$date),
                     end = max(cases_df$date),
                     min = min(cases_df$date),
                     max = max(cases_df$date))
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  filtered_data <- reactive({
    cases_df %>%
      filter(date >= input$dateRange[1] & date <= input$dateRange[2]) %>%
      group_by(state) %>%
      summarize(total_cases = sum(cases, na.rm = TRUE))
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = germany_shapefile,
                  fillColor = ~colorBin("YlOrRd", filtered_data()$total_cases, bins = 5)(filtered_data()$total_cases),
                  weight = 1,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(
                    weight = 2,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE
                  ),
                  label = ~paste(state, "<br>", "Total Cases:", filtered_data()$total_cases),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"
                  )) %>%
      addLegend(pal = colorBin("YlOrRd", filtered_data()$total_cases, bins = 5), 
                values = filtered_data()$total_cases, 
                opacity = 0.7, 
                title = "Total Cases",
                position = "bottomright")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)