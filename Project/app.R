library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(plotly)

# Function to load data and create a mapping of country codes to display names
load_and_map_data <- function(file_path) {
  data <- read_csv(file_path, col_types = cols(
    country_code = col_character(),
    country_name = col_character(),
    indicator_id = col_character(),
    indicator_name = col_character(),
    index_id = col_character(),
    index_name = col_character(),
    value = col_character(),  # Read as character to handle cleaning
    year = col_character()    # Read as character to handle cleaning
  ))
  
  # Print parsing problems if any
  if (nrow(problems(data)) > 0) {
    print(problems(data))
  }
  
  # Clean the value and year columns
  data <- data %>%
    mutate(
      value = as.numeric(gsub("[^0-9.]", "", value)),
      year = as.numeric(gsub("[^0-9]", "", year))
    ) %>%
    filter(!is.na(year) & year >= 1900 & year <= as.numeric(format(Sys.Date(), "%Y")))  # Keep only valid years
  
  country_code <- str_extract(basename(file_path), "(?<=_)[A-Za-z]+(?=\\.csv)")
  country_name <- case_when(
    country_code == "ind" ~ "INDIA",
    country_code == "irl" ~ "IRELAND",
    country_code == "nga" ~ "NIGERIA",
    country_code == "jpn" ~ "JAPAN",
    country_code == "sgp" ~ "SINGAPORE",
    country_code == "fra" ~ "FRANCE",
    country_code == "khm" ~ "CAMBODIA",
    TRUE ~ toupper(country_code)
  )
  data <- data %>% mutate(country_code = country_code, country = country_name)
  return(data)
}

# Load built-in datasets
data_files <- list.files("data", pattern = "\\.csv$", full.names = TRUE)
hd_data <- map_dfr(data_files, load_and_map_data)

# Remove any invalid country names
hd_data <- hd_data %>% filter(!str_detect(country, "#country\\+name"))

# Columns to exclude from the X and Y axis choices
exclude_cols <- c("country_code", "index_name", "index_id", "indicator_id", "indicator_name", "country_name")

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$head(
    tags$style(HTML("
      .shiny-input-container {
        margin-bottom: 20px;
      }
      .sidebar {
        background-color: #f8f9fa;
        padding: 20px;
        border-radius: 10px;
        box-shadow: 0px 0px 10px rgba(0,0,0,0.1);
      }
      .main-panel {
        padding: 20px;
      }
      .overview-text {
        font-size: 18px;  /* Increased font size */
        font-weight: bold;  /* Bold text */
      }
    "))
  ),
  div(class = "title",
      h2(uiOutput("dynamicTitle"))
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      fluidRow(
        column(10, selectizeInput("country", "Select Country:", choices = unique(hd_data$country), multiple = TRUE, options = list(plugins = list('remove_button')))),
        column(2, actionButton("clear_country", "Clear", class = "btn btn-danger btn-sm"))
      ),
      numericInput("rows", "Number of Rows:", min = 1, max = 100, value = 10),
      fluidRow(
        column(10, selectizeInput("columns", "Select Columns:", choices = names(hd_data), multiple = TRUE, options = list(plugins = list('remove_button')))),
        column(2, actionButton("clear_columns", "Clear", class = "btn btn-danger btn-sm"))
      ),
      fileInput("file", "Upload Additional Data:", accept = ".csv"),
      actionButton("update", "Update Data", class = "btn btn-primary"),
      
      # Inputs for Plot 1
      h3("Plot 1 Settings"),
      selectInput("plot1_x", "X-axis:", choices = setdiff(names(hd_data), exclude_cols), selected = "year"),
      selectInput("plot1_y", "Y-axis:", choices = setdiff(names(hd_data), exclude_cols), selected = "value"),
      fluidRow(
        column(10, selectizeInput("indicatorsPlot1", "Select Indicators for Plot 1:", choices = unique(hd_data$indicator_name), multiple = TRUE, options = list(plugins = list('remove_button')))),
        column(2, actionButton("clear_indicatorsPlot1", "Clear", class = "btn btn-danger btn-sm"))
      ),
      selectInput("plot1_type", "Plot Type:", choices = c("Line Plot" = "line", "Bar Plot" = "bar", "Scatter Plot" = "scatter")),
      numericInput("plot1_ncol", "Number of Columns in Facet:", min = 1, max = 5, value = 2),
      sliderInput("year_range_plot1", "Select Year Range for Plot 1:", min = 1990, max = max(hd_data$year, na.rm = TRUE), value = c(1990, max(hd_data$year, na.rm = TRUE)), step = 1, sep = ""),
      
      # Inputs for Plot 2
      h3("Plot 2 Settings"),
      selectizeInput("plot2_x_indicator", "X-axis Indicator:", choices = unique(hd_data$indicator_name), multiple = FALSE),
      selectizeInput("plot2_y_indicator", "Y-axis Indicator:", choices = unique(hd_data$indicator_name), multiple = FALSE),
      selectizeInput("plot2_countries", "Select Countries for Plot 2:", choices = unique(hd_data$country), multiple = TRUE, options = list(plugins = list('remove_button'))),
      sliderInput("year_range_plot2", "Select Year Range for Plot 2:", min = 1990, max = max(hd_data$year, na.rm = TRUE), value = c(1990, max(hd_data$year, na.rm = TRUE)), step = 1, sep = "")
    ),
    
    mainPanel(
      class = "main-panel",
      tabsetPanel(
        id = "main_tabs",  # Add an id for the tabsetPanel
        tabPanel("Overview", 
                 div(class = "overview-text",  # Add class for styling
                     h4("App Overview"),
                     p("This app allows users to explore Human Development Indicators for various countries."),
                     p("You can select multiple countries and visualize their data through different plots."),
                     p("The data table displays a portion of the selected data with customizable columns."),
                     p("Plot 1: Visualize trends of selected indicators over time."),
                     p("Plot 2: Compare values of two selected indicators using a scatter plot."),
                     p("Hover over points in the plots to see detailed information.")
                 )
        ),
        tabPanel("Data Table", DTOutput("dataTable")),
        tabPanel("Plot 1", plotlyOutput("plot1", height = "700px")),
        tabPanel("Plot 2", plotlyOutput("plot2", height = "700px"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  selected_data <- reactive({
    req(input$country)
    hd_data %>% filter(country %in% input$country, year >= input$year_range_plot1[1], year <= input$year_range_plot1[2])
  })
  
  selected_data_plot2 <- reactive({
    req(input$plot2_x_indicator, input$plot2_y_indicator, input$plot2_countries)
    data_x <- hd_data %>%
      filter(indicator_name == input$plot2_x_indicator, year >= input$year_range_plot2[1], year <= input$year_range_plot2[2], country %in% input$plot2_countries) %>%
      rename(value_x = value)
    
    data_y <- hd_data %>%
      filter(indicator_name == input$plot2_y_indicator, year >= input$year_range_plot2[1], year <= input$year_range_plot2[2], country %in% input$plot2_countries) %>%
      rename(value_y = value)
    
    merged_data <- inner_join(data_x, data_y, by = c("country", "year"))
    return(merged_data)
  })
  
  output$dynamicTitle <- renderUI({
    if (is.null(input$country) || length(input$country) == 0) {
      h2("Human Development Indicators")
    } else {
      h2(paste("Data for", paste(input$country, collapse = ", ")))
    }
  })
  
  observeEvent(input$clear_country, {
    updateSelectizeInput(session, "country", selected = character(0))
  })
  
  observeEvent(input$clear_columns, {
    updateSelectizeInput(session, "columns", selected = character(0))
  })
  
  observeEvent(input$clear_indicatorsPlot1, {
    updateSelectizeInput(session, "indicatorsPlot1", selected = character(0))
  })
  
  output$columnsSelector <- renderUI({
    selectizeInput("columns", "Select Columns:", choices = names(hd_data), multiple = TRUE, options = list(plugins = list('remove_button')))
  })
  
  output$indicatorSelectorPlot1 <- renderUI({
    selectizeInput("indicatorsPlot1", "Select Indicators for Plot 1:", choices = unique(hd_data$indicator_name), multiple = TRUE, options = list(plugins = list('remove_button')))
  })
  
  output$dataTable <- renderDT({
    req(input$columns)
    datatable(selected_data() %>% select(all_of(input$columns)) %>% head(input$rows), options = list(pageLength = input$rows))
  })
  
  output$plot1 <- renderPlotly({
    req(input$indicatorsPlot1)
    plot_data <- selected_data() %>% filter(indicator_name %in% input$indicatorsPlot1)
    
    p <- ggplot(plot_data, aes_string(x = input$plot1_x, y = input$plot1_y, color = "country", group = "country")) +
      {if (input$plot1_type == "line") geom_line() else if (input$plot1_type == "bar") geom_bar(stat = "identity") else geom_point()} +
      facet_wrap(~indicator_name, scales = "free_y", ncol = input$plot1_ncol) +  # Adjust number of columns here
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
      labs(title = "Plot 1")
    
    ggplotly(p)
  })
  
  output$plot2 <- renderPlotly({
    req(input$plot2_x_indicator, input$plot2_y_indicator, input$plot2_countries)
    plot_data <- selected_data_plot2()
    
    p <- ggplot(plot_data, aes(x = value_x, y = value_y, color = country)) +
      geom_point() +
      theme_minimal() +
      labs(
        title = "Plot 2: Scatter Plot of Selected Indicators",
        x = input$plot2_x_indicator,
        y = input$plot2_y_indicator
      )
    
    ggplotly(p)
  })
  
  observeEvent(input$file, {
    new_data <- load_and_map_data(input$file$datapath)
    hd_data <<- bind_rows(hd_data, new_data)
    hd_data <<- hd_data %>% filter(!str_detect(country, "#country\\+name"))  # Remove any invalid country names
    updateSelectInput(session, "country", choices = unique(hd_data$country))
    updateSelectInput(session, "plot2_x_indicator", choices = unique(hd_data$indicator_name))
    updateSelectInput(session, "plot2_y_indicator", choices = unique(hd_data$indicator_name))
    updateSelectInput(session, "plot1_x", choices = setdiff(names(hd_data), exclude_cols))
    updateSelectInput(session, "plot1_y", choices = setdiff(names(hd_data), exclude_cols))
    updateSelectInput(session, "indicatorsPlot1", choices = unique(hd_data$indicator_name))
  })
}

# Run the app
shinyApp(ui, server)

               