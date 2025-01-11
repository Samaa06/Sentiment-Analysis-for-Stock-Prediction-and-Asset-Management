#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(forecast)
library(plotly)
library(dplyr)
library(DT)
library(prophet)
stock_data <- read.csv("C:/Users/rober/Downloads/cleaned_stock_data.csv")
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")
original_stock_data <- read.csv("C:/Users/rober/Downloads/combined_nasdaq_yf_data_2023.csv")
original_stock_data$Date <- as.Date(original_stock_data$Date, format = "%Y-%m-%d")

# Top 20 ticker symbols
top20_tickers <- c(
  "AAPL", "NVDA", "MSFT", "AMZN", "META", "AVGO", "TSLA", "COST", "GOOGL", "NFLX",
  "ADP", "TMUS", "CSCO", "ADBE", "PEP", "AMD", "LIN", "ISRG", "TXN", "INTU"
)

# Load and filter data between 1/3/2023-12/29/2023
load_stock_data <- function(ticker) {
  filename <- file.path("C:/Users/rober/Downloads/stock_app/stock_app/data", paste0(ticker, ".csv"))
  if (!file.exists(filename)) {
    return(NULL)
  }
  df <- read_csv(filename,
                 col_types = cols(
                   ticker = col_character(),
                   date = col_date(format = "%Y-%m-%d"),
                   open = col_double(),
                   high = col_double(),
                   low = col_double(),
                   close = col_double()
                 ))
  
  df <- df %>% 
    filter(date >= as.Date("2023-01-03") & date <= as.Date("2023-12-29"))
  return(df)
}

# ARIMA Model
fit_arima_model <- function(ts_data) {
  fit <- auto.arima(ts_data)
  return(fit)
}

# Helper function to prepare train/test and forecast for ARIMA
# Returns a list with train_ts, df_train, df_test, df_all, last_date
prepare_data_for_ts <- function(df, train_start, train_end) {
  df_train <- df %>% filter(date >= train_start & date <= train_end)
  df_test <- df %>% filter(date > train_end)
  
  if(nrow(df_train) < 2) {
    return(NULL)
  }
  
  # create time series from training data
  ts_train <- ts(df_train$close, frequency=365)
  
  last_date <- max(df$date)
  
  list(df_all = df,
       df_train = df_train,
       df_test = df_test,
       ts_train = ts_train,
       last_date = last_date)
}


#------------------------------------------------------------
# Shiny UI
#------------------------------------------------------------
# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Stock Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Stock Lookup", tabName = "dashboard1", icon = icon("search")),
      menuItem("Performance Metrics", tabName = "dashboard2", icon = icon("chart-line")),
      menuItem("ARIMA Stock Price Forecast", tabName = "dashboard3", icon = icon("area-chart"))
    )
  ),
  dashboardBody(
  # Dashboard 1: Add feature where the user can see the sentiment score of the chosen stock 
    tabItems(
      tabItem(
        tabName = "dashboard1",
        fluidRow(
          box(
            title = "Select Stock and Date", width = 4,
            dateInput(
              "date_input", 
              "Enter Date between 01/03/2023 - 12/29/2023:", 
              value = Sys.Date(), 
              min = as.Date("2023-01-03"), 
              max = as.Date("2023-12-29")
            ),
            selectInput(
              "sector_input", 
              "Select Sector:", 
              choices = unique(stock_data$Sector)
            ),
            uiOutput("stock_selector")
          ),
          box(
            title = "Stock Data", width = 8,
            tableOutput("stock_table")
          )
        )
      ),
      
      # Dashboard 2: Performance Metrics
      tabItem(
        tabName = "dashboard2",
        fluidRow(
          box(
            title = "Select Stock Ticker", width = 12,
            selectInput(
              "ticker_input", 
              "Select Ticker:", 
              choices = unique(original_stock_data$Ticker), 
              selected = unique(original_stock_data$Ticker)[1] # Set the first ticker as default
            )
          )
        ),
        fluidRow(
          box(
            title = "Stock Performance Over Time", width = 12,
            plotOutput("stock_time_series")
          )
        ),
        fluidRow(
          box(
            title = "Top Stocks by Volume", width = 12,
            plotOutput("top_stocks_volume")
          )
        ),
        fluidRow(
          box(
            title = "Sector Trends Over Time", width = 12,
            plotOutput("sector_trends")
          )
        ),
        fluidRow(
          box(
            title = "Volume Traded by Sector", width = 12,
            plotOutput("volume_by_sector")
          )
        )
      ),
      
      # Dashboard 3: ARIMA/SARIMA Price Forecast
      tabItem(
        tabName = "dashboard3",
        fluidRow(
          box(
            title = "Select Company and Date Range (ARIMA)",
            width = 4,
            selectInput("selected_company_p3", "Company:", choices = top20_tickers, selected = "AAPL"),
            dateInput("arima_train_start", "Train Start Date:", value = "2023-01-03"),
            dateInput("arima_train_end", "Train End Date:", value = "2023-12-29")
          ),
          box(
            title = "Forecast Options",
            width = 4,
            numericInput("arima_h", "Forecast Horizon (days):", value = 30, min = 1, max = 365),
            actionButton("fit_arima_btn", "Fit & Forecast ARIMA")
          ),
          box(
            title = "Description",
            width = 4,
            "Training data: from arima_train_start to arima_train_end.",
            "Test data: after arima_train_end to last available date.",
            "Forecast: beyond last available date by arima days."
          )
        ),
        fluidRow(
          box(
            title = "ARIMA Forecast Plot",
            width = 12,
            plotOutput("arima_plot_p3")
          )
        )
      )
    )
  )
)



#------------------------------------------------------------
# SERVER
#------------------------------------------------------------

# Define the server
server <- function(input, output, session) {
  # Dashboard 1: Stock Lookup
  # Dynamically update stock selector based on date and sector
  output$stock_selector <- renderUI({
    req(input$date_input, input$sector_input)
    
    # Check if the date exists in the dataset
    if (!input$date_input %in% stock_data$Date) {
      return(NULL) # Return NULL if the date is not found
    }
    
    filtered_data <- stock_data %>%
      filter(Date == input$date_input, Sector == input$sector_input)
    
    selectInput(
      "stock_input", 
      "Select Stock:", 
      choices = unique(filtered_data$Name)
    )
  })
  
  # Render the stock data table
  output$stock_table <- renderTable({
    req(input$date_input, input$sector_input, input$stock_input)
    
    # Check if the date exists in the dataset
    if (!input$date_input %in% stock_data$Date) {
      return(NULL) # Return NULL if the date is not found
    }
    
    stock_data %>%
      filter(Date == input$date_input, 
             Sector == input$sector_input, 
             Name == input$stock_input) %>%
      select(Open, High, Low, Close, Volume)
  })
  
  # Display error message if the date is not found
  output$error_message <- renderText({
    req(input$date_input)
    if (!input$date_input %in% stock_data$Date) {
      return("Date Not Found")
    }
    return("")
  })
  
  # Dashboard 2: Time-Series Plot for Selected Ticker
  output$stock_time_series <- renderPlot({
    req(input$ticker_input) # Ensure a ticker is selected
    
    # Filter data for the selected ticker
    data <- original_stock_data %>%
      filter(Ticker == input$ticker_input)
    
    # Create the time-series plot
    ggplot(data, aes(x = Date)) +
      geom_line(aes(y = Open, color = "Open")) +
      geom_line(aes(y = High, color = "High")) +
      geom_line(aes(y = Low, color = "Low")) +
      geom_line(aes(y = Close, color = "Close")) +
      geom_line(aes(y = Volume / 1e5, color = "Volume (in hundred thousands)"), linetype = "dashed") +
      labs(
        title = paste("Stock Performance Over Time:", input$ticker_input),
        x = "Date",
        y = "Value",
        color = "Metric"
      ) +
      scale_color_manual(values = c("Open" = "blue", "High" = "green", "Low" = "red", "Close" = "purple", "Volume (in hundred thousands)" = "orange")) +
      theme_minimal()
  })
  
  
  # Dashboard 2: Performance Metrics
  output$top_stocks_volume <- renderPlot({
    data <- stock_data %>%
      group_by(Name) %>%
      summarize(TotalVolume = sum(Volume, na.rm = TRUE)) %>%
      arrange(desc(TotalVolume)) %>%
      slice_head(n = 10)
    
    ggplot(data, aes(x = reorder(Name, -TotalVolume), y = TotalVolume)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Top 10 Stocks by Volume over 2023", x = "Stock Name", y = "Total Volume Traded") +
      theme_minimal()
  })
  
  # Dashboard 2: Sector Trends Over Time
  output$sector_trends <- renderPlot({
    data <- stock_data %>%
      group_by(Date, Sector) %>%
      summarize(AverageClose = mean(Close, na.rm = TRUE), .groups = "drop")
    
    ggplot(data, aes(x = Date, y = AverageClose, color = Sector)) +
      geom_line() +
      labs(title = "Sector Trends Over Time", x = "Date", y = "Average Close Price") +
      theme_minimal()
  })
  
  # Dashboard 2: Volume Traded by Sector
  output$volume_by_sector <- renderPlot({
    data <- stock_data %>%
      group_by(Sector) %>%
      summarize(TotalVolume = sum(Volume, na.rm = TRUE), .groups = "drop")
    
    ggplot(data, aes(x = reorder(Sector, -TotalVolume), y = TotalVolume, fill = Sector)) +
      geom_bar(stat = "identity") +
      labs(title = "Volume Traded by Sector", x = "Sector", y = "Total Volume") +
      theme_minimal()
  })
  
  # Dashboard 3: ARIMA
  arima_data_p3 <- eventReactive(input$fit_arima_btn, {
    req(input$selected_company_p3)
    df <- load_stock_data(input$selected_company_p3)
    req(df)
    
    data_info <- prepare_data_for_ts(df, input$arima_train_start, input$arima_train_end)
    if (is.null(data_info)) {
      showNotification("Not enough training data for ARIMA.", type="error")
      return(NULL)
    }
    
    fit <- fit_arima_model(data_info$ts_train)
    fc <- forecast(fit, h = input$arima_h)
    
    list(
      df_all = data_info$df_all,
      df_train = data_info$df_train,
      df_test = data_info$df_test,
      fc = fc,
      last_date = data_info$last_date
    )
  })
  
  output$arima_plot_p3 <- renderPlot({
    arima_info <- arima_data_p3()
    req(arima_info)
    
    df_train <- arima_info$df_train %>% mutate(type="Train")
    df_test <- arima_info$df_test %>% mutate(type="Test")
    future_dates <- seq(arima_info$last_date + 1, by="day", length.out = input$arima_h)
    forecast_df <- data.frame(date=future_dates, close=as.numeric(arima_info$fc$mean), type="Forecast")
    
    combined <- bind_rows(df_train, df_test, forecast_df)
    
    ggplot(combined, aes(x=date, y=close, color=type)) +
      geom_line() +
      labs(title=paste("ARIMA Forecast for", input$selected_company_p3), x="Date", y="Close Price") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
