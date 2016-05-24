# Coursera Data Science Specialization
# Building Data Products Course
# Housing Prices Time Series Forecasting
# John James
# Start Date: May 23, 2015

# server.R

library(shiny)
library(datasets)
library(dplyr)
library(plyr)

# Read housing price data
zhvi <- read.csv("./data/zhviData.csv", row.names = NULL)
zhvi <- arrange(zhvi, StateName, City, RegionName)


shinyServer(function(input, output) {

  # Render list of states
  output$stateUi <- renderUI({
    selectInput("state", label = "State:", choices = c(Choose='', state.name), selectize = FALSE)
  })
  
  # Render list of cities for state selected
  output$cityUi <- renderUI({
    if (is.null(input$state)) {
      s <- ""
    } else {
      s <- input$state
    }
    cities <- data.frame(City = unique(zhvi[zhvi$StateName == s,"City"]))
    selectInput("city", label = "City:", choices = c("Choose", as.character(cities$City)), selectize = FALSE)
  })
  
  # Render list of zip codes for city selected
  output$zipUi <- renderUI({
    if (is.null(input$city)) {
      c <- ""
    } else {
      c <- input$city
    }
    zips <- data.frame(Zip = unique(zhvi[zhvi$City == c, "RegionName"]))
    selectInput("zip", label = "Zip:", choices = c(Choose='',as.character(zips$Zip)), 
                selectize = FALSE)
  })
})
