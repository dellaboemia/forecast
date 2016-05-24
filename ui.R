# Coursera Data Science Specialization
# Building Data Products Course
# Housing Prices Time Series Forecasting
# John James
# Start Date: May 23, 2015

#ui.R

library(shiny)
library(shinydashboard)

dashboardPage(skin = "green",
  dashboardHeader(title = "Home Price Forecasting Model"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Select Data", tabName = "selectData", icon = icon("database")),
      menuItem("Explore Data", tabName = "exploreData", icon = icon("area-chart")),
      menuItem("PreProcess Data", tabName = "preProcess", icon = icon("align-center")),
      menuItem("Train Model", tabName = "trainModel", icon = icon("gears")),
      menuItem("Diagnose Performance", tabName = "diagnose", icon = icon("check-square")),
      menuItem("Forecast", tabName = "forecast", icon = icon("line-chart"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "selectData",
              fluidRow(
                box(
                  title = "Location", status = "warning",
                  p(
                    class = "text-muted",
                    paste("To pick a property to analyze, select a state, city and zip code.")),
                  uiOutput("stateUi"),
                  uiOutput("cityUi"),
                  uiOutput("zipUi")
                  
                )
              ))
    )
  )
)