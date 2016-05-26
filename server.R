# Coursera Data Science Specialization
# Building Data Products Course
# Housing Prices Time Series Forecasting
# John James
# Start Date: May 23, 2015

# server.R

library(datasets)
library(dplyr)
library(forecast)
library(ggplot2)
library(plyr)
library(shiny)
library(TTR)
library(xlsx)

# Read housing price data
zhvi <- read.csv("./data/zhviData.csv", row.names = NULL)
zhvi <<- arrange(zhvi, StateName, City, RegionName)

#Read housing summary data
zhviSummary <- read.csv("./data/Zip_Zhvi_Summary_AllHomes.csv")
colnames(zhviSummary)[13] <- "fiveYear"
colnames(zhviSummary)[14] <- "tenYear"

#Read model data
modelData <- read.xlsx("./data/models.xlsx", sheetIndex = 1, header = TRUE)

#Default  Values
dfltState <- "California"
dfltCity  <- "San Francisco"
dfltZip   <- 94133
dfltModel <- "ARIMA"
dfltSplit <- 2014

shinyServer(function(input, output) {

  # Render list of states
  output$stateUi <- renderUI({
    selectInput("state", label = "State:", choices = c(Choose='', state.name), selected = dfltState, selectize = FALSE)
  })
  
  # Render list of cities for state selected
  output$cityUi <- renderUI({
    if (is.null(input$state)) {
      s <- dfltState
    } else {
      s <- input$state
    }
    cities <- data.frame(City = unique(zhvi[zhvi$StateName == s,"City"]))
    selectInput("city", label = "City:", choices = c("Choose", as.character(cities$City)), selected = dfltCity, selectize = FALSE)
  })
  
  # Render list of zip codes for city selected
  output$zipUi <- renderUI({
    if (is.null(input$city)) {
      c <- dfltCity
    } else {
      c <- input$city
    }
    zips <- data.frame(Zip = unique(zhvi[zhvi$City == c, "RegionName"]))
    selectInput("zip", label = "Zip:", choices = c(Choose='',as.character(zips$Zip)), selected = dfltZip, selectize = FALSE)
  })

  #Get housing summary information
  getSummary <- reactive ({
    if (is.null(input$zip)) {
      z <- dfltZip
    } else {
      z <- input$zip
    }
    subset(zhviSummary, RegionName == z)
  })

  #Render Median Market Price Box
  output$medianPriceBox <- renderValueBox({
    if (is.null(input$city)) {
      c <- dfltCity
      z <- dfltZip
      s <- dfltState
    } else {
      c <- input$city
      z <- input$zip
      s <- input$state
    }
    valueBox(
      paste0("$",getSummary()$Zhvi), paste("Median House Price in ",c,", ",s," ",z), icon = icon("dollar"), color = "green"
      )
  })
  
  #Render Monthly Price Growth  Box
  output$momBox <- renderValueBox({
    valueBox(
      paste0(round(getSummary()$MoM * 100,4), "%"), "Month to Month Price Growth", icon = icon("bullseye"), color = "blue"
    )
  })

  #Render Quarterly Price Growth  Box
  output$qoqBox <- renderValueBox({
    valueBox(
      paste0(round(getSummary()$QoQ * 100,4), "%"), "Quarter to Quarter Price Growth", icon = icon("calculator"), color = "yellow"
    )
  })
  
  #Render Annual Price Growth  Box
  output$yoyBox <- renderValueBox({
    valueBox(
      paste0(round(getSummary()$YoY * 100,4), "%"), "1 Year Price Growth", icon = icon("calendar"), color = "purple"
    )
  })
  
  #Render 5 Year Price Growth  Box
  output$fiveYearBox <- renderValueBox({
    valueBox(
      paste0(round(getSummary()$fiveYear * 100,4), "%"), "5 Year Price Growth", icon = icon("bar-chart"), color = "red"
    )
  })
  
  #Render 10 Year Price Growth  Box
  output$tenYearBox <- renderValueBox({
    valueBox(
      paste0(round(getSummary()$tenYear * 100,4), "%"), "10 Year Price Growth", icon = icon("line-chart"), color = "blue"
    )
  })
  
  # Get Time Series
  getTimeSeries <- reactive({
    if (is.null(input$zip)) {
      z <- dfltZip
    } else {
      z <- input$zip
    }
    
    market <- subset(zhvi, RegionName == z)
    prices <- as.numeric(as.vector(market[,53:244]))
    timeSeries <- ts(prices, frequency = 12, start = c(2000,1))
    
    return(timeSeries)
  })

  
  # Render time series plot
  output$tsPlot <- renderPlot({
    Price <- getTimeSeries()
    autoplot(Price, ts.colour = "blue") + theme_bw()
  })
  
  # Render non-seasonal trend time series
  output$nsPlot <- renderPlot({
    Price <- SMA(getTimeSeries(), n = input$span)
    autoplot(Price, ts.colour = "blue") + theme_bw()
  })
  
  # Render seasonal time series decomposition
  output$tsiPlot <- renderPlot({
    Price <- decompose(getTimeSeries())
    autoplot(Price, ts.colour = "blue") + theme_bw()
  })
  
  # Render model select
  output$modelsUi <- renderUI({
    selectInput("model", label = "Prediction Models:", choices = c(Choose='',as.character(modelData$code)), selected = dfltModel, selectize = FALSE)
  })

  # Render model name
  output$modelNameUi <- renderText({
    if (is.null(input$model)) {
      m <- dfltModel
    } else {
      m <- input$model
    }
    paste(modelData[ which(modelData$code == m), ]$name)
  })
  
  # Render model description
  output$modelDescUi <- renderText({
    if (is.null(input$model)) {
      m <- dfltModel
    } else {
      m <- input$model
    }
    paste(modelData[ which(modelData$code == m), ]$desc)  
  })


  # Split data into training and test/validation set
  splitData <- eventReactive(input$train, {
    if (is.null(input$split)) {
      y <- dfltSplit
    } else {
      y <- as.numeric(input$split)
    }
    
    if (is.null(input$zip)) {
      z <- dfltZip
    } else {
      z <- as.integer(input$zip)
    }
    # Create time series object on full data
    market        <- subset(zhvi, RegionName == z)
    marketPrices  <- as.numeric(as.vector(market[,53:244]))
    tSeries       <- ts(marketPrices, frequency = 12, start = c(2000,1))
    
    #Split into training and test set
    tsTest  <- window(tSeries, start = c(y+1,1))
    tsTrain <- window(tSeries, end = c(y,12))
    
    #Combine into a list
    l <- list("train" = tsTrain, "test" = tsTest)
    
    return(l)
  }, ignoreNULL = FALSE)
  

  
  # Get plot options, specifically, number of periods to forecast and to include
  getForecastOptions <- eventReactive(input$train, {
    # Determine number of periods to forecast
    if (is.null(input$split)) {
      periods <- 12
    } else {
      periods <- (2015 - as.integer(input$split)) * 12
    }
    
    # Determine number of back periods to include
    if ((periods * 3) > (192 - periods)) {
      include <- 192 - periods
    } else {
      include <- periods * 3
    }
    
    # Determine ylimit at peak price
    if (is.null(input$zip)) {
      z <- dfltZip
    } else {
      z <- input$zip
    }
    maximum <- as.integer(zhviSummary[ which(zhviSummary$RegionName == z),]$PeakZHVI)

    #Combine into a list and return
    l <- list(periods = periods, include = include, maximum = maximum)
    
  }, ignoreNULL = FALSE)
  
  
  # Prepare predictions
  trainModel <- function(d) {
    if (is.null(input$model)) {
      m <- dfltModel
    } else {
      m <- input$model
    }
    
    switch (m,
      ARIMA = auto.arima(d, ic='aicc', stepwise=FALSE),
      ETS = ets(d, ic='aicc', restrict=FALSE),
      NEURAL = nnetar(d, p=12, size=25),
      TBATS = tbats(d, ic='aicc', seasonal.periods=12),
      BATS = bats(d, ic='aicc', seasonal.periods=12),
      STLM = stlm(d, s.window=12, ic='aicc', robust=TRUE, method='ets'),
      STS = StructTS(d),
      NAIVE = naive(d, getPlotOptions()$periods)
    )
  }


  #Format Accuracy Results into a table
  formatAccuracy <- function(a) {
    r <- t(a)
    measure    <-c("Mean Error (ME)",
                   "Root Mean Squared Error (RMSE)",
                   "Mean Absolute Error (MAE)",
                   "Mean Percentage Error (MPE)",
                   "Mean Absolute Percentage Error (MAPE)",
                   "Mean Absolute Scaled Error (MASE)",
                   "Autocorrelation of errors at lag 1. (ACF1)",
                   "ThEIl's U")
    trainingSet <- r[,1]
    testSet     <- r[,2]
    results     <- data.frame(measure, trainingSet, testSet)
    names(results) <- c("Measure", "Training Set", "Test Set")
    return(results)
  }
  
  # Get training forecast and test data
  getPlotData <- eventReactive(input$train, {
    
    d <- splitData()
    m <- trainModel(d$train)
    o <- getForecastOptions()
    p <- forecast(m, o$periods)
    a <- accuracy(p, d$test)
    modelAccuracy <<- formatAccuracy(a)
    
    #Combine into a list
    l <- list("train" = d$train, "test" = d$test, "model" = m$model, 
              "periods" = o$periods, "maximum" = o$maximum, "include" = o$include,
              "prediction" = p, "results" = r)
    
    #Return list
    return(l)
  }, ignoreNULL = FALSE)
  

  # Render training forecast plot
  output$modelPlot <- renderPlot({
    
    p <- getPlotData()
    
    plot(p$prediction, include = p$include, ylim=c(0,as.numeric(p$maximum)))
    lines(p$test, col = "red")
    legend("bottomright",
           inset=.05,
           cex = 1,
           title="Legend",
           c("Actual Values","Predicted Values"),
           horiz=FALSE,
           lty=c(1,1),
           lwd=c(2,2),
           col=c("red","blue"),
           bg="white",
           text.font=3)
  })
  
  output$accuracy <- renderDataTable({modelAccuracy})
})