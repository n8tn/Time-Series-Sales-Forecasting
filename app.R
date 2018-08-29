library(shiny)
library(shinythemes)
library(ggplot2)
library(tseries)
library(forecast)
library(zoo)
library(strucchange)
library(rsconnect)


# Define UI 
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  # Application title
  titlePanel("Time-series Forecasting"),
  
  # Sidebar with controls to select ppv and forecast ahead duration
  sidebarLayout(
    sidebarPanel(
      sliderInput("ppv", "Price per value:",
                  min = 0, max = 2.03,
                  value = 2.03, step = 0.1),
      numericInput("ahead", "Months to Forecast Ahead:", 6),
      
      submitButton("Update View")
      ),
  
  # Show the forecast plot
    mainPanel(
      plotOutput("arimaForecastPlot"),
      tableOutput("view")
    )
  
  
   )
  
  
)

library(datasets)
library(forecast)
library(DT)
library(ggplot2)
library(zoo)

server <- function(input, output) {
  
  output$arimaForecastPlot <- renderPlot({
    ARIMAautofit <- auto.arima(log(Sales[,1]), d = 2, xreg = log(Sales[,3])) 
    ppv <- log(rep.int(input$ppv, input$ahead))
    fc <- forecast(ARIMAautofit, n = input$ahead, xreg = ppv, lambda = 0)
    fc$x <- exp(fc$x)
    Forecast <- as.numeric(fc$mean)
    Date <- rownames(as.data.frame(fc))
    Forecasts <- data.frame(Date, Forecast)
    Forecasts$Forecast <- as.integer(round(Forecasts$Forecast))
    Forecasts$Forecast <- format(Forecasts$Forecast, scientific=FALSE, big.mark = ",")
    colnames(Forecasts)[2] <- "Demand Forecast for Cans"
    output$view <- renderTable({
      Forecasts
    })
    
    options(scipen = 999)
    plot(fc, main = "Forecast from Sales Time-Series Model", ylab = "Cans")
  })
  

}
shinyApp(ui = ui, server = server)
