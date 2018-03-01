#FOR THE GRADER.  
#please note it is not necessary to have 2 files (ui and server)
#now it is possible to have all the code in one script.
#please look at the server function below
#I followed the format of an rstudio webinar

library(shiny)
ui <- fluidPage(
  headerPanel("FANG STOCKS.  Historical Adjusted Price and Volume"),
  
  radioButtons("FANG", "Select One of the Stocks",
               c("Facebook" = "FB",
                 "Apple" = "AAPL",
                 "Google" = "GOOG",
                 "Amazon" = "AMZ")),
  plotOutput("linePlot"),
  h3("This App graphs the historical volume and adjusted prices for the FANG stocks"),
  h3("The red line corresponds to closing prices at a given time"),
  h3("the blue line is a smooth average of prices")
  
)

server <- function(input, output) {
  output$linePlot <- renderPlot({
    library(ggplot2)
    library(gridExtra)
    library(readr)
    library(tidyquant)
    stock <- input$FANG
    
    from <- today() - years(4)
    dat <- tq_get(stock, get = "stock.prices", from = from)
    dat1 <- dat[,c(1,6,7)]
    
    prices <- dat1 %>% ggplot(aes(x = date, y = adjusted)) + geom_line(color = "red") + geom_smooth(method = "loess") +
      ylab("Adjusted Price") + xlab("") + theme_classic()
    
    volume <- dat1 %>% ggplot() + geom_col(aes(x = date, y = volume)) + theme_classic()
    
    grid.arrange(prices, volume)
  })
}

shinyApp(ui, server)