
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("predictor.R")

shinyServer(function(input, output) {
  output$distPlot <- renderText({
    if(input$query != ""){
      predict(input$query)
    }
  })
})
