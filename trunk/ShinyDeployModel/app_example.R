
helpfunc <- function(mean, sd) {
  hist = hist(rnorm(1000, mean, sd))
  myLines = function(){lines(1:10)}
  myLines()
  list(hist = hist, lines = myLines)
}

mwe2 <- function() {
  
  
  app = list(
    ui = bootstrapPage(
      fluidPage(
        sidebarPanel(
          sliderInput("mean", "choose mean", -10, 10, 1),
          sliderInput("sd", "choose sd", 0, 5, 1)),
        mainPanel(
          plotOutput("hist"),
          downloadButton("histDownload")
          
        )
      )
    ),
    server = function(input, output) {
      browser()
      output$hist <- renderPlot(.hist())
      
      .hist <- reactive(helpfunc(input$mean, input$sd))
      
      output$histDownload <- downloadHandler(
        filename = function() {
          paste("hist.jpg")
        }, 
        content = function(file) {
          myHist <- .hist()
          jpeg(file, quality = 100, width = 800, height = 800)
          plot(myHist$hist)
          myHist$lines()
          dev.off()
        }
      )
      
    }
    
  )
  runApp(app)
}


shareimprove this answer


answered Jun 11 '14 at 12:27 




jdharrison
12.3k1929 






add a comment  
