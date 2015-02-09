

# Define UI for miles per gallon application
shinyUI(fluidPage(
  fluidRow(column(3,
           h2("What Ever stuff goes here")
           
           ),###end left hand nav bar
           column(9, 
                  h2("Add Map"),
                  plotOutput(outputId = "OrigPlotAdd", "100%", "500px",  clickId = "AddOrigin"),
                  h2("Remove Map"),
                  plotOutput(outputId = "OrigPlotDelete", "100%", "500px", clickId = "DeleteOrigin")
           )
  )
)
)

  
  
  
#   h2("Central Maine Power Live Outage Map"),
#   htmlOutput('ts'),
#   plotOutput(outputId = "cmpPlot", height="650px",clickId = "MapClick"),
#   plotOutput(outputId = "cmpPlot2", height="650px",clickId = "MapClick2"),
#   dataTableOutput('click_behavior'),
#   dataTableOutput('click_behavior2'),
#   dataTableOutput('details'),
#   HTML("<hr noshade size=1><center>Made by <a href='http://twitter.com/hrbrmstr'>@hrbrmstr</a>; Source at: <a href='https://gist.github.com/hrbrmstr/7681842'>github</a></center>")
#   
# ))
