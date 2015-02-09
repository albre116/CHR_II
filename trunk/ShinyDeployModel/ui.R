

# Define UI for miles per gallon application
shinyUI(fluidPage(
  fluidRow(3,
           
           
           
           ),###end left hand nav bar
  fluidRow(9, navbarPage(title = "Version 1.0",id = "navbar1",
                         tabPanel("Dataset Selection", value = "panel1",
                                  h2("Add Map"),
                                  plotOutput(outputId = "OrigPlotAdd", height="650px",clickId = "AddOrigin"),
                                  h2("Remove Map"),
                                  plotOutput(outputId = "OrigPlotDelete", height="650px",clickId = "DeleteOrigin")
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  ),### end tab panel1
                          )###end navbar 1
           ) ###end right hand main panel
))
  
  
  
  
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
