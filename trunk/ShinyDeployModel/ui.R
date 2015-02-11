
header <- dashboardHeader(
  title="CPDS Time Series"
  
  )###end header

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Select Geography", tabName = "geography", icon = icon("fa fa-globe")),
    menuItem("Data Conditioning", tabName = "data_conditioning", icon = icon("fa fa-line-chart"))
  )
  
)###end side bar

body <- dashboardBody(

  tabItems(
    tabItem(tabName = "geography",
            fluidRow(
              column(width=6,
                     box(
                       title="Add Origin State",width=NULL,status="primary",
                       solidHeader = TRUE,
                       plotOutput(outputId = "OrigPlotAdd",clickId = "AddOrigin")
                        ),
                     box(
                       title="Remove Origin State",width=NULL,status="primary",
                       solidHeader = TRUE,
                       plotOutput(outputId = "OrigPlotDelete", clickId = "DeleteOrigin")
                        )
                     ),###end first column
              column(width=6,
                     box(
                       title="Add Destination State",width=NULL,status="primary",
                       solidHeader = TRUE,
                       plotOutput(outputId = "DestPlotAdd",clickId = "AddDest")
                     ),
                     box(
                       title="Remove Destination State",width=NULL,status="primary",
                       solidHeader = TRUE,
                       plotOutput(outputId = "DestPlotDelete", clickId = "DeleteDest")
                     )
              )###end second column
            )
    ),
    
    tabItem(tabName = "data_conditioning",
            h2("Data Selection")
    )
  )
)###end body



dashboardPage(header, sidebar, body)





# shinyUI(fluidPage(
#   fluidRow(column(3,
#            h2("What Ever stuff goes here")
#            
#            ),###end left hand nav bar
#            column(4.5, 
#                   bsCollapse(multiple = T,open="state",id="map_selector",
#                              bsCollapsePanel("States",
#                                              h2("Add Map"),
#                                              plotOutput(outputId = "OrigPlotAdd",clickId = "AddOrigin"),
#                                              h2("Remove Map"),
#                                              plotOutput(outputId = "OrigPlotDelete", clickId = "DeleteOrigin"),
#                                              id="state",value="test1"
#                                              )
#                   )
#            ),
#            column(4.5, h2("dest goes here")
#            )
#   )
# ))

  
  
  

