
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
                       title="Origin States: Click To Select or Enter/Delete In List",width=NULL,status="primary",
                       solidHeader = TRUE,
                       h3(textOutput("AddStatesHoverSelectedOrigin")),
                       plotOutput(outputId = "OrigPlotState",clickId = "OriginStates",hoverId="OriginStatesHover",hoverDelay=300),
                       uiOutput("SelectOrigStates")
                        ),
                     box(
                       title="Origin Counties: Click To Select or Enter/Delete In List",width=NULL,status="primary",
                       h3(textOutput("AddCountiesHoverSelectedOrigin")),
                       plotOutput(outputId = "OrigPlotCounties",clickId = "OriginCounties",hoverId="OriginCountiesHover",hoverDelay=300),
                       uiOutput("SelectOrigCounties")
                        )
                     ),###end first column
              column(width=6,
                     box(
                       title="Destination States: Click To Select or Enter/Delete In List",width=NULL,status="primary",
                       solidHeader = TRUE,
                       h3(textOutput("AddStatesHoverSelectedDestination")),
                       plotOutput(outputId = "DestPlotState",clickId = "DestinationStates",hoverId="DestinationStatesHover",hoverDelay=300),
                       uiOutput("SelectDestStates")
                     ),
                     box(
                       title="Destination Counties: Click To Select or Enter/Delete In List",width=NULL,status="primary",
                       solidHeader = TRUE,
                       h2("add county select map")
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

  
  
  

