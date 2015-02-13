
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
                       solidHeader = TRUE,
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
                       h3(textOutput("AddCountiesHoverSelectedDestination")),
                       plotOutput(outputId = "DestPlotCounties",clickId = "DestinationCounties",hoverId="DestinationCountiesHover",hoverDelay=300),
                       uiOutput("SelectDestCounties")
                     )
              )###end second column
            )###end fluid row
    ),
    
    tabItem(tabName = "data_conditioning",
            box(title="Map of Selected Data",width=NULL,status="primary",solidHeader = TRUE,collapsible = T,
                  fluidRow(
                    column(width=2,
                           checkboxGroupInput("maplayers","Map Layers to Display",
                                       c("Selected Data","Selected Counties","State Borders","Unselected Data"),
                                       selected=c("Selected Data","State Borders"))
                           ),
                    column(width=10,
                           plotOutput(outputId = "MapSelectedData",height="800px")
                           )
                    )
              ),
            box(title="Time Series Plot: Click Data to Identify Groups for Removal",width=NULL,status="primary",solidHeader = TRUE,collapsible = T,
                fluidRow(
                  column(width=2,
                         uiOutput("RemoveCustomerCarrier")
                  ),
                  column(width=10,
                         plotOutput(outputId = "RemovalPlot",height="800px",clickId = "RemoveGroups",hoverId="RemoveGroupsHover",hoverDelay=300)
                  )
                )
            )
    )###end current tab
    
    
    
  )###end tab items
)###end body



dashboardPage(header, sidebar, body)




  
  

