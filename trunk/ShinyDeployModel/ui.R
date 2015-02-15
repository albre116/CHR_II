
header <- dashboardHeader(
  title="CPDS Time Series"
  
  )###end header

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Select Geography", tabName = "geography", icon = icon("fa fa-globe")),
    menuItem("Data Conditioning",icon = icon("fa fa-line-chart"),
             menuSubItem("Map of Selected Data","MapSelected"),
             menuSubItem("Date Range & Outliers","DateRange")),
    menuItem("Model Fitting", tabName = "modeling", icon = icon("fa fa-cog"))
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
                       uiOutput("SelectOrigStates"),
                       checkboxGroupInput("maplayersOrigStates","Map Layers to Display",
                                          c("State Names","Data"),
                                          selected=c("State Names"),inline=TRUE)
                     ),
                     box(
                       title="Origin Counties: Click To Select or Enter/Delete In List",width=NULL,status="primary",
                       solidHeader = TRUE,
                       h3(textOutput("AddCountiesHoverSelectedOrigin")),
                       plotOutput(outputId = "OrigPlotCounties",clickId = "OriginCounties",hoverId="OriginCountiesHover",hoverDelay=300),
                       uiOutput("SelectOrigCounties"),
                       checkboxGroupInput("maplayersOrigCounties","Map Layers to Display",
                                          c("Data"),
                                          selected=NULL,inline=TRUE)
                        )
                     ),###end first column
              column(width=6,
                     box(
                       title="Destination States: Click To Select or Enter/Delete In List",width=NULL,status="primary",
                       solidHeader = TRUE,
                       h3(textOutput("AddStatesHoverSelectedDestination")),
                       plotOutput(outputId = "DestPlotState",clickId = "DestinationStates",hoverId="DestinationStatesHover",hoverDelay=300),
                       uiOutput("SelectDestStates"),
                       checkboxGroupInput("maplayersDestStates","Map Layers to Display",
                                          c("State Names","Data"),
                                          selected=c("State Names"),inline=TRUE)
                     ),
                     box(
                       title="Destination Counties: Click To Select or Enter/Delete In List",width=NULL,status="primary",
                       solidHeader = TRUE,
                       h3(textOutput("AddCountiesHoverSelectedDestination")),
                       plotOutput(outputId = "DestPlotCounties",clickId = "DestinationCounties",hoverId="DestinationCountiesHover",hoverDelay=300),
                       uiOutput("SelectDestCounties"),
                       checkboxGroupInput("maplayersDestCounties","Map Layers to Display",
                                          c("Data"),
                                          selected=NULL,inline=TRUE)
                     )
              )###end second column
            )###end fluid row
    ),
    
    tabItem(tabName = "MapSelected",
            box(title="Map of Selected Data",width=NULL,status="primary",solidHeader = TRUE,collapsible = F,
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
              )
    ),
    tabItem(tabName = "DateRange",
            box(title="Select Date Range For Analysis By Dragging on Screen",width=NULL,status="primary",solidHeader = TRUE,collapsible = T,
                fluidRow(
                  column(width=2,
                         sliderInput("lowerTau","Lower Percentile",min=0,max=1,value=0.10),
                         sliderInput("centralTau","Center Percentile",min=0,max=1,value=0.5),
                         sliderInput("upperTau","Upper Percentile",min=0,max=1,value=0.95),
                         numericInput("dfspline","Spline df",value=20,min=1,max=50,step=1),
                         actionButton("applyDygraph","Apply Date & Percentile Selections",icon=icon("fa fa-refresh"))
                  ),
                  column(width=10,
                         dygraphOutput("dygraph",height="400px")
                  )
                )
            ),
            box(title="Clean Up Fixed Rate Contracts: Click Data to Identify Groups for Removal",width=NULL,status="primary",solidHeader = TRUE,collapsible = T,
                fluidRow(
                  column(width=2,
                         h3(textOutput("RemoveCustomerCarrierHover")),
                         uiOutput("UpperLower"),
                         actionButton("applyUpperLower","Apply Rate Per Mile Filter",icon=icon("fa fa-refresh")),
                         checkboxGroupInput("QuantileFilter","Apply Quantile Filter",c("Lower Quantile","Upper Quantile")),
                         checkboxGroupInput("TypeRemoval","Point Click Removal Method",c("Customer Carrier","Individual Load"),selected = c("Customer Carrier")),
                         uiOutput("RemoveCustomerCarrier"),
                         uiOutput("RemoveIndividual"),
                         checkboxGroupInput("plotControls","Plot Layers to Display",
                                            c("Percentiles","Kept","Removed"),
                                            selected=c("Kept","Removed","Percentiles"))
                  ),
                  column(width=10,
                         plotOutput(outputId = "RemovalPlot",height="800px",clickId = "RemoveGroups",hoverId="RemoveGroupsHover",hoverDelay=300)
                  )
                )
            )
    ),###end current tab
    tabItem(tabName = "modeling",
            box(title="Model Selection and Options",width=NULL,status="primary",solidHeader = TRUE,collapsible = T,
                fluidRow(
                  column(width=2,
                         selectInput("ModelFamily","Modeling Kernel",c("Generalized Additive Model"),selected=c("Generalized Additive Model")),
                         actionButton("FitModel","Update Model")
                  ),
                  column(width=10,
                         checkboxGroupInput("BaseModelParameters","Base Model Parameters",
                                            c("Stop Count","Seasonality","Inflation"),
                                            selected=c("Stop Count","Seasonality","Inflation"))
                  )
                )
            ),
            box(title="Model Summary",width=NULL,status="primary",solidHeader = TRUE,collapsible = T,
                fluidRow(
                  column(width=2,
                        h3("stuff here")
                  ),
                  column(width=10,
                         div(class="span7", verbatimTextOutput("ModelSummary"))
                  )
                )
            )
    )###end current tab
    
    
    
  )###end tab items
)###end body



dashboardPage(header, sidebar, body)




  
  

