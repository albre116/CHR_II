
header <- dashboardHeader(
  title="CPDS Time Series"
  
  )###end header

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Select Geography", tabName = "geography", icon = icon("fa fa-globe"),
             badgeLabel = "Step 1",badgeColor = "red"),
    menuItem("Model & Volume", tabName = "VolumeEntry", icon = icon("fa fa-car"),
             badgeLabel = "Step 2",badgeColor = "red"),
    menuItem("Model Summary", tabName = "SummaryPredictions", icon = icon("fa fa-cog"),
             badgeLabel = "Step 3",badgeColor = "red"),
    menuItem("Advanced Options",icon = icon("fa fa-line-chart"),
             menuSubItem("Map of Selected Data","MapSelected"),
             menuSubItem("Date Range & Outliers","DateRange"),
             menuSubItem("Model Diagnostics","modeling"),
             menuSubItem("Partial Effects","partial"),
             menuSubItem("Marginal Effects","marginal"),
             menuSubItem("Adjusted Predictions","prediction"),
             menuSubItem("Session Info 1","sessionInfo"),
             menuSubItem("Session Info 2","sessionInfo2")
             )),
  uiOutput("response")
)###end side bar

body <- dashboardBody(

  tabItems(
    tabItem(tabName = "geography",
            fluidRow(
              column(width=6,
                     box(
                       title="Origin Zips & Cities: Select to Include",width=NULL,status="primary",
                       solidHeader = TRUE,collapsible = T,
                       uiOutput("OrigZip3"),
                       uiOutput("OrigCity")
                     )#end box
              ),#end column
              column(width=6,
                     box(
                       title="Destination Zips & Cities: Select to Include",width=NULL,status="primary",
                       solidHeader = TRUE,collapsible = T,
                       uiOutput("DestZip3"),
                       uiOutput("DestCity")
                     )#end box
              )#end column
            ),#end fluid row
            fluidRow(
              column(width=6,
                     box(
                       title="Origin States: Click To Select or Enter/Delete In List",width=NULL,status="primary",
                       solidHeader = TRUE,collapsible = T,
                       h3(textOutput("AddStatesHoverSelectedOrigin")),
                       plotOutput(outputId = "OrigPlotState",clickId = "OriginStates",hoverId="OriginStatesHover",hoverDelay=300),
                       uiOutput("SelectOrigStates"),
                       checkboxGroupInput("maplayersOrigStates","Map Layers to Display",
                                          c("State Names","Data"),
                                          selected=c("State Names"),inline=TRUE)
                     ),
                     box(
                       title="Origin Counties: Click To Select or Enter/Delete In List",width=NULL,status="primary",
                       solidHeader = TRUE,collapsible = T,
                       h3(textOutput("AddCountiesHoverSelectedOrigin")),
                       plotOutput(outputId = "OrigPlotCounties",clickId = "OriginCounties",hoverId="OriginCountiesHover",hoverDelay=300),
                       uiOutput("SelectOrigCounties"),
                       uiOutput("SelectOrigCircles"),
                       fluidRow(column(width=6,checkboxGroupInput("maplayersOrigCounties","Map Layers to Display",
                                          c("Data"),
                                          selected=NULL,inline=T)
                                       ),
                                column(width=6,
                                       selectInput("OrigCircle","Select Method:",
                                                          c("Counties","Bounding Circle"),selected="Counties"),
                                       numericInput("CircleRadiusOrig","Radius of Circle (mi)", value=50,min=0,step=25)
                                       )
                                )
                        )
                     ),###end first column
              column(width=6,
                     box(
                       title="Destination States: Click To Select or Enter/Delete In List",width=NULL,status="primary",
                       solidHeader = TRUE,collapsible = T,
                       h3(textOutput("AddStatesHoverSelectedDestination")),
                       plotOutput(outputId = "DestPlotState",clickId = "DestinationStates",hoverId="DestinationStatesHover",hoverDelay=300),
                       uiOutput("SelectDestStates"),
                       checkboxGroupInput("maplayersDestStates","Map Layers to Display",
                                          c("State Names","Data"),
                                          selected=c("State Names"),inline=TRUE)
                     ),
                     box(
                       title="Destination Counties: Click To Select or Enter/Delete In List",width=NULL,status="primary",
                       solidHeader = TRUE,collapsible = T,
                       h3(textOutput("AddCountiesHoverSelectedDestination")),
                       plotOutput(outputId = "DestPlotCounties",clickId = "DestinationCounties",hoverId="DestinationCountiesHover",hoverDelay=300),
                       uiOutput("SelectDestCounties"),
                       uiOutput("SelectDestCircles"),
                       fluidRow(column(width=6,checkboxGroupInput("maplayersDestCounties","Map Layers to Display",
                                                                  c("Data"),
                                                                  selected=NULL,inline=T)
                       ),
                       column(width=6,
                              selectInput("DestCircle","Select Method:",
                                          c("Counties","Bounding Circle"),selected="Counties"),
                              numericInput("CircleRadiusDest","Radius of Circle (mi)", value=50,min=0,step=25)
                       )
                       )
                     )
              )###end second column
            )###end fluid row
    ),
    tabItem(tabName = "VolumeEntry",
            fluidRow(
              column(width=6,
                     box(
                       title="Prediction Model Parameters",width=NULL,status="primary",solidHeader = TRUE,collapsible = T,
                       fluidRow(
                         column(width=6,
                                selectInput("ModelFamily","Modeling Kernel",c("Generalized Additive Model"),selected=c("Generalized Additive Model")),
                                checkboxInput("FilterDate","Perform Date and Observation Filtering?",value=F),
                                uiOutput("DateRange"),
                                sliderInput("ConfLimits","Model Confidince Intervals",0,1,c(0.15,0.85)),
                                uiOutput("PredictionLevels")
                         ),
                         column(width=6,
                                uiOutput("FactorTerms"),
                                uiOutput("LinearTerms"),
                                uiOutput("SplineTerms"),
                                uiOutput("SplineTermsCyclic")
                         )
                       )

                     ),
                     tabBox(title="Model Fit & Predictions",width=NULL,id="predicitonPlot",
                            tabPanel(title="Prediction Plot",value="predictionPlot",
                                     dygraphOutput("PredictionPlotInteractive")
                                     ),
                            tabPanel(title="Table of Predictions",value="predictionTable",
                                     dataTableOutput("PredicitonTable")
                                     )
                     )
              ),###end first column
              column(width=6,
                     box(
                       title="Customer Volume Pattern",width=NULL,status="primary",solidHeader = TRUE,
                       dygraphOutput("VolumeIntegrated")

                     ),
                     box(
                       title="Draw Desired Volume Profile",width=NULL,status="primary",solidHeader = TRUE,
                       fluidRow(
                         column(width=2,
                                uiOutput("fourierComp")
                         ),
                         column(width=10,
                                dyPencilgraphOutput("VolumeDraw")
                         )
                       )
                     )
              )###end second column
            )###end fluid row
    ),
    
    tabItem(tabName = "SummaryPredictions",
            box(title="HIstorical Integrated",width=NULL,status="primary",solidHeader = TRUE,collapsible = T,
                fluidRow(
                  column(width=12,
                         #dygraphOutput("Historical")
                         h2("turned off debugging")
                  )
                )
            ),
            box(title="Yearly Average",width=NULL,status="primary",solidHeader = TRUE,collapsible = T,
                fluidRow(
                  column(width=6,
                         #dygraphOutput("HistVolIntegrated")
                         h2("turned off debugging")
                  ),
                  column(width=6,
                         #dataTableOutput("HistVolIntegratedTable")
                         h2("turned off debugging")
                  )
                )
            )
    ),###end current tab
    
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
                         sliderInput("lowerTau","Lower Percentile",min=0,max=1,value=0.05),
                         sliderInput("centralTau","Center Percentile",min=0,max=1,value=0.5),
                         sliderInput("upperTau","Upper Percentile",min=0,max=1,value=0.95),
                         checkboxInput("doEstimation","Optimize Spline Fit",FALSE),
                         sliderInput("dfspline","Spline df Penalty Range",min=1,max=50,value=c(1,20)),
                         sliderInput("LambdaFixed","Fixed df Penalty",min=1,max=50,value=c(10)),
                         actionButton("applyDygraph","Apply Date & Percentile Selections",icon=icon("fa fa-refresh"))
                  ),
                  column(width=10,
                         dygraphOutput("dygraph_cut",height="400px")
                  )
                )
            ),
            box(title="Clean Up Fixed Rate Contracts: Click Data to Identify Groups for Removal",width=NULL,status="primary",solidHeader = TRUE,collapsible = T,
                fluidRow(
                  column(width=2,
                         h3(textOutput("RemoveCustomerCarrierHover")),
                         uiOutput("UpperLower"),
                         actionButton("applyUpperLower","Apply Rate Per Mile Filter",icon=icon("fa fa-refresh")),
                         checkboxGroupInput("QuantileFilter","Apply Quantile Filter",c("Lower Quantile","Upper Quantile"),c("Lower Quantile","Upper Quantile")),
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
                h2("stuff")
            ),
            tabBox(title="Model Fit Diagnostics",width=NULL,id="diagnostics",
                tabPanel(title="Partial Effects",value="partial",
                         fluidRow(
                           column(width=2,
                         h3("stuff here")
                         ),
                         column(width=10,
                                #plotOutput("ModelPlot",height="600px")
                                h2("turned off debugging")
                                )
                         )
                         ),
                tabPanel(title="Model Diagnostics",value="diagnostics",
                         fluidRow(
                           column(width=2,
                                  h3("stuff here")
                           ),
                           column(width=10,
                                  #plotOutput("ModelDiagnostics",height="600px")
                                  h2("turned off debugging")
                           )
                         )
                ),
                tabPanel(title="Model Summary",value="summary",
                         fluidRow(
                           column(width=2,
                                  h3("stuff here")
                                  ),
                           column(width=10,
                                  #div(class="span7", verbatimTextOutput("ModelSummary"))
                                  h2("turned off debugging")
                                  )
                           )
                         )
                )
            ),###end current tab
    tabItem(tabName = "partial",
            box(title="Partial Effects",width=NULL,status="primary",solidHeader = TRUE,collapsible = F,
                         fluidRow(
                           column(width=2,
                                  #uiOutput("PartialEffect"),
                                  #uiOutput("NusianceEffect"),
                                  #uiOutput("NusianceLevels"),
                                  #checkboxGroupInput("PartialSeries","Select Layers",
                                  #                   c("Fitted","Observed"),
                                  #                   selected=c("Fitted","Observed"))
                                  h2("turned off debugging")
                           ),
                           column(width=10,
                                  #plotOutput("PartialPlot")
                                  h2("turned off debugging")
                           )
                           )
                )
    ),###end current tab
    tabItem(tabName = "marginal",
            box(title="Marginal Model Effects",width=NULL,status="primary",solidHeader = TRUE,collapsible = F,
                            fluidRow(
                              column(width=2,
                                     #uiOutput("MarginalEffect")
                                     h2("turned off debugging")
                              ),
                              column(width=10,
                                    #plotOutput("MarginalPlot")
                                    h2("turned off debugging")
                              )
                              )
                )
    ),###end current tab
    tabItem(tabName = "prediction",
            box(title="Model Fit To Raw Data",width=NULL,status="primary",solidHeader = TRUE,collapsible = T,
                fluidRow(
                  column(width=2,
                         #checkboxGroupInput("PredictionPartial","Select Layers",
                          #                  c("Fitted","Observed","Predicted"),
                          #                  selected=c("Fitted","Observed","Predicted"))
                         h2("turned off debugging")
                  ),
                  column(width=10,
                         #plotOutput("PredictionFullPlot")
                         h2("turned off debugging")
                  )
                )
            )
            
            ),###end current tab
    tabItem(tabName = "sessionInfo",
            box(title="Session Info",width=NULL,status="primary",solidHeader = TRUE,collapsible = T,
                div(class="span7", verbatimTextOutput("session"))
            )###end box
    ),###end current tab
    tabItem(tabName = "sessionInfo2",
            box(title="Session Info 2",width=NULL,status="primary",solidHeader = TRUE,collapsible = T,
                div(class="span7", verbatimTextOutput("session2"))
            )###end box
    )###end current tab
  )###end tab items
)# ##end body



dashboardPage(header, sidebar, body)




  
  

