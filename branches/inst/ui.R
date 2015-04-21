
header <- dashboardHeader(
  title="CPDS Time Series"
  )###end header

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Load Model and Data?",tabName = "image",icon = icon("fa fa-file")),
    menuItem("Select Geography", tabName = "geography", icon = icon("fa fa-globe"),
             badgeLabel = "Start Here",badgeColor = "red"),
    menuItem("Basic Quote", tabName = "basicquote", icon = icon("fa fa-camera"),
             badgeLabel = "Market Average",badgeColor = "red"),
    menuItem("Customer Specific Quote",icon = icon("fa fa-bullseye"),
             menuSubItem("1: Select Predictors", tabName = "predictors", icon = icon("fa fa-bar-chart")),
             menuSubItem("2: Model & Volume", tabName = "VolumeEntry", icon = icon("fa fa-car")),
             menuSubItem("3: Model Summary", tabName = "SummaryPredictions", icon = icon("fa fa-cog"))),
    menuItem("Advanced Options",icon = icon("fa fa-line-chart"),
             menuSubItem("Map of Selected Data","MapSelected"),
             menuSubItem("Date Range & Outliers","DateRange"),
             menuSubItem("Model Diagnostics","modeling"),
             menuSubItem("Partial Effects","partial"),
             menuSubItem("Marginal Effects","marginal"),
             menuSubItem("Adjusted Predictions","prediction"),
             menuSubItem("Session Info 1","sessionInfo"),
             menuSubItem("Session Info 2","sessionInfo2")
             ),
   menuItem("Save Model Image?",tabName = "saveimage",icon = icon("fa fa-file"))
),
  box(title="Major Modeling Options",width=NULL,status = "warning",solidHeader = TRUE,
  uiOutput("response"),
  checkboxInput("FilterDate","Perform Date and Observation Filtering?",value=TRUE),
  uiOutput("DateRange"))
)###end side bar

body <- dashboardBody(
  singleton(
    tags$head(tags$script(src = "message-handler.js"))
  ),
  tabItems(
    tabItem(tabName = "image",
                fluidRow(
                  column(width=9,
                         box(width=NULL,background = NULL,status="primary",
                             withTags({
                               div(class="header", checked=NA,
                                   h2("Welcome to CPDS Time Series Modeling Applicaiton"),
                                   h2("First upload your data with the data upload botton on the Right"),
                                   h3("To use the tool you can take 1 of 2 paths:"),
                                   ul(ol(
                                     h3(li("You Can Generate a New Quote by Clicking on the \"Select Geography\" Tab")),
                                     h3(li("Or You Can Load a Model Image by Clicking the Upload Button to 
                                           the Right and then Clicking on the \"Select Geography\" Tab"))
                                     )),
                                   h3("After Selecting a Geography that Defines Your Data, 
                                      You Have Two Modeling Options:"),
                                   ul(ol(
                                     h3(li("You Can Get A Market Average by Clicking the \"Basic Quote\" Tab")),
                                     h3(li("Or You Can Enter Customer Specific Factors in the Model by Clicking
                                           the \"Customer Specific Quote\" Tab"))
                                     )),
                                   h3("Advanced Modeling Options Controlling Data Filtering Can be Found Under
                                      the \"Advanced Options\" Tab")
                               )
                             })
                         )
                         ),###end column
                  column(width=3,
                         box(title="Upload Data File",width=NULL,status="primary",solidHeader = TRUE,collapsible = F,
                             fileInput('rawdata', 'Choose RData File (If on Shiny Server No Need)',
                                       accept=c('.RData'))
                         ),##end box
                         box(title="Upload Model Image",width=NULL,status="primary",solidHeader = TRUE,collapsible = F,
                             fileInput('modelimage', 'Choose RData File',
                                       accept=c('.RData'))
                         ),##end box
                         box(title="Get Raw Data File From Server",width=NULL,status="primary",solidHeader = TRUE,collapsible = F,
                             downloadButton('downloadRawData','Get Raw Data File From Server?')
                         )##end box
                  )###end column
            )###end row
    ),###end current tab
    tabItem(tabName = "geography",
            fluidRow(
              column(width=6,
                     box(
                       title="Origin Zips & Cities: Select to Include",width=NULL,status="primary",
                       solidHeader = TRUE,collapsible = F,
                       uiOutput("OrigZip3"),
                       uiOutput("OrigCity"),
                       numericInput("OrigRadius","Miles Around Origin City to Include",value=50,min=0,step=1)
                     )#end box
              ),#end column
              column(width=6,
                     box(
                       title="Destination Zips & Cities: Select to Include",width=NULL,status="primary",
                       solidHeader = TRUE,collapsible = F,
                       uiOutput("DestZip3"),
                       uiOutput("DestCity"),
                       numericInput("DestRadius","Miles Around Destination City to Include",value=50,min=0,step=1)
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
                                          selected=c("State Names","Data"),inline=TRUE)
                     ),
                     box(
                       title="Origin Counties: Click To Select or Enter/Delete In List",width=NULL,status="primary",
                       solidHeader = TRUE,collapsible = F,
                       h3(textOutput("AddCountiesHoverSelectedOrigin")),
                       plotOutput(outputId = "OrigPlotCounties",clickId = "OriginCounties",hoverId="OriginCountiesHover",hoverDelay=300),
                       uiOutput("SelectOrigCounties"),
                       uiOutput("SelectOrigCircles"),
                       fluidRow(column(width=6,checkboxGroupInput("maplayersOrigCounties","Map Layers to Display",
                                                                      c("Data"),selected=c("Data"),inline=T)
                                       ),
                                column(width=6,
                                       selectInput("OrigCircle","Select Method:",
                                                   c("Counties","Bounding Circle"),selected=c("Counties")),
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
                                          selected=c("State Names","Data"),inline=TRUE)
                     ),
                     box(
                       title="Destination Counties: Click To Select or Enter/Delete In List",width=NULL,status="primary",
                       solidHeader = TRUE,collapsible = F,
                       h3(textOutput("AddCountiesHoverSelectedDestination")),
                       plotOutput(outputId = "DestPlotCounties",clickId = "DestinationCounties",hoverId="DestinationCountiesHover",hoverDelay=300),
                       uiOutput("SelectDestCounties"),
                       uiOutput("SelectDestCircles"),
                       fluidRow(column(width=6,checkboxGroupInput("maplayersDestCounties","Map Layers to Display",
                                                                      c("Data"),selected=c("Data"),inline=T)
                       ),
                       column(width=6,
                              selectInput("DestCircle","Select Method:",
                                          c("Counties","Bounding Circle"),selected=c("Counties")),
                              numericInput("CircleRadiusDest","Radius of Circle (mi)", value=50,min=0,step=25)
                       )
                       )
                     )
              )###end second column
            )###end fluid row
    ),
    tabItem(tabName = "basicquote",
            fluidRow(
              column(width=6,
                     valueBoxOutput("mile15_QUICK"),
                     valueBoxOutput("mile50_QUICK"),
                     valueBoxOutput("mile85_QUICK")
              ),###end column
              column(width=6,
                     plotOutput("DataCoverage_QUICK",height=100)
                     )
              ),###end row
            box(title="Historical Integrated: Market Average",width=NULL,status="primary",solidHeader = TRUE,collapsible = F,
                fluidRow(
                  column(width=12,
                         dygraphOutput("Historical_QUICK")
                  )
                )
            ),
            box(title="Volume Integrated Yearly Average: Market Average",width=NULL,status="primary",solidHeader = TRUE,collapsible = F,
                fluidRow(
                  column(width=6,
                         dygraphOutput("HistVolIntegrated_QUICK")
                  ),
                  column(width=6,
                         div(style = 'overflow-x: scroll',DT::dataTableOutput("HistVolIntegratedTable_QUICK"))
                  )
                )
            )
    ),###end current tab
    
    tabItem(tabName = "predictors",
            fluidRow(
              column(width=4,
                     box(
                       title="Model Terms and Parameters",width=NULL,status="primary",solidHeader = TRUE,collapsible = F,
                       fluidRow(
                         column(width=6,
                                uiOutput("ModelFamily"),
                                uiOutput("ConfLimits")
                         ),
                         column(width=6,
                                uiOutput("FactorTerms"),
                                uiOutput("LinearTerms"),
                                uiOutput("SplineTerms"),
                                uiOutput("SplineTermsCyclic")
                         )
                       )
                       
                     )              
              ),###end first column
              column(width=8,
                     box(
                       title="Predictor Ranges For Graph Display Below & Percentiles",width=NULL,status="primary",solidHeader = TRUE,
                       fluidRow(
                         column(width=2,
                                uiOutput("DataType")
                         ),
                         column(width=2,
                                uiOutput("PredicitonRangesLower")
                                ),
                         column(width=2,
                                uiOutput("PredicitonRangesUpper")
                                ),
                         column(width=3,
                                uiOutput("PredicitonPercentiles")
                         ),
                         column(width=3,
                                uiOutput("PredictorEntryFixed")
                         )
                       )###end row
                     )
              )###end second column
            ),###end fluid row
            box(title="Predictors to Set Value for Forecast",width=NULL,status="primary",solidHeader = TRUE,collapsible = F,
                fluidRow(
                  column(width=6,
                         uiOutput("PredictionHistorical")
                  ),###end column
                  column(width=6,
                         uiOutput("PredictionLevels")
                  )###end column
                )###end fluid row
            )###end box
    ),
    tabItem(tabName = "VolumeEntry",
            fluidRow(
              column(width=6,
                     box(title="Model Fit & Predictions",width=NULL,status="primary",solidHeader = TRUE,collapsible = F,
                       dygraphOutput("PredictionPlotInteractive")
                     )
                     ),###end first column
              column(width=6,
                     box(title="Customer Volume Pattern",width=NULL,status="primary",solidHeader = TRUE,
                       dygraphOutput("VolumeIntegrated")
                       
                     )
              )###end second column
            ),###end fluid row
            fluidRow(
              column(width=6,
                     box(title="Table of Predictions",width=NULL,status="primary",solidHeader = TRUE,collapsible = F,
                         div(style = 'overflow-x: scroll',DT::dataTableOutput("PredicitonTable"))
                     )
              ),###end first column
              column(width=6,
                     box(
                       title="Draw Desired Volume Profile",width=NULL,status="primary",solidHeader = TRUE,
                       fluidRow(
                         column(width=2,
                                uiOutput("CustomerSelect"),
                                uiOutput("CarrierSelect"),
                                uiOutput("volbasis"),
                                uiOutput("volmethod")
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
            fluidRow(
              column(width=6,
                     valueBoxOutput("mile15"),
                     valueBoxOutput("mile50"),
                     valueBoxOutput("mile85")
              ),###end column
              column(width=6,
                     plotOutput("DataCoverage",height=100)
              )
            ),###end row
            box(title="Historical Integrated: Customer Specific",width=NULL,status="primary",solidHeader = TRUE,collapsible = F,
                fluidRow(
                  column(width=12,
                         dygraphOutput("Historical")
                  )
                )
            ),
            box(title="Volume Integrated Yearly Average: Customer Specific",width=NULL,status="primary",solidHeader = TRUE,collapsible = F,
                fluidRow(
                  column(width=6,
                         dygraphOutput("HistVolIntegrated")
                  ),
                  column(width=6,
                         div(style = 'overflow-x: scroll',DT::dataTableOutput("HistVolIntegratedTable"))
                  )
                )
            )
    ),###end current tab
    
    tabItem(tabName = "saveimage",
            box(title="Save model image?",width=NULL,status = "warning",solidHeader = TRUE,
                #shinySaveButton('downloadData', 'Save Model Image', 'Save file as...', filetype=list(imgage='RData')),
                downloadButton('downloadData','Save Model Settings?')),
            
            box(title="Model Image Values at Current",width=NULL,status="primary",solidHeader = TRUE,collapsible = F,
                    div(class="span7", verbatimTextOutput("modelInput"))
                )###end box
    ),###end current tab
    
    tabItem(tabName = "MapSelected",
            box(title="Map of Selected Data",width=NULL,status="primary",solidHeader = TRUE,collapsible = F,
                  fluidRow(
                    column(width=2,
                           checkboxGroupInput("maplayers","Map Layers to Display",
                                       c("Selected Data","Selected Counties","State Borders","Unselected Data","Selected Circles"),
                                       selected=c("Selected Data","State Borders"))
                           ),
                    column(width=10,
                           plotOutput(outputId = "MapSelectedData",height="800px")
                           )
                    )
              )
    ),
    tabItem(tabName = "DateRange",
            box(title="Select Date Range For Analysis By Dragging on Screen",width=NULL,status="primary",solidHeader = TRUE,collapsible = F,
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
            box(title="Clean Up Fixed Rate Contracts: Click Data to Identify Groups for Removal",width=NULL,status="primary",solidHeader = TRUE,collapsible = F,
                fluidRow(
                  column(width=2,
                         h3(textOutput("RemoveCustomerCarrierHover")),
                         checkboxInput("EnableSelect","Enable Customer Carrier Click Removal (warning only use with smaller data sets)",value=FALSE),
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
            tabBox(title="Model Fit Diagnostics",width=NULL,id="diagnostics",
                tabPanel(title="Partial Effects",value="partial",
                                plotOutput("ModelPlot",height="600px")
                         ),
                tabPanel(title="Model Diagnostics",value="diagnostics",
                                  plotOutput("ModelDiagnostics",height="600px")
                         ),
                tabPanel(title="Model Summary",value="summary",
                                  div(class="span7", verbatimTextOutput("ModelSummary"))
                         )
            )
            ),###end current tab
    tabItem(tabName = "partial",
            box(title="Partial Effects",width=NULL,status="primary",solidHeader = TRUE,collapsible = F,
                         fluidRow(
                           column(width=2,
                                  uiOutput("PartialEffect"),
                                  uiOutput("NusianceEffect"),
                                  uiOutput("NusianceLevels"),
                                  checkboxGroupInput("PartialSeries","Select Layers",
                                                     c("Fitted","Observed"),
                                                     selected=c("Fitted","Observed"))
                           ),
                           column(width=10,
                                  plotOutput("PartialPlot")
                           )
                           )
                )
    ),###end current tab
    tabItem(tabName = "marginal",
            box(title="Marginal Model Effects",width=NULL,status="primary",solidHeader = TRUE,collapsible = F,
                            fluidRow(
                              column(width=2,
                                     uiOutput("MarginalEffect")
                              ),
                              column(width=10,
                                    plotOutput("MarginalPlot")
                              )
                              )
                )
    ),###end current tab
    tabItem(tabName = "prediction",
            box(title="Model Fit To Raw Data",width=NULL,status="primary",solidHeader = TRUE,collapsible = F,
                fluidRow(
                  column(width=2,
                         checkboxGroupInput("PredictionPartial","Select Layers",
                                            c("Fitted","Observed","Predicted"),
                                            selected=c("Fitted","Observed","Predicted"))
                  ),
                  column(width=10,
                         plotOutput("PredictionFullPlot")
                  )
                )
            )
            
            ),###end current tab
    tabItem(tabName = "sessionInfo",
            box(title="Session Info",width=NULL,status="primary",solidHeader = TRUE,collapsible = F,
                div(class="span7", verbatimTextOutput("session"))
            )###end box
    ),###end current tab
    tabItem(tabName = "sessionInfo2",
            box(title="Session Info 2",width=NULL,status="primary",solidHeader = TRUE,collapsible = F,
                div(class="span7", verbatimTextOutput("session2"))
            )###end box
    )###end current tab
  )###end tab items
)# ##end body



dashboardPage(header, sidebar, body)




  
  

