library(shiny)

shinyUI(fluidPage(
  
    # Application title
  titlePanel("Search Prognosis Tool"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(width=3,
      
  conditionalPanel(condition="input.collapse1!=2",
                                  
      numericInput('RID',h5('RID'),value=NULL),
      uiOutput('rid_invalid'),
      actionButton('RID_lookup','RID Lookup'),
      br(),
      br(),
      
      h5('Patient HLA Typing'),    
      helpText("For the most accurate result use high",
               "resolution typings at all loci possible."),
      
      div(id='question',"Need Help?",class='simpleDiv', style = "color:blue"),
      
      uiOutput('table_mug'),
    
      uiOutput('mug_invalid'),

      radioButtons('population_group',label=h5("Broad Races"),
                         choices=list('White (CAU)'='CAU',
                                      'African American (AFA)'='AFA',
                                      'Hispanic (HIS)'='HIS',
                                      'Asian Pacific Islander (API)'='API',
                                      'Unknown'='UNK')),
      
      actionButton('MUG_lookup','HLA Lookup')
    ),

  
    conditionalPanel(condition="input.collapse1==2",
        h5('Bayes Input Options'),
          helpText("Choose below the races you would like to consider for the Bayesian Classifier"),
          checkboxGroupInput(inputId = "bayesRace",
                             label="Populations",
                             choices = c('White'="CAU",'African American'="AFA",'Hispanic'="HIS",'Asian Pacific Islander'="API"),
                             selected = c("CAU","AFA","HIS","API")),
          br(),
          sliderInput('constant','Prior Uncertainty Constant',min=0,max=100,value=25,round=TRUE)
      )
#       sliderInput('cauPerc','CAU Relative Percent',min=1,max=100,value=67,round=TRUE),
#       sliderInput('afaPerc','AFA Relative Percent',min=1,max=100,value=7,round=TRUE),
#       sliderInput('hisPerc','HIS Relative Percent',min=1,max=100, value=10,round=TRUE),
#       sliderInput('apiPerc','API Relative Percent',min=1,max=100, value=7,round=TRUE)      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      bsAlert(inputId = "alert_anchor1"),
      bsAlert(inputId = "alert_anchor2"),
      bsAlert(inputId = "alert_anchor3"),
      
      #uiOutput("error_box"),
      br(),
      bsCollapse(multiple = FALSE, id = "collapse1",open = "col1",
                 
                 bsCollapsePanel("Self Identifed Race Results", 
                                 uiOutput("sire_text"),
                                 uiOutput("prognosis_section"), 
                                 showOutput("sire_stack",'highcharts'),
                                 id="col1", 
                                 value=1),
                 bsCollapsePanel("Bayesian Parameters", 
                                 p("This table displays values used in Bayes Calculations."),
                                 p("Using the selection boxes on the left, you can choose which races will receive weight."),
                                 p("The Uncertainty Constant/Pseudo Count helps to strengthen or weaken those weights. The higher the constant, the less confident you are in your guess and the lower the constant the more confident you are."),
                                 tableOutput('inputTable'),
                                 id="col2", 
                                 value=2),
                 bsCollapsePanel("Bayesian Race Results", 
                                 p("Bayesian considers a set of races for the a mug and calculates a probability that the typing belongs to a race based on the frequency of the genotype in the selected races. The probabilty is displayed in the pie plot below."),
                                 p("The table on the right shows the prognosis scores for each available race.  These scores, weighted by the probabilty are used to create the overall Bayesian Prognosis score in the bar plot."),
                                 h4('Bayes Calculations'),
                                 fluidRow(column(5,showOutput('race_probability_pie','highcharts')),
                                          column(7,dataTableOutput('race_prognosis_table'))),
                                 h4('Overall Bayes Prognosis:'),
                                 showOutput("bayes_stack",'highcharts'),
                                 id="col3", 
                                 value=3)
      )
      
    )
  ),

bsTooltip(id="table_mug", title="Input example: 01:02 or 55:01", placement="top",trigger="hover"),  
bsPopover(id="question", title="",
          content="When entering data, enter the one typing per box for each corresponding locus. You should only enter the allele, without the locus. If only one typing is input at a locus, it is assumed to be homozygous.", 
          placement="top",trigger="click")
))
