
# This is the server logic for a Shiny web application.  You can find
# out more about building applications with Shiny here:
# http://shiny.rstudio.com


shinyServer(function(input, output, session) {
  
  updateCollapse(session, id = "collapse1", multiple = FALSE, open = NULL, close = NULL)
  
  output$table_mug <- renderUI({
    checkErrorNotFound()
    checkErrorImputed()
    checkErrorResolution()
    matrixCustom("mug", "MUG Typing", updateMUG())
  })

  # RID ---------------------------------------------------------------------

  runRID <- reactive({
    input$RID_lookup   
  })
  
  validateRID <- reactive({
    if (runRID() == 0) return(NULL)    
    
    # Don't get the RID until the button is clicked.
    isolate({
      rid <- input$RID      
    })

    if (nchar(rid) != 7) {
      "invalid"
    } else {
      rid
    } 
  })
    
  output$rid_invalid <- renderUI({
    if (runRID() == 0) return(NULL)    

    rid <- validateRID()

    if (rid == "invalid") {
      p("RID is invalid", style = "color:red")
    } else {
      ""
    }    
  })
  
  mugForRID <- reactive({
    rid <- validateRID() 
    print(paste("Getting Mug From the server", rid))
    mug <- removedDRBX( getMugForRID(rid,host) )    
  })
  
  lookupRID <- reactive({
    if (runRID() == 0) return(NULL)    
    
    rid <- validateRID() 
    
    if (!is.null(rid) && rid != "invalid") {
      
      mug <- mugForRID()
      
      #send to look up later
      if (!is.null(mug)) {
        rid
      } else {
        "notfound"    
      }     
    }    
  })
  
  updateMUG <- reactive({
    
    rid <- lookupRID()
    
    if (is.null(rid) || rid == "" || rid == "notfound") {
      return(display_mug)
    }
    
    formatListToTable(mug <- mugForRID())
  })
  
  output$error_box <- renderUI ({
    rid <- lookupRID()
    if (!is.null(rid) && rid == "notfound") {
      return(div(h3(paste("Sorry, RID was not found.") , style = "color:red"),
                 div("If you have the typings, try entering them on the left.")))
    }
    
    if(!is.null(rid) && rid != "notfound"){
      mug <- removedDRBX(getMugForRID(rid,host))
    } else {
      mug <- lookupMUG() 
    }
    
    error_message <- ""
    if(!is.null(mug) && length(mug) > 0 && length(mug) < 5) {
      error_message <- "Missing loci are being imputed, results may be less reliable."
    }
    
    if(!is.null(mug) && length(mug) > 0 && !isHighResolution(mug)) {
      error_message <- paste(error_message, "Some loci are low resolution and are being imputed, results may be less reliable.")
    }
    
    div(h3(error_message), style = "color:red")
    
  })
  
  checkErrorNotFound <- reactive({
    rid <- lookupRID()
    if (!is.null(rid) && rid == "notfound") {
      createAlert(session, inputId = "alert_anchor1",
                  title = "Sorry, RID was not found.",
                  message = "If you have the typings, try entering them on the left.",
                  type = "danger",
                  dismiss = TRUE,
                  block = FALSE,
                  append = TRUE
      )
    } 
  })
  
  checkErrorImputed <- reactive({
    rid <- lookupRID()
    if(!is.null(rid) && rid != "notfound"){
      mug <- removedDRBX(getMugForRID(rid,host))
    } else {
      mug <- lookupMUG() 
    }
    
    if (!is.null(mug) && length(mug) > 0 && length(mug) < 5) {
      createAlert(session, inputId = "alert_anchor2",
                  title = "Incomplete Typings",
                  message = "Missing loci are being imputed, results may be less reliable.",
                  type = "warning",
                  dismiss = TRUE,
                  block = FALSE,
                  append = TRUE
      )
    }
  })
  
  checkErrorResolution <- reactive({
    rid <- lookupRID()
    if(!is.null(rid) && rid != "notfound"){
      mug <- removedDRBX(getMugForRID(rid,host))
    } else {
      mug <- lookupMUG() 
    }
    
    if (!is.null(mug) && length(mug) > 0 && !isHighResolution(mug)) {
      createAlert(session, inputId = "alert_anchor2",
                  title = "Low Resolution Typings",
                  message = "Some loci are low resolution and are being imputed, results may be less reliable.",
                  type = "warning",
                  dismiss = TRUE,
                  block = FALSE,
                  append = TRUE
      )
    }
  })
  
  # SIRE --------------------------------------------------------------------
  
  getSIRE <- reactive ({
    sire <- input$population_group
  })
  
  ridSIRE <- reactive({
    
    rid <- lookupRID()
    
    if (is.null(rid) || rid == "" || rid == "notfound") {
      return("")
    }
    
    sire <- lookupSIRE()
    
    rollupRace(sire)
  })
  
  lookupSIRE <- reactive({
    rid <- lookupRID()
    print(paste("Looking up Sire", rid))
    getSireForRID(rid = rid, SERVER_HOST_ADDRESS)  
  })
  
  output$sire_text <- renderUI({
    sire <- ridSIRE()
    if (sire != "") {
      fullsire=POPULATIONS_TEXT[which(POPULATIONS==sire)]
      line <- paste("SIRE:", fullsire)
      div(h3(line), if (sire == "UNK") {
        p("Unknown SIRE, defaulting to CAU.")
      } else if (sire == "NAM"){
        p("NAM SIRE, defaulting to CAU due to low numbers.")
      } else {
        br()
      })
    }
    
  })
  
  # MUG ---------------------------------------------------------------------
  
  runMUG <- reactive({
    input$MUG_lookup    
  })
  
  validateMUG <- reactive({
    if(runMUG() == 0) return(NULL)
    
    mug <- extractMUG()
    
    print("Mug extracted")
    if(length(mug) > 0) {
      loci <- sapply(mug, FUN=function(c) c$locus)
      if (all(c("A","B") %in% loci)){      
        return(mug)    
      }
    }
    
    list()    
  })
  
  lookupMUG <- reactive({
    mug <- validateMUG()
  })
  
  extractMUG <- reactive({
    input_mug <- data.frame(input$mug)
    HLA <- list()
    reg <- 1
    for (i in 1:nrow(input_mug)) {
      s <- list(locus = input_mug[i, 1], type1 = input_mug[i, 2], type2 = input_mug[i, 3])
      if (s$type1 != "" | s$type2 != "") {
        HLA[[reg]] <- s
        reg <- reg + 1
      }
    }
    HLA
  })
  
  output$mug_invalid <- renderUI({
    mug <- validateMUG()
    if (is.null(mug)) {
      return(NULL)
    }
    
    if(length(mug) == 0) {
      p("HLA Typing must include A and B.", style = "color:red")    
    } 
  })
  
  removedDRBX <- function (mug){
    nloci <- length(mug)
    if(nloci>0){
      keepInds <-numeric()
      for (l in 1:nloci){
        this <- mug[[l]]$locus
        if(this !="DRB3" && this !="DRB4" && this !="DRB5" && this !="DPB1"&& this !="DPA1" && this !="DQA1") {
          keepInds <- c(keepInds,l)
        }
          
        
      } 
     return(mug[keepInds])
    }
    NULL
  }
  
  # Prognosis ---------------------------------------------------------------
  
  getPrognosis <- reactive({
    
    if (runRID() == 0  && runMUG() == 0) {
      return(NULL)
    }
    
    
    if (runMUG() != 0) {
      mug <- lookupMUG()
      
      if( !is.null(mug) &&  length(mug) != 0) {
        
        sire <- getSIRE()
        
        imputation <- imputePairForMug(sire, mug,host)
        if (!is.null(imputation)){
          h1 <- as.numeric(imputation$Frequency1[1])
          h2 <- as.numeric(imputation$Frequency2[1])
          prog <- calculatePrognosis(h1, h2, race = sire)
          return(prog)
        } else {
          h1 <- 0
          h2 <- 0
          prog <- calculatePrognosis(h1, h2, race = sire)
        }
        
        
      } else {
        return(NULL)
      }
    } else if (runRID() != 0){
      rid <- lookupRID()
      if( !is.null(rid) &&  rid != "notfound") {
        
        mug <- mugForRID()
        sire <- ridSIRE()
        
        imputation <- imputePairForMug(sire, mug,host)
        
        if (!is.null(imputation)){
          h1 <- as.numeric(imputation$Frequency1[1])
          h2 <- as.numeric(imputation$Frequency2[1])
          prog <- calculatePrognosis(h1, h2, race = sire)
        return(prog)
        } else {
          h1 <- 0
          h2 <- 0
          prog <- calculatePrognosis(h1, h2, race = sire)
        }
        
      } else {
        return(NULL)
      }
      
    } 
    
  })

  output$prognosis_section <- renderUI({
    
    if (runRID() == 0  && runMUG() == 0) {
      return(NULL)
    }
    
    prognosis_probabilities <- getPrognosis()
    
    if (!is.null(prognosis_probabilities)){
      
      prog <-  names(prognosis_probabilities)[prognosis_probabilities==max(prognosis_probabilities)]  
      
      filename <- paste0(prog,'.jpg')
      
      div(span(paste("Prognosis:", prog), style = "font-size:2em"), 
          img(src=filename,height=100, width=100))
    } else {
      NULL
    }
  })
  
  output$sire_stack <- renderChart({
    rid <- lookupRID()
    mug <- lookupMUG()
    sire <- ridSIRE()
    if( (!is.null(rid) && rid != "notfound") || ( !is.null(mug) &&  length(mug) != 0)) {      
      prognosis_probabilities <- getPrognosis()      
      
      p<- makeBarPlot(prognosis_probabilities, "SIRE Prognosis Confidence")
      p$addParams(dom = 'sire_stack') 
      return(p)
      
     } else {
       b=Highcharts$new()
       b$addParams(dom = 'sire_stack',height=0,width=0) 
       return(b) 
     }
  })
  
  output$bayes_stack <- renderChart({
    rid <- lookupRID()
    mug <- lookupMUG()
    
    if ( !is.null(mug) &&  length(mug) != 0) {
      
      prognosis <- condenseBayes(buildBayesTable(mug))
      
      p<- makeBarPlot(prognosis, "Bayes Prognosis Confidence")
      p$addParams(dom = 'bayes_stack')       
      return(p)
    } else if( !is.null(rid) && rid != "notfound") {
      mug <- mugForRID()
      
      prognosis <- condenseBayes(buildBayesTable(mug))
            
       p<- makeBarPlot(prognosis, "Bayes Prognosis Confidence")
       p$addParams(dom = 'bayes_stack') 
      
      return(p)
        
    } else {
      b=Highcharts$new()
      b$addParams(dom = 'bayes_stack',height=0,width=0) 
      return(b) 
    }
    
  })

  observe({
    
    sire <- ridSIRE()
    updateRadioButtons(session, "population_group", selected = sire)
    
  })

  output$bayes_text <- renderUI({
    
    rid <- lookupRID()
    mug <- lookupMUG()
    
    if( !is.null(rid) && rid != "notfound") {
      return(h3('Bayes Classifier on all broad races.'))
    }
    
    if( !is.null(mug) && length(mug) > 0) {
      return(h3('Bayes Classifier on all broad races.'))
    }
    
    return(NULL)
  })

  # Pie ---------------------------------------------------------------------

  output$race_probability_pie <- renderChart ({
    rid <- lookupRID()  ### remove after sire is corrected
    mug <- lookupMUG()
    
    if (!is.null(mug) && length(mug) > 0 ){
      
      sire <- getSIRE()
      r_count <- calculateRaceProbability(sire,mug)
      
      if(all(is.nan(r_count))){
        b=Highcharts$new()
        b$addParams(dom = 'race_probability_pie',height=0,width=0) 
        return(b)     
      }
    } else if (!is.null(rid) && rid != "notfound"){
      
      sire <- ridSIRE()
      mug <- mugForRID()
      r_count <- calculateRaceProbability(sire,mug)
      if(all(is.nan(r_count))){
        b=Highcharts$new()
        b$addParams(dom = 'race_probability_pie',height=0,width=0) 
        return(b) 
      }
      
    } else {
      b=Highcharts$new()
      b$addParams(dom = 'race_probability_pie',height=0,width=0) 
      return(b) 
    }
    
    r_count <- round(r_count,digits = 4)
    plotdata <- lapply(1:4, function(x) {list(POPULATIONS[x],r_count[x])})
    
    p<- Highcharts$new()
    p$plotOptions(pie = list (showInLegend =TRUE))
    p$series(name='Populations',type='pie', data=plotdata)
    p$title(text="Probability of Race")
    p$addParams(dom = 'race_probability_pie') 
    
    return (p)
  })

  output$race_prognosis_table <- renderDataTable ({
    rid <- lookupRID()  ### remove after sire is corrected
    mug <- lookupMUG()
    
    race_table <- NULL
    
    if (!is.null(rid) && rid != "notfound"){
      mug <- mugForRID()
    }
    
    if (!is.null(mug) && length(mug) > 0) {
      race_table  <- buildRaceTable(mug)
      race_table[,2]<-paste0(round(race_table[,2]*100,1),"%")
      race_table[,3]<-paste0(round(race_table[,3]*100,1),"%")
      race_table[,4]<-paste0(round(race_table[,4]*100,1),"%")
      race_table[,5]<-paste0(round(race_table[,5]*100,1),"%")
    }
    
    race_table
    
  }, options = list(paging = FALSE, searching=FALSE, info = FALSE))

  output$inputTable <- renderTable ({
    rid <- lookupRID()
    if(is.null(rid)){
      mug <- lookupMUG()
    } else {
      mug <- mugForRID()
    }
    
    outTable <- buildBayesTable(mug)
    
    this <- outTable[,1:4]
    this[,3] <- format(signif(this[,3], 3),scientific = TRUE)
    this[,c(2)] <- paste0(round(this[,c(2)]*100,1),"%")
    this[,c(4)] <- paste0(round(this[,c(4)]*100,1),"%")
    this
    
  })

  getBayesInput <- reactive({
    #TODO: These will be set to constant values later. Move it to global.r
    #     PSEUDO_COUNT <- input$constant 
    #     CAU_PERCENT <- input$cauPerc
    #     AFA_PERCENT <- input$afaPerc
    #     HIS_PERCENT <- input$hisPerc
    #     API_PERCENT <- input$apiPerc
    #     THE_BIG_N <- input$n
    
#     c(PSEUDO_COUNT = input$constant , 
#       CAU_PERCENT = input$cauPerc,
#       AFA_PERCENT = input$afaPerc,
#       HIS_PERCENT = input$hisPerc,
#       API_PERCENT = input$apiPerc,
#       THE_BIG_N = sum(c(input$cauPerc,input$afaPerc,input$hisPerc,input$apiPerc,4*input$constant)))
    
    race_list <- input$bayesRace
    weight <- 100/length(race_list)
    CAU <- if('CAU' %in% race_list) {weight} else {0}
    AFA <- if('AFA' %in% race_list) {weight} else {0}
    HIS <- if('HIS' %in% race_list) {weight} else {0}
    API <- if('API' %in% race_list) {weight} else {0}
    
    c(PSEUDO_COUNT = input$constant , 
      CAU_PERCENT = CAU,
      AFA_PERCENT = AFA,
      HIS_PERCENT = HIS,
      API_PERCENT = API,
      THE_BIG_N = sum(c(CAU,AFA,HIS,API,4*input$constant)))
    
  })

# Helper Functions --------------------------------------------------------
makeBarPlot <- function(prognosis, title) {
  percent <- as.numeric(round(prognosis,digits=2))
  levels <- cumsum(percent)

 p<- Highcharts$new()
 p$chart(type = "column", width=300)
 p$plotOptions(column = list(stacking = "percent"))
 p$xAxis(categories = c("Prognosis",""))
 p$series(name = "Poor", data = percent[3]*100, stack = "Prognosis")
 p$series(name = "Fair", data = percent[2]*100, stack = "Prognosis")
 p$series(name = "Good", data = percent[1]*100, stack = "Prognosis")
 p$title(text=title)

 return(p)

}

formatListToTable <- function(mugList){
  n <- length(mugList)
  mugTable <- display_mug
  if (n > 0){
    for (i in 1:n){
      currLocus <- mugList[[i]]$locus
      mugTable[which(mugTable$locus==currLocus),2:3] <- c(mugList[[i]]$type1, mugList[[i]]$type2)
    } 
  }
  mugTable
}

buildRaceTable <- function (mug) {
  full_bayes_table <- buildBayesTable(mug)
  
  race_table <- full_bayes_table[,c('Population','Posterior','Good','Fair','Poor')]
  
  prognosis <- PROGNOSIS[max.col(race_table[,c('Good','Fair','Poor')])]    
  race_table['Prognosis'] <- prognosis
  for(col in c('Posterior','Good','Fair','Poor')) {
    race_table[col] <- round(race_table[col], digits = 4)      
  }
  
  race_table
}

calculateRaceProbability <- function(sire,mug) {
  outTable <- buildBayesTable(mug)
  outTable$Posterior
}

condenseBayes <- function(bayes_table){
  good_weighted_score <- sum(bayes_table$Good * bayes_table$Posterior)
  fair_weighted_score <- sum(bayes_table$Fair * bayes_table$Posterior)
  poor_weighted_score <- sum(bayes_table$Poor * bayes_table$Posterior)
  
  #if all Probs are 0 return Poor
  if(sum(good_weighted_score, fair_weighted_score,poor_weighted_score)==0){
    data.frame(Good = 0, Fair = 0, Poor = 1)
  } else {
    data.frame(Good = good_weighted_score, Fair = fair_weighted_score, Poor = poor_weighted_score)
  }
}

# High Resolution typing has a typing with ':' in it or is empty.
# Empty means the locus has homozygous typings
isHighResolution <- function (mug) {
  for(i in 1:length(mug)) {
    typing <- mug[[i]]
    if(!grepl(':', typing$type1) || (typing$type2 != "" && !grepl(':', typing$type2))) {
      return(FALSE)
    }
  }
  TRUE
}

rollupRace <- function(sire){
  if (sire %in% c("AFA","AAFA","AFB","CARB","SCAMB","MAFA","NAMB")){
    return("AFA")
  }
  if (sire %in% c("API","AINDI","FILII","JAPI","KORI","NCHI","VIET","SCSEAI","MAPI","HAWI","HAWAII","GUAMAN","SAMOAN","OPI","MHAW")){
    return("API")
  }
  if (sire %in% c("NAM","ALANAM","AMIND","AISC","CARIBI","MNAM")){
    return("CAU")
  }
  if (sire %in% c("CAU","NAMER","EEURO","WEURO","EURCAU","NEURO","MEDIT","MIDEAS","NCAFRI","WSCA","WCARIB","MENAFC","MCAU","EURWRC")){
    return("CAU")
  }
  if (sire %in% c("MSWHIS","SCAHIS","CARHIS","MHIS","HIS")){
    return("HIS")
  }
  return("UNK")
}

buildBayesTable <- function(mug){

  bayes_input <- getBayesInput()
  
  outTable <- as.data.frame(matrix(NA,nrow=4,ncol=9))
  names(outTable) <- c('Population','Prior','Likelihood','Posterior','Good','Fair','Poor')
  outTable$Population <- c('CAU','AFA','HIS','API')
  outTable$Pseudocount <- bayes_input['PSEUDO_COUNT']
  outTable$Prior[1] <- bayes_input['CAU_PERCENT']
  outTable$Prior[2] <- bayes_input['AFA_PERCENT']
  outTable$Prior[3] <- bayes_input['HIS_PERCENT']
  outTable$Prior[4] <- bayes_input['API_PERCENT']
  
  for (i in 1:4){
    outTable$Prior[i] <-  (outTable$Prior[i] + bayes_input['PSEUDO_COUNT'])/bayes_input['THE_BIG_N']
  }
  
  bayes<-calculateBayesProbabilities(mug,race=outTable$Population,prior=outTable$Prior,host)
  outTable$Posterior<-bayes$posterior
  outTable$Likelihood<-bayes$likelihood
  
  if (!is.null(mug)) {
    for (i in 1:4) {
      race <- POPULATIONS[i]
      imputation <- imputePairForMug(race, mug,host)
      
      if (length(imputation)>0){
        prog <- calculatePrognosis(as.numeric(imputation$Frequency1[1]),as.numeric(imputation$Frequency2[1]),race)
        outTable$Good[i] <- prog$Good
        outTable$Fair[i] <- prog$Fair
        outTable$Poor[i] <- prog$Poor
      } else {
        outTable$Good[i] <- 0
        outTable$Fair[i] <- 0
        outTable$Poor[i] <- 1
      }
      
      #outTable$Prob[i] <-  outTable$GF[i] * outTable$Post[i]
    }
    #####set to non-zero to avoid NaN
    #outTable$Prob <- outTable$Prob/(sum(outTable$Prob)+0.0000000000001)
  }
  
  outTable
}
  
})


