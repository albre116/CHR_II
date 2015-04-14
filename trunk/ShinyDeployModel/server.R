options(shiny.maxRequestSize=500*1024^2)###500 megabyte file upload limit set


shinyServer(function(input, output, session) {
  
  ###########################################################
  #######Model Saving/Loading Features
  ###########################################################
  #####This saves a model image of all of the chosen settings
  output$downloadData<-downloadHandler(
    filename = function(){paste(input$settings_name,".RData",sep = "")},
    content = function(file){
      saved_settings <- reactiveValuesToList(input)
      save(saved_settings, file = file)
    })
  
  ####this will load a model image and set the values of the different selectors
  Read_Settings <- reactive({
    inFile <- input$settings_file
    if (is.null(inFile)) return(NULL)
    load(inFile$datapath)
    return(saved_settings)
  })
  
  ModelImageUpdate <- reactiveValues()
  
  ####scan across inputs and set values for static inputs
  Change_static_settings <- observe({
    if (is.null(Read_Settings())){return(NULL)}
    R <- Read_Settings()
    #####change the data selection settings on page 1
    updateSelectInput(session,"response",selected = R[["response"]])
    
    updateSelectizeInput(session,"OrigZip3",selected=R[["OrigZip3"]])
    updateSelectizeInput(session,"OrigCity",selected=R[["OrigCity"]])
    updateNumericInput(session,"OrigRadius",value=R[["OrigRadius"]])
    
    updateSelectizeInput(session,"DestZip3",selected=R[["DestZip3"]])
    updateSelectizeInput(session,"DestCity",selected=R[["DestCity"]])
    updateNumericInput(session,"DestRadius",value=R[["DestRadius"]])
    
    updateSelectizeInput(session,"SelectOrigStates",selected=R[["SelectOrigStates"]])
    updateCheckboxGroupInput(session,"maplayersOrigStates",selected = R[["maplayersOrigStates"]])
    
    updateSelectizeInput(session,"SelectDestStates",selected=R[["SelectDestStates"]])
    updateCheckboxGroupInput(session,"maplayersDestStates",selected = R[["maplayersDestStates"]])

    
    updateSelectizeInput(session,"SelectOrigCounties",selected = R[["SelectOrigCounties"]])
    updateSelectizeInput(session,"SelectOrigCircles",selected = R[["SelectOrigCircles"]])
    updateCheckboxGroupInput(session,"maplayersOrigCounties",selected = R[["maplayersOrigCounties"]])
    updateSelectInput(session,"OrigCircle",selected=R[["OrigCircle"]])
    updateNumericInput(session,"CircleRadiusOrig",value=R[["CircleRadiusOrig"]])
    
    updateSelectizeInput(session,"SelectDestCounties",selected = R[["SelectDestCounties"]])
    updateSelectizeInput(session,"SelectDestCircles",selected = R[["SelectDestCircles"]])
    updateCheckboxGroupInput(session,"maplayersDestCounties",selected = R[["maplayersDestCounties"]])
    updateSelectInput(session,"DestCircle",selected=R[["DestCircle"]])
    updateNumericInput(session,"CircleRadiusDest",value=R[["CircleRadiusDest"]])
    
    
    #####Customer Specific Section of inputs
    updateSelectInput(session,"ModelFamily",selected = R[["ModelFamily"]])
    updateSliderInput(session,"ConfLimits",value = R[["ConfLimits"]])


    
    ####assign the values to slots in an object that can be re-written
    for(i in names(R)){
      ModelImageUpdate[[i]] <- R[[i]]
    }
    
  })
  
  
  
  output$FilterDate <- renderUI({
    value=TRUE
    if(!is.null(Read_Settings()[["FilterDate"]])){
      value <- Read_Settings()[["FilterDate"]]
    }
    checkboxInput("FilterDate","Perform Date and Observation Filtering?",value=value)
  })
  


  ###########################################################
  #######Tab Panel 1:  Geography
  ###########################################################
  output$response<- renderUI({
    idx <- colnames(RAW)
    selectInput("response","Response",choices=idx,selected=c("CPM_AllInCarrier"))
  })
  
  output$OrigZip3<- renderUI({
    idx <- unique(RAW$Orig3DigZip)
    selectizeInput("OrigZip3","3-Digit Origin Zip",choices=idx,selected=NULL,multiple=TRUE)
  })
  
  output$DestZip3<- renderUI({
    idx <- unique(RAW$Dest3DigZip)
    selectizeInput("DestZip3","3-Digit Destination Zip",choices=idx,selected=NULL,multiple = TRUE)
  })
  
  output$OrigCity<- renderUI({
    idx <- unique(RAW$OrigCity)
    selectizeInput("OrigCity","Origin City",choices=idx,selected=NULL,multiple=TRUE)
  })
  
  output$DestCity<- renderUI({
    idx <- unique(RAW$DestCity)
    selectizeInput("DestCity","Destination City",choices=idx,selected=NULL,multiple = TRUE)
  })
  
  output$DateRange <- renderUI({
    data <- RAW###data brought in after filtering is complete
    start_date <-max(data$EntryDate)
    yr <- format(start_date,format="%Y")
    mo <- format(start_date,format="%m")
    day <- format(start_date,format="%d")
    yr <- as.numeric(yr)+1
    end_date <- as.Date(format(paste(yr,mo,day,sep="-"),
                               format="%y-%m-%d"))
    
    if(!is.null(Read_Settings()[["DateRange"]])){
      start_date <- Read_Settings()[["DateRange"]][1]
      end_date <- Read_Settings()[["DateRange"]][2]
    }
    
    
    dateRangeInput("DateRange","Select Prediction Date Range ",
                   start=start_date,
                   min=start_date,
                   end=end_date,
                   max=end_date
    )
  })
  
  
  
  RAWPLOT <- reactive({
    reduced_orig <- data.frame(x=RAW$OrigLongitude,y=RAW$OrigLatitude)
    reduced_orig <- unique(reduced_orig)
    reduced_dest <- data.frame(x=RAW$DestLongitude,y=RAW$DestLatitude)
    reduced_dest <- unique(reduced_dest)
    return(list(reduced_orig=reduced_orig,reduced_dest=reduced_dest))
  })
  
  ###########################################################
  #######All of the Selection functions for the Origin States
  ###########################################################
  
      OriginAddStates<- reactiveValues(x=NULL, y=NULL)
      OriginStatesHover <- reactiveValues(x=NULL, y=NULL)
      ###listen for clicks
      observe({
        # Initially will be empty
        if (is.null(input$OriginStates)){
          return()
        } else{
        isolate(OriginAddStates$x <- input$OriginStates$x)
        isolate(OriginAddStates$y <- input$OriginStates$y)
        }
      })
      
      ###listen for hover
      observe({
        # Will be NULL when no hover
        if (is.null(input$OriginStatesHover)){
          return()
        } else{
          isolate(OriginStatesHover$x <- input$OriginStatesHover$x)
          isolate(OriginStatesHover$y <- input$OriginStatesHover$y)
        }
      })
      
      

      ClickStateAddOrig<- reactive({
        AddOrigStates <- map.where(states,x=OriginAddStates$x,y=OriginAddStates$y)
        AddOrigStates <- AddOrigStates[!is.na(AddOrigStates)]
        if (length(AddOrigStates)==0){AddOrigStates <- NULL}
        return(AddOrigStates)
      })
      
      output$SelectOrigStates <- renderUI({
        isolate(selected <- input$SelectOrigStates)
        selected <- c(selected,ClickStateAddOrig())
        selected <- unlist(lapply(selected,function(x){strsplit(x,":")[[1]][1]}))
        selected <- unique(selected)
        selected <- selected[!is.null(selected)]
        selectizeInput("SelectOrigStates","Selected Origin States",choices=unlist(lapply(states$names,function(x){strsplit(x,":")[[1]][1]})),selected=selected,multiple=T)
      })
      
      output$AddStatesHoverSelectedOrigin <- renderText({
        if(is.null(input$OriginStatesHover)){return("Mouse Hover:")}
        AddOrigStates <- map.where(states,x=OriginStatesHover$x,y=OriginStatesHover$y)
        AddOrigStates <- AddOrigStates[!is.na(AddOrigStates)]
        if (length(AddOrigStates)==0){AddOrigStates <-NULL}
        return(paste("Mouse Hover:",AddOrigStates))
      })
      
      
      output$OrigPlotState <- renderPlot({
        plot_states(selectStates=input$SelectOrigStates,
                    reduced=RAWPLOT()[["reduced_orig"]],
                    cities=input$OrigCity,
                    mapLayer=input$maplayersOrigStates,
                    color="blue")
      })
      
      ################################################################
      #######All of the Selection functions for the Destination States
      ################################################################
      
      DestinationAddStates<- reactiveValues(x=NULL, y=NULL)
      DestinationStatesHover <- reactiveValues(x=NULL, y=NULL)
      ###listen for clicks
      observe({
        # Initially will be empty
        if (is.null(input$DestinationStates)){
          return()
        } else{
          isolate(DestinationAddStates$x <- input$DestinationStates$x)
          isolate(DestinationAddStates$y <- input$DestinationStates$y)
        }
      })
      
      ###listen for hover
      observe({
        # Will be NULL when no hover
        if (is.null(input$DestinationStatesHover)){
          return()
        } else{
          isolate(DestinationStatesHover$x <- input$DestinationStatesHover$x)
          isolate(DestinationStatesHover$y <- input$DestinationStatesHover$y)
        }
      })
      
      
      
      ClickStateAddDest<- reactive({
        AddDestStates <- map.where(states,x=DestinationAddStates$x,y=DestinationAddStates$y)
        AddDestStates <- AddDestStates[!is.na(AddDestStates)]
        if (length(AddDestStates)==0){AddDestStates <- NULL}
        return(AddDestStates)
      })
      
      output$SelectDestStates <- renderUI({
        isolate(selected <- input$SelectDestStates)
        selected <- c(selected,ClickStateAddDest())
        selected <- unlist(lapply(selected,function(x){strsplit(x,":")[[1]][1]}))
        selected <- unique(selected)
        selected <- selected[!is.null(selected)]
        selectizeInput("SelectDestStates","Selected Destination States",choices=unlist(lapply(states$names,function(x){strsplit(x,":")[[1]][1]})),selected=selected,multiple=T)
      })
      
      output$AddStatesHoverSelectedDestination <- renderText({
        if(is.null(input$DestinationStatesHover)){return("Mouse Hover:")}
        AddDestStates <- map.where(states,x=DestinationStatesHover$x,y=DestinationStatesHover$y)
        AddDestStates <- AddDestStates[!is.na(AddDestStates)]
        if (length(AddDestStates)==0){AddDestStates <-NULL}
        return(paste("Mouse Hover:",AddDestStates))
      })
      
      
      output$DestPlotState <- renderPlot({
        plot_states(selectStates=input$SelectDestStates,
                    reduced=RAWPLOT()[["reduced_dest"]],
                    cities=input$DestCity,
                    mapLayer=input$maplayersDestStates,
                    color="red")
      })
      
      ###########################################################
      #######All of the Selection functions for the Origin Counties
      ###########################################################
      CountiesOrigin <- reactive({
        pick <- input$SelectOrigStates
        if(!is.null(pick)){pick <- unlist(lapply(pick,function(x){strsplit(x,":")[[1]][1]}))}
        
        if(!is.null(input$OrigCity) | !is.null(pick)){
          selected <- c(pick,
                        (as.character(state.fips$polyname[
                          state.fips$abb %in% unlist(lapply(input$OrigCity,function(x){strsplit(x,",")[[1]][2]}))])))
          selected <- unlist(lapply(selected,function(x){strsplit(x,":")[[1]][1]}))
          selected <- unique(selected)
          selected <- selected[!is.null(selected)]}else{return(NULL)}
        
        counties <- map("county",regions = selected,plot=F,fill=TRUE)
        return(counties)
      })
      
      OriginAddCounties<- reactiveValues(x=NULL, y=NULL)
      OriginCountiesHover <- reactiveValues(x=NULL, y=NULL)
      
      ###listen for clicks
      observe({
        # Initially will be empty
        if (is.null(input$OriginCounties)){
          return()
        } else{
          isolate(OriginAddCounties$x <- input$OriginCounties$x)
          isolate(OriginAddCounties$y <- input$OriginCounties$y)
        }
      })
      
      ###listen for hover
      observe({
        # Will be NULL when no hover
        if (is.null(input$OriginCountiesHover)){
          return()
        } else{
          isolate(OriginCountiesHover$x <- input$OriginCountiesHover$x)
          isolate(OriginCountiesHover$y <- input$OriginCountiesHover$y)
        }
      })
      
      
      ClickCountiesAddOrig<- reactive({
        counties <- CountiesOrigin()
        if(is.null(counties)){return(NULL)}
        if(isolate(input$OrigCircle)=="Counties"){
        AddOrigCounties <- map.where(counties,x=OriginAddCounties$x,y=OriginAddCounties$y)
        AddOrigCounties <- AddOrigCounties[!is.na(AddOrigCounties)]
        if (length(AddOrigCounties)==0){AddOrigCounties <- NULL}
        return(AddOrigCounties)}else{
          OriginAddCounties$x###put to activate switch
          return(NULL)}
      })
      
      output$SelectOrigCounties <- renderUI({
        counties <- CountiesOrigin()
        isolate(selected <- input$SelectOrigCounties)
        isolate({
        if(!is.null(ModelImageUpdate[["SelectOrigCounties"]])){
          selected <- ModelImageUpdate[["SelectOrigCounties"]]
          ModelImageUpdate[["SelectOrigCounties"]] <- NULL
        }
          })
        pick <- unlist(lapply(input$SelectOrigStates,function(x){strsplit(x,":")[[1]][1]}))
        pick <- c(pick,counties$names)
        pick <- pick[!is.null(pick)]
        selected <- c(selected,ClickCountiesAddOrig())
        selected <- unique(selected)
        selected <- selected[!is.null(selected)]
        selectizeInput("SelectOrigCounties","Selected Origin Counties or Entire State",choices=pick,selected=selected,multiple=T)
      })
      
      OrigCircles <- reactive({
        pts <- data.frame(x=OriginAddCounties$x,y=OriginAddCounties$y)
        if(isolate(input$OrigCircle=="Bounding Circle")){
          return(data.frame(pts,r=isolate(input$CircleRadiusOrig)))
        }else{return(data.frame())}
      })
      


      output$SelectOrigCircles<- renderUI({
        counties <- CountiesOrigin()
        pts <- OrigCircles()
        isolate(pick <- input$SelectOrigCircles)
        isolate({
          if(!is.null(ModelImageUpdate[["SelectOrigCircles"]])){
            pick <- ModelImageUpdate[["SelectOrigCircles"]]
            ModelImageUpdate[["SelectOrigCircles"]] <- NULL
          }
        })
        pick <- c(pick,paste(pts$x,pts$y,pts$r,sep=":"))
        pick <- pick[!is.na(pick)]
        selected <- pick
        selected <- unique(selected)
        selected <- selected[!is.na(selected)]
        selectizeInput("SelectOrigCircles","Selected Circle Coordinates",choices=pick,selected=selected,multiple=T)
      })
      
      
      output$AddCountiesHoverSelectedOrigin <- renderText({
        counties <- CountiesOrigin()
        if(is.null(counties)){return(NULL)}
        if(is.null(input$OriginCountiesHover)){return("Mouse Hover:")}
        AddOrigCounties <- map.where(counties,x=OriginCountiesHover$x,y=OriginCountiesHover$y)
        AddOrigCounties <- AddOrigCounties[!is.na(AddOrigCounties)]
        if (length(AddOrigCounties)==0){AddOrigCounties <-NULL}
        return(paste("Mouse Hover:",AddOrigCounties))
      })
      
      
      output$OrigPlotCounties <- renderPlot({
        plot_counties(counties=CountiesOrigin(),
                      reduced=RAWPLOT()[["reduced_orig"]],
                      selectCounties=input$SelectOrigCounties,
                      City=input$OrigCity,
                      radius=input$OrigRadius,
                      Circles=input$SelectOrigCircles,
                      layers=input$maplayersOrigCounties,
                      color="blue")
      })
      
      ###########################################################
      #######All of the Selection functions for the Destination Counties
      ###########################################################
      CountiesDestination <- reactive({
        pick <- input$SelectDestStates
        if(!is.null(pick)){pick <- unlist(lapply(pick,function(x){strsplit(x,":")[[1]][1]}))}
        
        if(!is.null(input$DestCity) | !is.null(pick)){
          selected <- c(pick,
                        (as.character(state.fips$polyname[
                          state.fips$abb %in% unlist(lapply(input$DestCity,function(x){strsplit(x,",")[[1]][2]}))])))
          selected <- unlist(lapply(selected,function(x){strsplit(x,":")[[1]][1]}))
          selected <- unique(selected)
          selected <- selected[!is.null(selected)]}else{return(NULL)}
        
        counties <- map("county",regions = selected,plot=F,fill=TRUE)
        return(counties)
      })
      
      DestinationAddCounties<- reactiveValues(x=NULL, y=NULL)
      DestinationCountiesHover <- reactiveValues(x=NULL, y=NULL)
      
      ###listen for clicks
      observe({
        # Initially will be empty
        if (is.null(input$DestinationCounties)){
          return()
        } else{
          isolate(DestinationAddCounties$x <- input$DestinationCounties$x)
          isolate(DestinationAddCounties$y <- input$DestinationCounties$y)
        }
      })
      
      ###listen for hover
      observe({
        # Will be NULL when no hover
        if (is.null(input$DestinationCountiesHover)){
          return()
        } else{
          isolate(DestinationCountiesHover$x <- input$DestinationCountiesHover$x)
          isolate(DestinationCountiesHover$y <- input$DestinationCountiesHover$y)
        }
      })
      
      
      
      ClickCountiesAddDest<- reactive({
        counties <- CountiesDestination()
        if(is.null(counties)){return(NULL)}
        if(isolate(input$DestCircle)=="Counties"){
        AddDestCounties <- map.where(counties,x=DestinationAddCounties$x,y=DestinationAddCounties$y)
        AddDestCounties <- AddDestCounties[!is.na(AddDestCounties)]
        if (length(AddDestCounties)==0){AddDestCounties <- NULL}
        return(AddDestCounties)}else{
          DestinationAddCounties$x###put here to activate switch
          return(NULL)}
      })
      
      output$SelectDestCounties <- renderUI({
        counties <- CountiesDestination()
        isolate(selected <- input$SelectDestCounties)
        isolate({
          if(!is.null(ModelImageUpdate[["SelectDestCounties"]])){
            selected <- ModelImageUpdate[["SelectDestCounties"]]
            ModelImageUpdate[["SelectDestCounties"]] <- NULL
          }
        })
        pick <- unlist(lapply(input$SelectDestStates,function(x){strsplit(x,":")[[1]][1]}))
        pick <- c(pick,counties$names)
        pick <- pick[!is.null(pick)]
        selected <- c(selected,ClickCountiesAddDest())
        selected <- unique(selected)
        selected <- selected[!is.null(selected)]
        selectizeInput("SelectDestCounties","Selected Destination Counties or Entire State",choices=pick,selected=selected,multiple=T)
      })
      
      DestCircles <- reactive({
        pts <- data.frame(x=DestinationAddCounties$x,y=DestinationAddCounties$y)
        if(isolate(input$DestCircle=="Bounding Circle")){
          return(data.frame(pts,r=isolate(input$CircleRadiusDest)))
        }else{return(data.frame())}
      })
      
      output$SelectDestCircles<- renderUI({
        counties <- CountiesDestination()
        pts <- DestCircles()
        isolate(pick <- input$SelectDestCircles)
        isolate({
          if(!is.null(ModelImageUpdate[["SelectDestCircles"]])){
            pick <- ModelImageUpdate[["SelectDestCircles"]]
            ModelImageUpdate[["SelectDestCircles"]] <- NULL
          }
        })
        pick <- c(pick,paste(pts$x,pts$y,pts$r,sep=":"))
        pick <- pick[!is.na(pick)]
        selected <- pick
        selected <- unique(selected)
        selected <- selected[!is.na(selected)]
        selectizeInput("SelectDestCircles","Selected Circle Coordinates",choices=pick,selected=selected,multiple=T)
      })
      
      output$AddCountiesHoverSelectedDestination <- renderText({
        counties <- CountiesDestination()
        if(is.null(counties)){return(NULL)}
        if(is.null(input$DestinationCountiesHover)){return("Mouse Hover:")}
        AddDestCounties <- map.where(counties,x=DestinationCountiesHover$x,y=DestinationCountiesHover$y)
        AddDestCounties <- AddDestCounties[!is.na(AddDestCounties)]
        if (length(AddDestCounties)==0){AddDestCounties <-NULL}
        return(paste("Mouse Hover:",AddDestCounties))
      })
      
      
      output$DestPlotCounties <- renderPlot({
        plot_counties(counties=CountiesDestination(),
                      reduced=RAWPLOT()[["reduced_dest"]],
                      selectCounties=input$SelectDestCounties,
                      City=input$DestCity,
                      radius=input$DestRadius,
                      Circles=input$SelectDestCircles,
                      layers=input$maplayersDestCounties,
                      color="red")
      })
      
      
      ###########################################################
      #######Tab Panel 2:  Data Conditioning
      ###########################################################
      
      output$LinearTerms <- renderUI({
        terms <- colnames(RAW)
        if(!is.null(Read_Settings())){
          selected <- Read_Settings()[["LinearTerms"]]
          }else{selected <- c("NumericDate")}
        selectizeInput("LinearTerms","Linear Terms in Model",
                       choices=terms,selected=selected,multiple=T)
      })
      
      output$FactorTerms <- renderUI({
        terms <- colnames(RAW)
        if(!is.null(Read_Settings())){
          selected <- Read_Settings()[["FactorTerms"]]
        }else{selected <- c("SumOfStops")}
        selectizeInput("FactorTerms","Factors in Model",
                       choices=terms,selected=selected,multiple=T)
      })
      
      output$SplineTerms <- renderUI({
        terms <- colnames(RAW)
        if(!is.null(Read_Settings())){
          selected <- Read_Settings()[["SplineTerms"]]
        }else{selected <- NULL}
        selectizeInput("SplineTerms","Spline Terms in Model (non cyclic)",
                       choices=terms,selected=selected,multiple=T)
      })
      
      output$SplineTermsCyclic <- renderUI({
        terms <- colnames(RAW)
        if(!is.null(Read_Settings())){
          selected <- Read_Settings()[["SplineTermsCyclic"]]
        }else{selected <- c("Day365")}
        selectizeInput("SplineTermsCyclic","Cyclical Spline Terms in Model",
                       choices=terms,selected=selected,multiple=T)
      })
      
      RAWReduced <- reactive({
        r <- input$response
        linear <- input$LinearTerms
        spline <- input$SplineTerms
        splineCC <- input$SplineTermsCyclic
        factors <- input$FactorTerms
        additional <- c("CustomerCarrier","EntryDate","CustomerCCode",
                        "CarrierTCode","OrigLongitude","OrigLatitude","LoadMiles",
                        "DestLongitude","DestLatitude","loadnum","NumericDate","Day365")
        kept <- as.character(unique(c(r,linear,spline,splineCC,factors,additional)))
        data <- RAW[,kept]
        return(data)
      })
      
      
      
      
      DATA <- reactive({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Subsetting the Data',
                     detail = 'Hold Tight...')
        on.exit(progress$close())

        data <- RAWReduced()
        if(!is.null(input$SelectDestCounties)){
          countiesDestination <- input$SelectDestCounties
          selectDestination <- map("county",regions = countiesDestination,fill=T,plot=F)
          indexDestCounty <- map.where(selectDestination,x=RAW$DestLongitude,y=RAW$DestLatitude)
        }else{indexDestCounty <- rep(NA,nrow(RAW))}
          
        if(!is.null(input$SelectOrigCounties)){
          countiesOrigin <- input$SelectOrigCounties
          selectOrigin <- map("county",regions = countiesOrigin,fill=T,plot=F)
          indexOrigCounty <- map.where(selectOrigin,x=RAW$OrigLongitude,y=RAW$OrigLatitude)
        }else{indexOrigCounty <- rep(NA,nrow(RAW))}
        
        if(!is.null(input$SelectDestCircles)){
          circles <- input$SelectDestCircles
          circles <- lapply(circles,function(b){
            b <- strsplit(b,":")
            b <- unlist(b)
            x_center <- as.numeric(b[1])
            y_center <- as.numeric(b[2])
            r <- radius_xyunits(miles=as.numeric(b[3]))
            return(data.frame(x_center=x_center,y_center=y_center,r=r))
          })
          
         idx <- lapply(circles,function(b){
           x=RAW$DestLongitude-b$x_center
           y=RAW$DestLatitude-b$y_center
           dist <- sqrt(x^2+y^2)
           idx <- dist<=b$r
           return(idx)
         })
         
         idx <- matrix(unlist(idx),ncol=length(idx),byrow=F)
         indexDestCircle <- apply(idx,1,any)
         indexDestCircle[is.na(indexDestCircle)] <- FALSE
        }else{indexDestCircle <- rep(FALSE,nrow(RAW))}
        
        if(!is.null(input$SelectOrigCircles)){
          circles <- input$SelectOrigCircles
          circles <- lapply(circles,function(b){
            b <- strsplit(b,":")
            b <- unlist(b)
            x_center <- as.numeric(b[1])
            y_center <- as.numeric(b[2])
            r <- radius_xyunits(miles=as.numeric(b[3]))
            return(data.frame(x_center=x_center,y_center=y_center,r=r))
          })
          
          idx <- lapply(circles,function(b){
            x=RAW$OrigLongitude-b$x_center
            y=RAW$OrigLatitude-b$y_center
            dist <- sqrt(x^2+y^2)
            idx <- dist<=b$r
            return(idx)
          })
          
          idx <- matrix(unlist(idx),ncol=length(idx),byrow=F)
          indexOrigCircle <- apply(idx,1,any)
          indexOrigCircle[is.na(indexOrigCircle)] <- FALSE
        }else{indexOrigCircle <- rep(FALSE,nrow(RAW))}
        
        
        a <- RAW$Orig3DigZip %in% input$OrigZip3
        b <- RAW$Dest3DigZip %in% input$DestZip3
        e <- RAW$OrigCity %in% input$OrigCity
        f <- RAW$DestCity %in% input$DestCity
        orig <- ((a |  e) | indexOrigCircle)
        dest <- ((b |  f) | indexDestCircle)
        r <- input$response
        idxx <- (!is.na(indexOrigCounty) | orig) & (!is.na(indexDestCounty) | dest) & (!is.infinite(RAW[,r]) & !is.na(RAW[,r]))
        
        ####non dplyr version (consider for stability)
        #SELECTED <- RAW[idxx,]

        ####dplyr version consider for speed
        SELECTED <- data %>% filter(idxx)
        SELECTED <- as.data.frame(SELECTED)
        
        NOTSELECTED_IDX <- !idxx
        return(list(SELECTED=SELECTED,NOTSELECTED_IDX=NOTSELECTED_IDX))
      })
      

      

      ###########################################################
      #######Select the Appropriate Time Window
      ###########################################################
    
      
      PERCENTILES <- reactive({
        if(input$FilterDate==FALSE){return(NULL)}
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Removing Outliers',
                     detail = 'Computing Percentiles')
        on.exit(progress$close())

        SELECTED <- DATA()[["SELECTED"]]
        input$applyDygraph
        isolate(df <- input$dfspline)
        isolate(df_fixed <- input$LambdaFixed)
        isolate(tau_lower <- input$lowerTau)
        isolate(tau_center <- input$centralTau)
        isolate(tau_upper <- input$upperTau)
        r <- input$response
        CLEAN <- SELECTED[,c(r,"EntryDate")]
        CLEAN <- CLEAN[!is.infinite(CLEAN[,r]) & !is.na(CLEAN[,r]),]
        CLEAN <- CLEAN %>%  arrange(EntryDate)
        quantiles <- data.frame()
        y=CLEAN[,r]
        x=as.numeric(CLEAN$EntryDate)
        xy=data.frame(y=y,x=x)
        xy <- xy[complete.cases(xy),]
        y=xy$y
        x=xy$x
        params <- c(tau_lower,tau_center,tau_upper)
        for(tau in params){
          if(isolate(input$doEstimation==T)){
            g <- function(lam,y,x,tau) AIC(rqss(y ~ qss(x, lambda = lam),tau=tau),k = -1)
            lamstar <- optimize(g, interval = c(df[1], df[2]), x = x, y = y, tau= tau)
            fit <- quantreg::rqss(y ~ qss(x, lambda = lamstar$min),tau=tau)
          }else{
            fit <- quantreg::rqss(y ~ qss(x, lambda = df_fixed),tau=tau)
          }
          
          quantile.fit <- predict(fit,newdata=data.frame(y=y,x=x))
          quant <- data.frame(EntryDate=CLEAN$EntryDate,fit=quantile.fit)
          quant <- tapply(quant$fit,quant$EntryDate,unique)
          quant <- data.frame(EntryDate=as.Date(names(quant)),fit=quant,quantile=tau)
          rownames(quant)=NULL
          quant <- quant[order(quant$EntryDate),]
          quantiles <- rbind(quantiles,quant)
        }
        rm(fit)
        EntryDate<- unique(quantiles$EntryDate)
        quantiles <- unstack(quantiles,fit~quantile,data=quantiles)
        quantiles$EntryDate <- EntryDate
        colnames(quantiles) <- c(paste0(params*100,"th"),"Date")
        return(quantiles)
      })
      

      output$dygraph_cut <- renderDygraph({
        if(input$FilterDate==FALSE){return(NULL)}
        r <- input$response
        quantiles <- PERCENTILES()
        plot_dat <- xts(quantiles[,-4],quantiles[,4])
        dygraph(plot_dat,main=paste(paste(colnames(quantiles)[1:3],collapse=" "),"Percentiles of",input$response)) %>%
          dySeries(c(colnames(quantiles)[1:3]),label=paste("Median",r)) %>%
          dyAxis("y",label=r) %>%
          dyRangeSelector()
      })
      
      
      
      ###########################################################
      #######Remove fixed rate observations (if desired)
      ###########################################################
      
      output$UpperLower <- renderUI({
        if(input$FilterDate==FALSE | input$EnableSelect==FALSE){return(NULL)}
        r <- input$response
        DATA <- DATA()[["SELECTED"]]
        low <- min(DATA[,r],na.rm = TRUE)
        low <- floor(low*100)/100
        high <- max(DATA[,r],na.rm = TRUE)
        high <- ceiling(high*100)/100
        sliderInput("UpperLower",paste(r,"Limits"),min=low,max=high,value=c(0,high))
      })
      
      
      
      DATAWINDOW <- reactive({
        SELECTED <- DATA()[["SELECTED"]]
        r <- input$response
        if(input$FilterDate==FALSE){return(list(SELECTED=SELECTED))}
        input$applyDygraph #this is the action button for the percentiles
        input$applyUpperLower #this is the action button for a RAW RPM filter
        isolate(if(is.null(input$dygraph_cut_date_window)){return(list(SELECTED=SELECTED))})
        isolate(min_dte <- input$dygraph_cut_date_window[1])
        isolate(max_dte <- input$dygraph_cut_date_window[2])
        low <-input$UpperLower[1]
        high <-input$UpperLower[2]
        SELECTED <- SELECTED[(EntryDate>=min_dte & EntryDate<=max_dte),]
        idx <- (SELECTED[,r] >= low) & (SELECTED[,r] <= high)
        SELECTED <- SELECTED[idx,]
        return(list(SELECTED=SELECTED))
      })
      

      
      
      RemoveGroups<- reactiveValues(x=NULL, y=NULL)
      RemoveGroupsHover <- reactiveValues(x=NULL, y=NULL)
      
      ###listen for clicks
      observe({
        # Initially will be empty
        if (is.null(input$RemoveGroups)){
          return()
        } else{
          isolate(RemoveGroups$x <- input$RemoveGroups$x)
          isolate(RemoveGroups$y <- input$RemoveGroups$y)
        }
      })
      
      ###listen for hover
      observe({
        # Will be NULL when no hover
        if (is.null(input$RemoveGroupsHover)){
          return()
        } else{
          isolate(RemoveGroupsHover$x <- input$RemoveGroupsHover$x)
          isolate(RemoveGroupsHover$y <- input$RemoveGroupsHover$y)
        }
      })
      
      ClickRemovalPoints<- reactive({
        if(input$FilterDate==FALSE | input$EnableSelect==FALSE){return(NULL)}
        SELECTED <- DATAWINDOW()[["SELECTED"]]
        if(is.null(RemoveGroups$x)){return(NULL)}
        r <- input$response
        train <- data.frame(as.numeric(SELECTED$EntryDate),SELECTED[,r])
        idx <- complete.cases(train)
        train <- train[idx,]
        lower <- unlist(apply(train,2,min))
        upper <- unlist(apply(train,2,max))
        scaling <- upper-lower
        train <- scale(train,center=F,scale=scaling)
        cl <- factor(1:nrow(train))
        test <- data.frame(RemoveGroups$x,RemoveGroups$y)
        test <- scale(test,center=F,scale=scaling)
        id <- knn1(train, test, cl)
        id <- as.numeric(id)
        p <- SELECTED$CustomerCarrier[idx]
        p <- p[id]
        obs_id <- 1:nrow(SELECTED)
        obs_id <- obs_id[idx]
        obs_id <- obs_id[id]
        return(list(p=p,lower=lower,upper=upper,id=obs_id))
      })
      
      output$RemoveCustomerCarrier <- renderUI({
        if(input$FilterDate==FALSE | input$EnableSelect==FALSE){return(NULL)}
        SELECTED <- DATAWINDOW()[["SELECTED"]]
        pick <- SELECTED$CustomerCarrier
        remove <- ClickRemovalPoints()[["p"]]
        if(!("Customer Carrier" %in% isolate(input$TypeRemoval))){remove=NULL}
        isolate(selected <- input$RemoveCustomerCarrier)
        selected <- selected[!is.null(selected)]
        selected <- c(selected,remove)
        selected <- unique(selected)
        selected <- selected[!is.null(selected)]
        selectizeInput("RemoveCustomerCarrier","Customer Carrier Groups to Remove",choices=pick,selected=selected,multiple=T)
      })
      
      
      output$RemoveIndividual <- renderUI({
        if(input$FilterDate==FALSE | input$EnableSelect==FALSE){return(NULL)}
        SELECTED <- DATAWINDOW()[["SELECTED"]]
        pick <- SELECTED$loadnum
        remove <- ClickRemovalPoints()[["id"]]
        remove <- pick[remove]
        if(!("Individual Load" %in% isolate(input$TypeRemoval))){remove=NULL}
        isolate(selected <- input$RemoveIndividual)
        selected <- selected[!is.null(selected)]
        selected <- c(selected,remove)
        selected <- unique(selected)
        selected <- selected[!is.null(selected)]
        selectizeInput("RemoveIndividual","Load Number",choices=pick,selected=selected,multiple=T)
      })
      
      
      
      output$RemoveCustomerCarrierHover <- renderText({
        if(input$FilterDate==FALSE | input$EnableSelect==FALSE){return(NULL)}
        SELECTED <- DATAWINDOW()[["SELECTED"]]
        if(is.null(RemoveGroupsHover$x)){return("Mouse Hover:")}
        r <- input$response
        train <- data.frame(as.numeric(SELECTED$EntryDate),SELECTED[,r])
        idx <- complete.cases(train)
        train <- train[idx,]
        lower <- unlist(apply(train,2,min))
        upper <- unlist(apply(train,2,max))
        scaling <- upper-lower
        train <- scale(train,center=F,scale=scaling)
        #cl <- factor(SELECTED$CustomerCarrier[idx])
        cl <- factor(1:nrow(train))
        test <- data.frame(RemoveGroupsHover$x,RemoveGroupsHover$y)
        test <- scale(test,center=F,scale=scaling)
        id <- knn1(train, test, cl)
        id <- as.numeric(id)
        p <- SELECTED$CustomerCarrier[idx]
        p <- p[id]
        obs_id <- 1:nrow(SELECTED)
        obs_id <- obs_id[idx]
        obs_id <- obs_id[id]
        obs <- SELECTED$loadnum[obs_id]
        if (length(p)==0){p <-NULL}
        return(paste("Mouse Hover:",p,paste0("Load:",obs)))
      })
      
      
      
      
      DATAFILTERED <- reactive({
        SELECTED <- DATAWINDOW()[["SELECTED"]]
        if(input$FilterDate==FALSE){return(list(KEEP=SELECTED,TOSS=NULL))}
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Removing Outliers',
                     detail = 'Picking Off Bad Observations')
        on.exit(progress$close())

        r <- input$response
        pull <- input$RemoveCustomerCarrier
        pull2 <- input$RemoveIndividual
        idx_toss <- (SELECTED$CustomerCarrier %in% pull) | (SELECTED$loadnum %in% pull2)
        if(("Lower Quantile" %in% input$QuantileFilter) | ("Upper Quantile" %in% input$QuantileFilter)){
          quantiles <- PERCENTILES()
          ids <- colnames(quantiles)
#           QUANT <-  select_(SELECTED,.dots = c("EntryDate",r)) %>% 
#             left_join(quantiles,by=c("EntryDate"="Date"))
          QUANT <- SELECTED[,c("EntryDate",r)]
          QUANT <- base::merge(QUANT,quantiles,by.x="EntryDate",by.y="Date",all.x=TRUE)
          
          
          if(c("Lower Quantile") %in% input$QuantileFilter){
            idx_toss <- idx_toss | (QUANT[,r]<=QUANT[ids[1]])
          }

          if(c("Upper Quantile") %in% input$QuantileFilter){
            idx_toss <- idx_toss | (QUANT[,r]>=QUANT[ids[3]])
          }
        }
        
        KEEP <- SELECTED[!idx_toss,]
        TOSS <- SELECTED[idx_toss,]
        return(list(KEEP=KEEP,TOSS=TOSS))
      })
      
      
      output$RemovalPlot <- renderPlot({
        if(input$FilterDate==FALSE){return(NULL)}
        KEEP <- DATAFILTERED()[["KEEP"]]
        TOSS <- DATAFILTERED()[["TOSS"]]
        LIMITS <- rbind(KEEP,TOSS)
        r <- input$response
        plot(x=LIMITS$EntryDate,y=LIMITS[,r],type="n",pch=19,col="black",
             xlab="Date",ylab=r)
        
        if("Percentiles" %in% input$plotControls){
          quantiles <- PERCENTILES()
          cord.x <- c(quantiles[,4],quantiles[nrow(quantiles):1,4])
          cord.y <- c(quantiles[,3],quantiles[nrow(quantiles):1,1])
          polygon(cord.x,cord.y,col='grey95')
        }
        
        if("Kept" %in% input$plotControls){
          points(x=KEEP$EntryDate,y=KEEP[,r],pch=19,col="black")
        }
        
      if("Removed" %in% input$plotControls){
        points(x=TOSS$EntryDate,y=TOSS[,r],type="p",pch=19,col="grey75")
        }

        legend("topright",c("Removed","Kept"),pch=19,col=c("grey75","black"))
      })
      
      DATAFILTERED2 <- reactive({
        data <- DATAFILTERED()[["KEEP"]]
        idx <- complete.cases(data)
        data <- data[idx,]
        return(list(KEEP=data))
      })
      
      mileECDF <- reactive({
        data <- DATAFILTERED2()[["KEEP"]]###data brought in after filtering is complete
        Fn <- ecdf(data$LoadMiles)
        return(Fn)
      })
      
      
      ###########################################################
      #######Tab Panel 2:  Quick quote
      ###########################################################
      output$mile15_QUICK<- renderValueBox({
        Fn <- mileECDF()
        pct <- 0.15
        valueBox(
          paste0(quantile(Fn,pct), " mi"), paste0(pct*100,"th Mileage Percentile"), icon = icon("list"),
          color = "purple"
        )
      })
      
      output$mile50_QUICK<- renderValueBox({
        Fn <- mileECDF()
        pct <- 0.5
        valueBox(
          paste0(quantile(Fn,pct), " mi"), paste0(pct*100,"th Mileage Percentile"), icon = icon("list"),
          color = "purple"
        )
      })
      
      output$mile85_QUICK<- renderValueBox({
        Fn <- mileECDF()
        pct <- 0.85
        valueBox(
          paste0(quantile(Fn,pct), " mi"), paste0(pct*100,"th Mileage Percentile"), icon = icon("list"),
          color = "purple"
        )
      })
      
      
      MODELFIT_QUICK <- reactive({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Modeling',
                     detail = 'Fitting Model')
        on.exit(progress$close())
        
        data <- DATAFILTERED2()[["KEEP"]]###data brought in after filtering is complete
        r <- input$response
        f <- as.formula(paste(r,"1",sep="~"))  ###place holder
        f_add <- paste0(".~.+",c("NumericDate"))
        f <- do.call("update",list(f,f_add))
        eval(parse(text=paste0("f_add=.~.+s(","Day365",",bs=\"cc\")")))
        f <- do.call("update",list(f,f_add))
        fit <- modelCPDS(f=f,data=data,kernel="Generalized Additive Model",gamma=1.4)
        return(fit)
      })
      
      PREDICTIONDATA_QUICK <- reactive({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Modeling',
                     detail = 'Computing Predictions')
        on.exit(progress$close())
        
        data <- DATAFILTERED2()[["KEEP"]]
        fit <- MODELFIT_QUICK()
        date_window <- input$DateRange
        terms <- as.character(fit$pred.formula)[2]
        terms <- unlist(strsplit(terms," + ",fixed=T))
        PredTerms <- terms[!(terms %in% c("NumericDate","Day365"))]###get rid of date terms
        DateTerms <- terms[(terms %in% c("NumericDate","Day365"))]###get rid of date terms
        predDays <- difftime(date_window[2],date_window[1],units="days")
        date_sequence <- date_window[1]+1:predDays
        PredData <- data.frame(EntryDate=date_sequence)
        
        ###construct prediction matrix terms
        for(i in DateTerms){
          if(i=="NumericDate"){
            PredData <- cbind(PredData,NumericDate=as.numeric(date_sequence))
          }
          if(i=="Day365"){
            PredData <- cbind(PredData,Day365=as.numeric(format(date_sequence,format="%j")))
          }
        }
        
        preds <- predict(fit,newdata=PredData,se.fit=T)
        y_hat <- preds$fit
        y_se <- sqrt(preds$se.fit^2+fit$sig2)
        LCL <- y_hat+qnorm(0.15)*y_se
        UCL <- y_hat+qnorm(0.85)*y_se
        response <- as.character(formula(fit))[2]
        eval(parse(text=paste0("prediction_data <- data.frame(",response,"=y_hat,LCL=LCL,UCL=UCL,PredData)")))
        colnames(prediction_data)[c(2,3)] <- c(paste0("FCST",c(0.15,0.85)*100,"th"))
        
        
        ###now we have to generate adjusted response data for this to work
        ###since there are nusiance factors that need to be integrated out
        ids <- length(PredTerms)
        data2 <- data
        
        ####Run the partial predictions
        y_hat <- predict(fit,newdata=data)
        y <- data[,as.character(formula(fit))[2]]
        residual <- y-y_hat
        y_partial <- y_hat
        y_residual <- y_partial+residual
        observed_data <- data.frame("y_partial"=as.numeric(y_partial),
                                    "y_residual"=as.numeric(y_residual),
                                    "y_hat"=as.numeric(y_hat),
                                    "residual"=as.numeric(residual),
                                    data)
        
        ###now we regress the quantiles of these partials for the historical data
        df <- input$dfspline
        df_fixed <- input$LambdaFixed
        y=observed_data$y_residual
        x=as.numeric(data$EntryDate)
        xy=data.frame(y=y,x=x)
        xy <- xy[complete.cases(xy),]
        y=xy$y
        x=xy$x
        tau_lower <- 0.15
        tau_center <- 0.5
        tau_upper <- 0.85
        params <- c(tau_lower,tau_center,tau_upper)
        for(tau in params){
          if(isolate(input$doEstimation==T)){
            g <- function(lam,y,x,tau) AIC(rqss(y ~ qss(x, lambda = lam),tau=tau),k = -1)
            lamstar <- optimize(g, interval = c(df[1], df[2]), x = x, y = y, tau= tau)
            fitq <- quantreg::rqss(y ~ qss(x, lambda = lamstar$min),tau=tau)
          }else{
            fitq <- quantreg::rqss(y ~ qss(x, lambda = df_fixed),tau=tau)
          }
          x2=as.numeric(seq(min(data$EntryDate), max(data$EntryDate), "days"))
          
          
          quantile.fit <- predict(fitq,newdata=data.frame(x=x2))
          quant <- data.frame(fit=quantile.fit)
          if(tau==params[1]){quantiles <- quant}else{quantiles <- data.frame(quantiles,quant)}
        }
        rm(fitq)
        quantiles <- data.frame(EntryDate=as.Date(seq(min(data$EntryDate), max(data$EntryDate), "days")),quantiles)
        colnames(quantiles) <- c("EntryDate",paste0("HIST",params*100,"th"))
        
        ####done with quantiles
        #         observed_summary <- observed_data %>% 
        #         group_by(EntryDate) %>%
        #         summarise(Prediction = mean(y_partial,na.rm=T))
        observed_summary <- tapply(observed_data$y_partial,observed_data$EntryDate,mean,na.rm=T)
        observed_summary <- data.frame(EntryDate=as.Date(names(observed_summary)),Prediction=observed_summary)
        rownames(observed_summary)=NULL
        #observed_summary <- left_join(observed_summary,quantiles)
        observed_summary <- base::merge(observed_summary,quantiles,all.x=TRUE)
        event <- prediction_data$EntryDate[1]-0.5
        out <- list(prediction_data=prediction_data,
                    observed_data=observed_data,
                    event=event,
                    observed_summary=observed_summary)
        return(out)
      })
      
      
      TransactionalVolume_QUICK <- reactive({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Modeling',
                     detail = 'Predicting Transactional Volume')
        on.exit(progress$close())
        
        data <- DATAFILTERED2()[["KEEP"]]###data brought in after filtering is complete
        date_window <- input$DateRange
        predDays <- difftime(date_window[2],date_window[1],units="days")
        date_sequence <- date_window[1]+1:predDays
        volume <- tapply(data$EntryDate,data$EntryDate,length)
        volume <- data.frame(EntryDate=as.Date(names(volume)),TransVolume=volume)
        rownames(volume)=NULL
        
        interval <- difftime(max(volume$EntryDate),min(volume$EntryDate),units = "days")
        dateseq <- min(volume$EntryDate)+1:interval
        JoinDat <- data.frame(EntryDate =dateseq)
        #volume <- JoinDat %>% left_join(volume)
        volume <- base::merge(JoinDat,volume,all.x=TRUE)
        volume$TransVolume[is.na(volume$TransVolume)] <- 0
        volume <- xts(volume[,"TransVolume",drop=F],volume$EntryDate)
        
        ###gam path
          dat <- data.frame(coredata(volume),Day365=as.numeric(format(index(volume),format="%j")),
                            NumericDate=as.numeric(index(volume)),idx=1:nrow(volume))
          dat$week_groups <- paste(format(index(volume),format="%Y"),format(index(volume),format="%W"),sep="-")
          out_idx <- lapply(unique(dat$week_groups),function(b){
            tmp <- dat[dat$week_groups==b,]
            return(tmp$idx[which.max(tmp$TransVolume)])
          })
          out_idx <- unlist(out_idx)
          dat <- dat[out_idx,] ###select maximum
          fit <- mgcv::gam(TransVolume~s(Day365,bs="cc")+NumericDate,data=dat)
          PredData <- data.frame(EntryDate=date_sequence,Day365=as.numeric(format(date_sequence,format="%j")),NumericDate=as.numeric(date_sequence))
          pred_volume <- predict(fit,newdata=PredData)
          pred_volume <- data.frame(TransFcst=as.numeric(pred_volume))

        pred_volume <- xts(pred_volume,date_sequence)
        return(list(volume=volume,pred_volume=pred_volume))
      })
      
      VolumeDataPrep_QUICK <- reactive({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Modeling',
                     detail = 'Merging Data')
        on.exit(progress$close())
        
        preds <- PREDICTIONDATA_QUICK()[["prediction_data"]]
        data <- PREDICTIONDATA_QUICK()[["observed_summary"]]
        event <- PREDICTIONDATA_QUICK()[["event"]]
        volume <- TransactionalVolume_QUICK()[["volume"]]
        pred_volume <- TransactionalVolume_QUICK()[["pred_volume"]]
        fit <- MODELFIT_QUICK()
        response <- as.character(formula(fit))[2]
        idx_date_data <- colnames(data) %in% c("EntryDate")
        idx_date_preds <- colnames(preds) %in% c("EntryDate")
        preds <- xts(preds[,c(1,2,3),drop=F],preds[,idx_date_preds])
        data <- xts(data[,c(3,4,5),drop=F],data[,idx_date_data])
        vol_int_rate_fcst <- numeric(length=ncol(coredata(preds)))
        names(vol_int_rate_fcst) <- colnames(coredata(preds))
        for (i in 1:length(vol_int_rate_fcst)){
          vol_int_rate_fcst[i] <- weighted.mean(coredata(preds)[,i],coredata(pred_volume))
        }
        vol_int_rate_fcst <- round(vol_int_rate_fcst,2)
        series <- cbind(data,preds,volume,pred_volume)
        idx <- colnames(coredata(data))
        for(i in idx){
          series[index(series)<index(preds)[1],i] <- na.approx(series[index(series)<index(preds)[1],i])
        }
        
        name <- paste0("Volume Integrated Quote: $",vol_int_rate_fcst[1]," ($",vol_int_rate_fcst[2],", $",vol_int_rate_fcst[3],") Per Mile")
        return(list(series=series,vol_int_rate_fcst=vol_int_rate_fcst,event=event,
                    response=response,data=data,preds=preds,volume=volume,
                    name=name))
      })
      

      HistoricalData_QUICK <- reactive({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Modeling',
                     detail = 'Merging Data')
        on.exit(progress$close())
        
        series <- VolumeDataPrep_QUICK()[["series"]]
        response <- VolumeDataPrep_QUICK()[["response"]]
        vol_int_rate_fcst <- VolumeDataPrep_QUICK()[["vol_int_rate_fcst"]]
        data <- VolumeDataPrep_QUICK()[["data"]]
        preds <- VolumeDataPrep_QUICK()[["preds"]]
        volume <- VolumeDataPrep_QUICK()[["volume"]]
        event <- VolumeDataPrep_QUICK()[["event"]]
        name <- VolumeDataPrep_QUICK()[["name"]]
        date <- index(series)
        year <- format(date,format="%Y")
        month <- format(date,format="%m")
        day <- format(date,format="%d")
        data <- as.data.frame(coredata(series))
        idx <- !is.na(data$TransFcst)###get the forecast portion
        fcst <- data$TransFcst[idx]
        data$TransFcst[!idx] <- 0
        for(i in 1:length(date[!idx])){
          id <- day[idx]==day[i] & month[idx]==month[i]
          if(any(id)){data$TransFcst[i] <- fcst[id]}
        }
        data$TransFcst[!idx] <- na.approx(data$TransFcst[!idx])
        rm <- colnames(data) %in% "TransVolume"
        data <- data[,!rm]
        series <- xts(data,date)
        ###now get the date window to do a historical volume pass
        ###loop over all of the past values
        dte_window <- c(min(date[idx]),max(date[idx]))
        loop <- as.numeric(format(dte_window[1],format="%Y"))-as.numeric(format(min(index(series)),format="%Y"))
        quote <- data.frame()
        for(i in 1:loop){
          lower <- paste(as.numeric(format(dte_window[1],format="%Y"))-i,format(dte_window[1],format="%m-%d"),sep="-")
          upper <- paste(as.numeric(format(dte_window[2],format="%Y"))-i,format(dte_window[2],format="%m-%d"),sep="-")
          wdow <- paste(lower,upper,sep="::")
          tmp <- series[wdow]
          if(i==loop & (min(index(tmp))>as.Date(lower))){break}###dont add value if not complete cycle
          quote <- rbind(quote,data.frame(
            "StartDate"=as.Date(lower),
            "EndDate"=as.Date(upper),
            "MidPoint"=as.Date(lower)+difftime(as.Date(upper),as.Date(lower))/2,
            "WeightedY_LCL"=weighted.mean(tmp[,1],tmp$TransFcst,na.rm = T),
            "WeightedY"=weighted.mean(tmp[,2],tmp$TransFcst,na.rm = T),
            "WeightedY_UCL"=weighted.mean(tmp[,3],tmp$TransFcst,na.rm = T)
          ))
        }
        
        
        ###add on the predicted quote
        quote <- rbind(data.frame(
          "StartDate"=dte_window[1],
          "EndDate"=dte_window[2],
          "MidPoint"=dte_window[1]+difftime(dte_window[2],dte_window[1])/2,
          "WeightedY_LCL"=as.numeric(vol_int_rate_fcst[2]),
          "WeightedY"=as.numeric(vol_int_rate_fcst[1]),
          "WeightedY_UCL"=as.numeric(vol_int_rate_fcst[3])),
          quote)
        
        colnames(quote)[c(4,5,6)] <- gsub("HIST","Percentile.",colnames(series)[c(1:3)])
        
        ####fix up series to add a section of 0's if needed to pad the plot  in the next step
        full_date_run <- data.frame(EntryDate=as.Date(seq(min(index(series)),max(index(series)),"days")),padit=NA)
        padit <- xts(full_date_run[,2,drop=F],full_date_run[,1])
        series <- cbind(series,padit)
        series$TransFcst[is.na(series$TransFcst)] <- 0
        series <- series[,-c(8)]
        return(list(series=series,vol_int_rate_fcst=vol_int_rate_fcst,event=event,
                    response=response,data=data,preds=preds,volume=volume,
                    name=name,quote=quote))
      })
      
      
      
      output$Historical_QUICK <- renderDygraph({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Modeling',
                     detail = 'Plotting')
        on.exit(progress$close())
        
        series <- HistoricalData_QUICK()[["series"]]
        #series <- series[,c(2,4:7)]
        p <- colnames(series)
        response <- HistoricalData_QUICK()[["response"]]
        vol_int_rate_fcst <- HistoricalData_QUICK()[["vol_int_rate_fcst"]]
        data <- HistoricalData_QUICK()[["data"]]
        preds <- HistoricalData_QUICK()[["preds"]]
        volume <- HistoricalData_QUICK()[["volume"]]
        event <- HistoricalData_QUICK()[["event"]]
        name <- HistoricalData_QUICK()[["name"]]
        dygraph(series,name) %>%
          dySeries(p[c(1,2,3)],label="Historical") %>%
          dySeries("TransFcst",label="Repeated Volume",
                   axis='y2',stepPlot = TRUE, fillGraph = TRUE) %>%  
          dySeries(p[c(5,4,6)],label="FCST") %>% ###turned off error bars... if desired but might crash
          dyAxis("y",label=response) %>%
          dyAxis("y2", label = "Transacitonal Volume", 
                 independentTicks = TRUE, valueRange = c(0, max(volume))) %>%
          dyRoller(rollPeriod = 1) %>%
          dyEvent(date = event, "Observed/Predicted", labelLoc = "bottom")
      })
      
      
      
      output$HistVolIntegrated_QUICK<- renderDygraph({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Modeling',
                     detail = 'Plotting')
        on.exit(progress$close())
        
        quote <- HistoricalData_QUICK()[["quote"]]
        event <- HistoricalData_QUICK()[["event"]]
        response <- HistoricalData_QUICK()[["response"]]
        quote <- xts(quote[,c(4:6),drop=F],quote$MidPoint)
        p <- colnames(quote)
        dygraph(quote,paste(p)) %>%
          dySeries(p,label=response) %>%
          dyAxis("y",label=response) %>%
          dyOptions(drawPoints = TRUE, pointSize = 5) %>%
          dyEvent(date = event, "Observed/Predicted", labelLoc = "bottom")
      })
      
      tdat2_QUICK <- reactive({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Modeling',
                     detail = 'Plotting')
        on.exit(progress$close())
        table <- HistoricalData_QUICK()[["quote"]]
        idx <- sapply(table,class)
        idx <- idx %in% c("numeric","array")
        table[,idx] <- round(table[,idx],2)
        pull <- colnames(table) %in% "MidPoint"
        Fn <- mileECDF()
        pct <- 0.5
        table[,pull] <- quantile(Fn,pct)
        colnames(table)[pull] <- paste0(pct*100,"th Percentile Mileage")
        return(table)
      })
      
      output$HistVolIntegratedTable_QUICK<- DT::renderDataTable(
        DT::datatable(tdat2_QUICK(),filter='bottom',extensions = 'TableTools',
                      options=list(scrollX=TRUE,    dom = 'T<"clear">lfrtip',
                                   tableTools = list(sSwfPath = copySWF())))
      )
      

      
      ###########################################################
      #######Tab Panel 3:  Advanced Modeling
      ###########################################################
      output$ModelFamily <- renderUI({
        selected <- c("Generalized Additive Model")
        if(!is.null(Read_Settings()[["ModelFamily"]])){
          selected <- Read_Settings()[["ModelFamily"]]
        }
        
        selectInput("ModelFamily","Modeling Kernel",c("Generalized Additive Model"),selected=selected)
      })
      
      output$ConfLimits <- renderUI({
        value <- c(0.15,0.85)
        if(!is.null(Read_Settings()[["ConfLimits"]])){
          value <- Read_Settings()[["ConfLimits"]]
        }
        sliderInput("ConfLimits","Model Confidince Intervals",0,1,value=value)
      })
      



      MODELFIT <- reactive({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Modeling',
                     detail = 'Fitting')
        on.exit(progress$close())
        
        data <- DATAFILTERED2()[["KEEP"]]###data brought in after filtering is complete
        r <- input$response
        linear <- input$LinearTerms
        spline <- input$SplineTerms
        splineCC <- input$SplineTermsCyclic
        factors <- input$FactorTerms
        
        if(is.null(linear) & is.null(spline) & is.null(splineCC)){return(NULL)}
        f <- as.formula(paste(r,"1",sep="~"))  ###place holder
        
        for(t in factors){
          f_add <- paste0(".~.+as.factor(",t,")")
          f <- do.call("update",list(f,f_add))
        }
        
        
        for(t in linear){
          f_add <- paste0(".~.+",t)
          f <- do.call("update",list(f,f_add))
        }
        
        for(t in spline){
          f_add <- paste0(".~.+s(",t,")")
          f <- do.call("update",list(f,f_add))
        }
        
        for(t in splineCC){
          eval(parse(text=paste0("f_add=.~.+s(",t,",bs=\"cc\")")))
          f <- do.call("update",list(f,f_add))
        }
        fit <- modelCPDS(f=f,data=data,kernel=input$ModelFamily,gamma=1.4)
        return(fit)
      })
      
      output$ModelSummary <- renderPrint({
        summary <- summary(MODELFIT())
        if (!is.null(summary)) {
          return(print(summary))
        }
      })
      
      output$ModelPlot <- renderPlot({ 
        fit <- MODELFIT()
        mgcv::plot.gam(fit,pages=1,all.terms=T)
      })
      
      output$ModelDiagnostics <- renderPlot({
        gam.check(MODELFIT())
      })
      

      ###########################################################
      #######Model Predictions
      ###########################################################

      output$PredicitonRangesLower <- renderUI({
        fit <- MODELFIT()
        data <- DATAFILTERED2()[["KEEP"]]###data brought in after filtering is complete
        terms <- as.character(fit$pred.formula)[2]
        terms <- unlist(strsplit(terms," + ",fixed=T))
        terms <- terms[!(terms %in% c("NumericDate","Day365"))]###get rid of date terms they are dealt with elsewhere
        if(length(terms)==0){return(NULL)}
        
        myUIs<- lapply(1:length(terms), function(i) {
          inputname <- paste("PredictorTerms_range_lower_", terms[i], sep="")
          u <- max(data[,terms[i]])
          l <- min(data[,terms[i]])
          
          ####do the update here on the first pass through
          isolate({
            if(!is.null(ModelImageUpdate[[inputname]])){
              l <- ModelImageUpdate[[inputname]]
            }
          })
          
          numericInput(inputname,paste0("Lower Y Range:",terms[i]),value=l)
        })
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, myUIs)
        
      })
      
      output$PredicitonRangesUpper <- renderUI({
        fit <- MODELFIT()
        data <- DATAFILTERED2()[["KEEP"]]###data brought in after filtering is complete
        terms <- as.character(fit$pred.formula)[2]
        terms <- unlist(strsplit(terms," + ",fixed=T))
        terms <- terms[!(terms %in% c("NumericDate","Day365"))]###get rid of date terms they are dealt with elsewhere
        if(length(terms)==0){return(NULL)}
        
        myUIs<- lapply(1:length(terms), function(i) {
          inputname <- paste("PredictorTerms_range_upper_", terms[i], sep="")
          u <- max(data[,terms[i]])
          l <- min(data[,terms[i]])
          
          
          ####do the update here on the first pass through
          isolate({
            if(!is.null(ModelImageUpdate[[inputname]])){
              u <- ModelImageUpdate[[inputname]]
            }
          })
          
          numericInput(inputname,paste0("Upper Y Range:",terms[i]),value=u)
        })
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, myUIs)
        
      })
      
      output$PredicitonPercentiles <- renderUI({
        fit <- MODELFIT()
        data <- DATAFILTERED2()[["KEEP"]]###data brought in after filtering is complete
        terms <- as.character(fit$pred.formula)[2]
        terms <- unlist(strsplit(terms," + ",fixed=T))
        terms <- terms[!(terms %in% c("NumericDate","Day365"))]###get rid of date terms they are dealt with elsewhere
        if(length(terms)==0){return(NULL)}
        
        myUIs<- lapply(1:length(terms), function(i) {
          inputname <- paste("PredictorTerms_percentile_", terms[i], sep="")
          ####do the update here on the first pass through
          value=0.5
          isolate({
            if(!is.null(ModelImageUpdate[[inputname]])){
              value <- ModelImageUpdate[[inputname]]
            }
          })

          sliderInput(inputname,paste0("Percentile:",terms[i]),value=value,min=0,max=1)
        })
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, myUIs)
        
      })
      

      output$PredictorEntryFixed <- renderUI({
        fit <- MODELFIT()
        data <- DATAFILTERED2()[["KEEP"]]###data brought in after filtering is complete
        terms <- as.character(fit$pred.formula)[2]
        terms <- unlist(strsplit(terms," + ",fixed=T))
        terms <- terms[!(terms %in% c("NumericDate","Day365"))]###get rid of date terms they are dealt with elsewhere
        if(length(terms)==0){return(NULL)}
        
        myUIs<- lapply(1:length(terms), function(i) {
          inputname <- paste("PredictorTerms_entryFixed_", terms[i], sep="")
          pick <- unique(data[,terms[i]])
          pick <- as.numeric(pick)
          pick <- pick[order(pick)]
          pick <- as.character(c("Automatic",pick))
          ####do the update here on the first pass through
          selected=c("Automatic")
          isolate({
            if(!is.null(ModelImageUpdate[[inputname]])){
              selected <- ModelImageUpdate[[inputname]]
            }
          })
          
          selectInput(inputname,paste0("Input Value:",terms[i]),choices=pick,selected=selected)
        })
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, myUIs)
        
      })
      
      
      output$DataType<- renderUI({
        fit <- MODELFIT()
        data <- DATAFILTERED2()[["KEEP"]]###data brought in after filtering is complete
        terms <- as.character(fit$pred.formula)[2]
        terms <- unlist(strsplit(terms," + ",fixed=T))
        terms <- terms[!(terms %in% c("NumericDate","Day365"))]###get rid of date terms they are dealt with elsewhere
        if(length(terms)==0){return(NULL)}
        
        myUIs<- lapply(1:length(terms), function(i) {
          inputname <- paste("PredictorTerms_dataType_", terms[i], sep="")
          pick <- as.character(c(unique(data$CustomerCCode),unique(data$CarrierTCode)))
          selected=NULL
          isolate({
            if(!is.null(ModelImageUpdate[[inputname]])){
              selected <- ModelImageUpdate[[inputname]]
            }
          })

          selectizeInput(inputname,paste0("Include in Plot (no selection includes all):",terms[i]),choices=pick,selected=selected,multiple=TRUE)
        })
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, myUIs)
        
      })
      
      
      # Insert the right number of plot output objects into the web page for the pencil graphs
      output$PredictionLevels <- renderUI({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Generating Inputs',
                     detail = 'Pencil Graphs')
        on.exit(progress$close())
        
        fit <- MODELFIT()
        data <- DATAFILTERED2()[["KEEP"]]###data brought in after filtering is complete
        date_window <- input$DateRange
        predDays <- difftime(date_window[2],date_window[1],units="days")
        date_sequence <- date_window[1]+1:predDays
        terms <- as.character(fit$pred.formula)[2]
        terms <- unlist(strsplit(terms," + ",fixed=T))
        terms <- terms[!(terms %in% c("NumericDate","Day365"))]###get rid of date terms they are dealt with elsewhere
        if(length(terms)==0){return(NULL)}
        term_class <- sapply(data[,terms,drop=F],class)
        
        for (i in 1:length(terms)) {
          # Need local so that each item gets its own number. Without it, the value
          # of i in the renderPlot() will be the same across all instances, because
          # of when the expression is evaluated.
          local({
          my_i <- i
          inputname <- paste("PredictorTerms_", terms[my_i], sep="")
          inputname_type <- paste("PredictorTerms_entryFixed_", terms[my_i], sep="")
          
          inputname_dataType <- paste("PredictorTerms_dataType_", terms[my_i], sep="")
          objs <- input[[inputname_dataType]]
          if(!is.null(objs)){
            idx_local <- (data$CustomerCCode %in% objs) | (data$CarrierTCode %in% objs) 
          }else{idx_local <- rep(TRUE,nrow(data))}
          data_local <- data[idx_local,]
          
          if(input[[inputname_type]]!="Automatic"){
            tmp <- input[[inputname_type]]
            class(tmp) <- class(data_local[,terms[my_i]])
            predictor <- eval(parse(text=paste0("data.frame(",terms[my_i],"=tmp,date_sequence=date_sequence)")))
            predictor <- predictor[,1,drop=F]
            predictor <- xts(predictor,date_sequence)
          }else{
          eval(parse(text=paste0("fit <- mgcv::gam(",terms[my_i],"~s(Day365,bs=\"cc\")+NumericDate,data=data_local)")))
          PredData <- data.frame(EntryDate=date_sequence,Day365=as.numeric(format(date_sequence,format="%j")),NumericDate=as.numeric(date_sequence))
          tmp <- predict(fit,newdata=PredData,se.fit=T)
          y_hat <- tmp$fit
          y_se <- sqrt(tmp$se.fit^2+fit$sig2)
          grob <- paste0("PredictorTerms_percentile_",terms[my_i])
          tau_center <- input[[grob]]
          tmp <- y_hat+qnorm(tau_center)*y_se
          ###for factors
          if (terms[my_i] %in% input$FactorTerms){
          train <- as.data.frame(unique(data_local[,terms[my_i]]))
          test <- tmp
          cl <- factor(unique(data_local[,terms[my_i]]))
          tmp <- knn1(train, test, cl)
          tmp <- as.character(tmp)}
          class(tmp) <- class(data_local[,terms[my_i]])
          predictor <- eval(parse(text=paste0("data.frame(",terms[my_i],"=tmp)")))
          predictor <- xts(predictor,date_sequence)
          }
          
          
          ####drop in saved values here from model image
          isolate({
            plotname <- paste("PredictorTerms_", terms[my_i], sep="")
            grob <- paste0(plotname,"_data_extract")
            if(!is.null(ModelImageUpdate[[grob]])){
              q <- ModelImageUpdate[[grob]]
              grob <- paste0(plotname,"_data_dimension_RowCol")
              dimension <-ModelImageUpdate[[grob]]
              q <- matrix(q,nrow=dimension[[1]],ncol=dimension[[2]],byrow = T)
              predictor <- eval(parse(text=paste0("data.frame(",terms[my_i],"=q[,2])")))
              predictor <- xts(predictor,date_sequence)###FIX
            }
          })
          
          
          
            lims <- paste("PredictorTerms_range_lower_", terms[my_i], sep="")
            l <- input[[lims]]
            lims <- paste("PredictorTerms_range_upper_", terms[my_i], sep="")
            u <- input[[lims]]
            output[[inputname]] <- renderdyPencilgraph({
              dyPencilgraph(predictor,paste0("Draw Values:",terms[my_i])) %>%
                dySeries(terms[my_i],fillGraph=T) %>%
                dyAxis("y",valueRange=c(l,u)) ###you must specify the y values range or it will fail
            })
          })
        }
        
        plot_output_list <- lapply(1:length(terms), function(i) {
          inputname <- paste("PredictorTerms_", terms[i], sep="")
          dyPencilgraphOutput(inputname)
        })
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, plot_output_list)
      })
      
      
      
      ###add in section here to plot historical values using medians etc...
      
      MedianPreds <- reactive({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Determining Medians',
                     detail = 'Computing Predictor Curves')
        on.exit(progress$close())
        
        fit <- MODELFIT()
        data <- DATAFILTERED2()[["KEEP"]]###data brought in after filtering is complete
        date_window <- input$DateRange
        predDays <- difftime(date_window[2],date_window[1],units="days")
        date_sequence <- date_window[1]+1:predDays
        terms <- as.character(fit$pred.formula)[2]
        terms <- unlist(strsplit(terms," + ",fixed=T))
        terms <- terms[!(terms %in% c("NumericDate","Day365"))]###get rid of date terms they are dealt with elsewhere
        if(length(terms)==0){return(NULL)}
        term_class <- sapply(data[,terms,drop=F],class)
        output <- list()###this is where we will store the model images
        ###now we regress the quantiles of these partials for the historical data
        for (i in 1:length(terms)) {
          inputname_dataType <- paste("PredictorTerms_dataType_", terms[i], sep="")
          objs <- input[[inputname_dataType]]
          if(!is.null(objs)){
            idx_local <- (data$CustomerCCode %in% objs) | (data$CarrierTCode %in% objs) 
          }else{idx_local <- rep(TRUE,nrow(data))}
          data_local <- data[idx_local,]
          
        df <- input$dfspline
        df_fixed <- input$LambdaFixed
        y=data_local[,terms[i]]
        x=as.numeric(data_local$EntryDate)
        xy=data.frame(y=y,x=x)
        xy <- xy[complete.cases(xy),]
        y=xy$y
        x=xy$x
        #tau_lower <- input$ConfLimits[1]
        grob <- paste0("PredictorTerms_percentile_",terms[i])
        tau_center <- input[[grob]]
        #tau_upper <- input$ConfLimits[2]
        #params <- c(tau_lower,tau_center,tau_upper)
        params <- c(tau_center)
        for(tau in params){
          if(isolate(input$doEstimation==T)){
            g <- function(lam,y,x,tau) AIC(rqss(y ~ qss(x, lambda = lam),tau=tau),k = -1)
            lamstar <- optimize(g, interval = c(df[1], df[2]), x = x, y = y, tau= tau)
            fitq <- quantreg::rqss(y ~ qss(x, lambda = lamstar$min),tau=tau)
          }else{
            fitq <- quantreg::rqss(y ~ qss(x, lambda = df_fixed),tau=tau)
          }
          x2=as.numeric(seq(min(data_local$EntryDate), max(data_local$EntryDate), "days"))
          quantile.fit <- predict(fitq,newdata=data.frame(x=x2))
          quant <- data.frame(fit=quantile.fit)
          if(tau==params[1]){quantiles <- quant}else{quantiles <- data.frame(quantiles,quant)}
        }
        rm(fitq)
        quantiles <- data.frame(EntryDate=as.Date(seq(min(data_local$EntryDate), max(data_local$EntryDate), "days")),quantiles)
        colnames(quantiles) <- c("EntryDate",paste0("HIST",params*100,"th"))
        
        for(b in 2:ncol(quantiles)){
          if (terms[i] %in% input$FactorTerms){
          train <- as.data.frame(unique(data_local[,terms[i]]))
          test <- as.data.frame(quantiles[,b])
          cl <- factor(unique(data_local[,terms[i]]))
          q <- knn1(train, test, cl)
          q <- as.character(q)
          class(q) <- class(data_local[,terms[i]])
          quantiles[,b] <- q}
        }
        
        output[[terms[i]]] <- quantiles
        }
        
        return(output)
      })
      
      
      PredData <- reactive({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Determining Medians',
                     detail = 'Computing Predictor Curves')
        on.exit(progress$close())
        
        data <- DATAFILTERED2()[["KEEP"]]###data brought in after filtering is complete
        fit <- MODELFIT()
        MedianPreds <- MedianPreds()
        terms <- as.character(fit$pred.formula)[2]
        terms <- unlist(strsplit(terms," + ",fixed=T))
        PredTerms <- terms[!(terms %in% c("NumericDate","Day365"))]###get rid of date terms
        DateTerms <- terms[(terms %in% c("NumericDate","Day365"))]###get rid of date terms
        date_window <- input$DateRange
        predDays <- difftime(date_window[2],date_window[1],units="days")
        date_sequence <- date_window[1]+1:predDays
        PredData <- data.frame(EntryDate=date_sequence)
        
        ###construct prediction matrix terms
        for(i in DateTerms){
          if(i=="NumericDate"){
            PredData <- cbind(PredData,NumericDate=as.numeric(date_sequence))
          }
          if(i=="Day365"){
            PredData <- cbind(PredData,Day365=as.numeric(format(date_sequence,format="%j")))
          }
        }
        

        if(length(PredTerms)>0){
          for(i in PredTerms){
            inputname_dataType <- paste("PredictorTerms_dataType_", terms[i], sep="")
            objs <- input[[inputname_dataType]]
            if(!is.null(objs)){
              idx_local <- (data$CustomerCCode %in% objs) | (data$CarrierTCode %in% objs) 
            }else{idx_local <- rep(TRUE,nrow(data))}
            data_local <- data[idx_local,]
            

          plotname <- paste("PredictorTerms_", i, sep="")
          grob <- paste0(plotname,"_data_extract")
          q <- input[[grob]]
          grob <- paste0(plotname,"_data_dimension_RowCol")
          dimension <-input[[grob]]
          q <- matrix(q,nrow=dimension[[1]],ncol=dimension[[2]],byrow = T)
          if (i %in% input$FactorTerms){
          train <- as.data.frame(unique(data_local[,i]))
          test <- as.data.frame(q[,2])
          cl <- factor(unique(data_local[,i]))
          q <- knn1(train, test, cl)
          q <- as.character(q)}else{
            q <- q[,2]
          }
          
          class(q) <- class(data_local[,i])
          eval(parse(text=paste0("PredData <- cbind(PredData,",i,"=q)")))
          }
          }
        
        
        return(PredData)
      })
      
      
      ###plot the medians and forecasted data
      output$PredictionHistorical <- renderUI({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Generating Graphs',
                     detail = 'Plotting Predictor Curves')
        on.exit(progress$close())
        
        fit <- MODELFIT()
        MedianPreds <- MedianPreds()
        PredData <- PredData()
        terms <- as.character(fit$pred.formula)[2]
        terms <- unlist(strsplit(terms," + ",fixed=T))
        terms <- terms[!(terms %in% c("NumericDate","Day365"))]###get rid of date terms they are dealt with elsewhere
        if(length(terms)==0){return(NULL)}

        for (i in 1:length(terms)) {
          local({
            my_i <- i
          inputname <- paste("PredictorHistorical_", terms[my_i], sep="")
          predictor <- MedianPreds[[terms[my_i]]]
          colnames(predictor) <- c("EntryDate",paste0("Hist_",terms[my_i]))
          predictor <- xts(predictor[,2,drop=F],predictor[,1])
          future <- PredData[,c("EntryDate",terms[my_i])]
          colnames(future) <- c("EntryDate",paste0("Fcst_",terms[my_i]))
          event <- future$EntryDate[1]-0.5
          future <- xts(future[,2,drop=F],future[,1])
          predictor <- cbind(predictor,future)

          
          # Need local so that each item gets its own number. Without it, the value
          # of i in the renderPlot() will be the same across all instances, because
          # of when the expression is evaluated.
          

          lims <- paste("PredictorTerms_range_lower_", terms[my_i], sep="")
          l <- input[[lims]]
          lims <- paste("PredictorTerms_range_upper_", terms[my_i], sep="")
          u <- input[[lims]]
          grob <- paste0("PredictorTerms_percentile_",terms[my_i])
          pctile <- input[[grob]]
            output[[inputname]] <- renderDygraph({
              dygraph(predictor,paste0(terms[my_i]," Historical & Forecast for: ",round(pctile,2)*100,"%th percentile")) %>%
                dySeries(paste0("Hist_",terms[my_i]),fillGraph=T) %>%
                dySeries(paste0("Fcst_",terms[my_i]),fillGraph=T) %>%
                dyAxis("y",valueRange=c(l,u)) %>%
                dyEvent(date = event, "Observed/Predicted", labelLoc = "bottom")
            })
          })
        }
        
        plot_output_list <- lapply(1:length(terms), function(i) {
          inputname <- paste("PredictorHistorical_", terms[i], sep="")
          dygraphOutput(inputname)
        })
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, plot_output_list)
      })
      

      
      
      #####Move to Seperate Page of Model Results
      #####This is needed to deal with reactive value issues
      #####Important to do this otherwise we get a strange cycle going on
      
      MedianDATA <- reactive({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Modeling Response',
                     detail = 'Computing Medians')
        on.exit(progress$close())
        
        fit <- MODELFIT()
        data <- DATAFILTERED2()[["KEEP"]]###data brought in after filtering is complete
        date_window <- input$DateRange
        predDays <- difftime(date_window[2],date_window[1],units="days")
        date_sequence <- date_window[1]+1:predDays
        terms <- as.character(fit$pred.formula)[2]
        terms <- unlist(strsplit(terms," + ",fixed=T))
        terms <- terms[!(terms %in% c("NumericDate","Day365"))]###get rid of date terms they are dealt with elsewhere
        if(length(terms)==0){return(NULL)}
        term_class <- sapply(data[,terms,drop=F],class)
        output <- list()###this is where we will store the model images
        ###now we regress the quantiles of these partials for the historical data
        for (i in 1:length(terms)) {
          df <- input$dfspline
          df_fixed <- input$LambdaFixed
          y=data[,terms[i]]
          x=as.numeric(data$EntryDate)
          xy=data.frame(y=y,x=x)
          xy <- xy[complete.cases(xy),]
          y=xy$y
          x=xy$x
          #tau_lower <- input$ConfLimits[1]
          tau_center <- 0.5
          #tau_upper <- input$ConfLimits[2]
          #params <- c(tau_lower,tau_center,tau_upper)
          params <- c(tau_center)
          for(tau in params){
            if(isolate(input$doEstimation==T)){
              g <- function(lam,y,x,tau) AIC(rqss(y ~ qss(x, lambda = lam),tau=tau),k = -1)
              lamstar <- optimize(g, interval = c(df[1], df[2]), x = x, y = y, tau= tau)
              fitq <- quantreg::rqss(y ~ qss(x, lambda = lamstar$min),tau=tau)
            }else{
              fitq <- quantreg::rqss(y ~ qss(x, lambda = df_fixed),tau=tau)
            }
            x2=as.numeric(seq(min(data$EntryDate), max(data$EntryDate), "days"))
            quantile.fit <- predict(fitq,newdata=data.frame(x=x2))
            quant <- data.frame(fit=quantile.fit)
            if(tau==params[1]){quantiles <- quant}else{quantiles <- data.frame(quantiles,quant)}
          }
          rm(fitq)
          quantiles <- data.frame(EntryDate=as.Date(seq(min(data$EntryDate), max(data$EntryDate), "days")),quantiles)
          colnames(quantiles) <- c("EntryDate",paste0("HIST",params*100,"th"))
          
          for(b in 2:ncol(quantiles)){
            if (terms[i] %in% input$FactorTerms){
              train <- as.data.frame(unique(data[,terms[i]]))
              test <- as.data.frame(quantiles[,b])
              cl <- factor(unique(data[,terms[i]]))
              q <- knn1(train, test, cl)
              q <- as.character(q)
              class(q) <- class(data[,terms[i]])
              quantiles[,b] <- q}
          }
          
          output[[terms[i]]] <- quantiles
        }
        
        return(output)
      })
      
      
      
      PREDICTIONDATA <- reactive({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Modeling Response',
                     detail = 'Computing Medians')
        on.exit(progress$close())
        
        data <- DATAFILTERED2()[["KEEP"]]
        fit <- MODELFIT()
        MedianPreds <- MedianDATA()
        PredData <- PredData()
        date_window <- input$DateRange
        terms <- as.character(fit$pred.formula)[2]
        terms <- unlist(strsplit(terms," + ",fixed=T))
        PredTerms <- terms[!(terms %in% c("NumericDate","Day365"))]###get rid of date terms
        DateTerms <- terms[(terms %in% c("NumericDate","Day365"))]###get rid of date terms
        predDays <- difftime(date_window[2],date_window[1],units="days")
        date_sequence <- date_window[1]+1:predDays

        preds <- predict(fit,newdata=PredData,se.fit=T)
        y_hat <- preds$fit
        y_se <- sqrt(preds$se.fit^2+fit$sig2)
        LCL <- y_hat+qnorm(input$ConfLimits[1])*y_se
        UCL <- y_hat+qnorm(input$ConfLimits[2])*y_se
        response <- as.character(formula(fit))[2]
        eval(parse(text=paste0("prediction_data <- data.frame(",response,"=y_hat,LCL=LCL,UCL=UCL,PredData)")))
        colnames(prediction_data)[c(2,3)] <- c(paste0("FCST",input$ConfLimits*100,"th"))
        
        
        ###now we have to generate adjusted response data for this to work
        ###since there are nusiance factors that need to be integrated out
        ids <- length(PredTerms)
        data2 <- data
        

        ####Run the partial predictions
        y_hat <- predict(fit,newdata=data)
        y <- data[,as.character(formula(fit))[2]]
        residual <- y-y_hat
        
        ####fix the data at the constant integration value
        ####for this purpose we should set at the median to make the market reasonable
        ####this is done above in the median stuff
        
        if(ids>0){for(i in 1:ids){
          var <- PredTerms[i]
          value <- base::merge(data2[,"EntryDate",drop=F],MedianPreds[[var]],by="EntryDate",all.x=TRUE)
          colnames(value) <- c("EntryDate",var)
          #eval(parse(text=paste0("value <- input$PredictorTerms_",var)))
          #class(value) <- class(data2[,var])
          data2[,var] <- value[,var]
        }}
        
        y_delta <- -(y_hat-predict(fit,newdata=data2))
        y_partial <- y_hat+y_delta
        y_residual <- y_partial+residual
        observed_data <- data.frame("y_partial"=as.numeric(y_partial),
                          "y_residual"=as.numeric(y_residual),
                          "y_hat"=as.numeric(y_hat),
                          "residual"=as.numeric(residual),
                          data)
        
        ###now we regress the quantiles of these partials for the historical data
        df <- input$dfspline
        df_fixed <- input$LambdaFixed
        y=observed_data$y_residual
        x=as.numeric(data$EntryDate)
        xy=data.frame(y=y,x=x)
        xy <- xy[complete.cases(xy),]
        y=xy$y
        x=xy$x
        tau_lower <- input$ConfLimits[1]
        tau_center <- 0.5
        tau_upper <- input$ConfLimits[2]
        params <- c(tau_lower,tau_center,tau_upper)
        for(tau in params){
          if(isolate(input$doEstimation==T)){
            g <- function(lam,y,x,tau) AIC(rqss(y ~ qss(x, lambda = lam),tau=tau),k = -1)
            lamstar <- optimize(g, interval = c(df[1], df[2]), x = x, y = y, tau= tau)
            fitq <- quantreg::rqss(y ~ qss(x, lambda = lamstar$min),tau=tau)
          }else{
            fitq <- quantreg::rqss(y ~ qss(x, lambda = df_fixed),tau=tau)
          }
          x2=as.numeric(seq(min(data$EntryDate), max(data$EntryDate), "days"))
          
          ###here we have to prevent extrapolation at the tails because 
          ###quantile regression won't tolerate it
          ###We use the carry forward and backward method

          quantile.fit <- predict(fitq,newdata=data.frame(x=x2))
          quant <- data.frame(fit=quantile.fit)
          if(tau==params[1]){quantiles <- quant}else{quantiles <- data.frame(quantiles,quant)}
        }
        rm(fitq)
        quantiles <- data.frame(EntryDate=as.Date(seq(min(data$EntryDate), max(data$EntryDate), "days")),quantiles)
        colnames(quantiles) <- c("EntryDate",paste0("HIST",params*100,"th"))

        ####done with quantiles
#         observed_summary <- observed_data %>% 
#         group_by(EntryDate) %>%
#         summarise(Prediction = mean(y_partial,na.rm=T))
        observed_summary <- tapply(observed_data$y_partial,observed_data$EntryDate,mean,na.rm=T)
        observed_summary <- data.frame(EntryDate=as.Date(names(observed_summary)),Prediction=observed_summary)
        rownames(observed_summary)=NULL
        #observed_summary <- left_join(observed_summary,quantiles)
        observed_summary <- base::merge(observed_summary,quantiles,all.x=TRUE)
        event <- prediction_data$EntryDate[1]-0.5
        out <- list(prediction_data=prediction_data,
                    observed_data=observed_data,
                    event=event,
                    observed_summary=observed_summary)
        return(out)
      })
      
      
      output$PredictionPlotInteractive <- renderDygraph({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Generating Plot',
                     detail = 'Historical Predictions')
        on.exit(progress$close())
        
        preds <- PREDICTIONDATA()[["prediction_data"]]
        data <- PREDICTIONDATA()[["observed_summary"]]
        event <- PREDICTIONDATA()[["event"]]
        fit <- MODELFIT()
        p <- colnames(preds)
        d <- colnames(data)
        response <- as.character(formula(fit))[2]
        idx_date_data <- colnames(data) %in% c("EntryDate")
        idx_date_preds <- colnames(preds) %in% c("EntryDate")
        preds <- xts(preds[,c(1:3),drop=F],preds[,idx_date_preds])
        data <- xts(data[,!idx_date_data,drop=F],data[,idx_date_data])
        series <- cbind(data,preds)
        dygraph(series,paste0("Model Fit, Historical, and Predictions for ",
                              input$ConfLimits[1]*100,"th, 50th & ",input$ConfLimits[2]*100,"th Percentiles")) %>%
          dySeries(d[c(3,4,5)],label="Historical 50th") %>%
          dySeries(p[c(2,1,3)],label="Predicted 50th") %>%
          dySeries("Prediction",label="Model Fit to 50th") %>%
          dyAxis("y",label=response) %>%
          dyRoller(rollPeriod = 1) %>%
          dyEvent(date = event, "Observed/Predicted", labelLoc = "bottom")
      })
      
      tdat1 <- reactive({
        table <- PREDICTIONDATA()[["prediction_data"]]
        idx <- sapply(table,class)
        idx <- idx %in% c("numeric","array")
        table[,idx] <- round(table[,idx],2)
        return(table)
      })
      
      output$PredicitonTable <- DT::renderDataTable({
        DT::datatable(tdat1(),filter='bottom',extensions = 'TableTools',
                      options=list(scrollX=TRUE,    dom = 'T<"clear">lfrtip',
                                   tableTools = list(sSwfPath = copySWF())))
      })
      
      output$PredictionFullPlot <- renderPlot({
        dat <- PREDICTIONDATA()[["observed_data"]]
        preds <- PREDICTIONDATA()[["prediction_data"]]
        selected <- input$PredictionPartial
        var <- "EntryDate"
        fit <- MODELFIT()
        wkdata <- data.frame()
        wkdata1 <- data.frame(y=dat[,"y_partial"],x=dat[,var],group="Model Fit")
        wkdata2 <- data.frame(y=dat[,"y_residual"],x=dat[,var],group="Adjusted Observation")
        wkdata3 <- data.frame(y=preds[,as.character(formula(fit))[2]],x=preds[,var],group="Predicted")
        if("Fitted" %in% selected){wkdata <- rbind(wkdata,wkdata1)}
        if("Observed" %in% selected){wkdata <- rbind(wkdata,wkdata2)}
        if("Predicted" %in% selected){wkdata <- rbind(wkdata,wkdata3)}
        p <- ggplot(wkdata,aes(y=y,x=x,color=group))
        p <- p+geom_point()+xlab(var)+ylab(paste(as.character(formula(fit))[2]))
        print(p)
      })
      
      
      ###########################################################
      #######Volume Integrated Quote
      ###########################################################
      
      
      output$CustomerSelect <- renderUI({
        if(!(input$volbasis=="Specific Customer")){return(NULL)}
        data <- DATAFILTERED2()[["KEEP"]]###data brought in after filtering is complete
        customers <- unique(data$CustomerCCode)
        selected <- NULL
        if(!is.null(Read_Settings()[["CarrierSelect"]])){
          selected <- Read_Settings()[["CustomerSelect"]]
        }
        selectizeInput("CustomerSelect","Customer CCodes to Base Volume On",choices=customers,selected = selected,
                       multiple=TRUE)
      })
      
      output$CarrierSelect <- renderUI({
        if(!(input$volbasis=="Specific Carrier")){return(NULL)}
        data <- DATAFILTERED2()[["KEEP"]]###data brought in after filtering is complete
        customers <- unique(data$CarrierTCode)
        selected <- NULL
        if(!is.null(Read_Settings()[["CarrierSelect"]])){
          selected <- Read_Settings()[["CarrierSelect"]]
        }
        selectizeInput("CarrierSelect","Carrier TCodes to Base Volume On",choices=customers,selected=selected,
                       multiple=TRUE)
      })
      
      
      output$volbasis <- renderUI({
        selected <- c("Transactions in Lane")
        if(!is.null(Read_Settings()[["volbasis"]])){
          selected <- Read_Settings()[["volbasis"]]
        }
        
        selectInput("volbasis","Volume Basis",
                  choices=c("Transactions in Lane","Specific Customer","Specific Carrier"),
                  selected=selected)
      })
      
      output$volmethod <- renderUI({
        selected <- c("GAM Weekly Max")
        if(!is.null(Read_Settings()[["volmethod"]])){
          selected <- Read_Settings()[["volmethod"]]
        }
        
        selectInput("volmethod","Volume Modeling Method",
                    choices = c("GAM","GAM No Weekend","GAM Weekly Max"),
                    selected=selected)
      })
      

      
      
      TransactionalVolume <- reactive({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Modeling Volume',
                     detail = 'Predicting Volume')
        on.exit(progress$close())
        data <- DATAFILTERED2()[["KEEP"]]###data brought in after filtering is complete
        date_window <- input$DateRange
        predDays <- difftime(date_window[2],date_window[1],units="days")
        date_sequence <- date_window[1]+1:predDays
        

        if(input$volbasis=="Transactions in Lane"){
        volume <- tapply(data$EntryDate,data$EntryDate,length)
        volume <- data.frame(EntryDate=as.Date(names(volume)),TransVolume=volume)
        rownames(volume)=NULL
        }
        
        if(input$volbasis=="Specific Customer"){
          dat <- data[data$CustomerCCode %in% input$CustomerSelect,]
          volume <- tapply(dat$EntryDate,dat$EntryDate,length)
          volume <- data.frame(EntryDate=as.Date(names(volume)),TransVolume=volume)
          rownames(volume)=NULL
        }
        
        if(input$volbasis=="Specific Carrier"){
          dat <- data[data$CarrierTCode %in% input$CarrierSelect,]
          volume <- tapply(dat$EntryDate,dat$EntryDate,length)
          volume <- data.frame(EntryDate=as.Date(names(volume)),TransVolume=volume)
          rownames(volume)=NULL
        }
        
        
        
        interval <- difftime(max(volume$EntryDate),min(volume$EntryDate),units = "days")
        dateseq <- min(volume$EntryDate)+1:interval
        JoinDat <- data.frame(EntryDate =dateseq)
        #volume <- JoinDat %>% left_join(volume)
        volume <- base::merge(JoinDat,volume,all.x=TRUE)
        volume$TransVolume[is.na(volume$TransVolume)] <- 0
        volume <- xts(volume[,"TransVolume",drop=F],volume$EntryDate)

        
        ###gam path
        if(input$volmethod=="GAM"){
          dat <- data.frame(coredata(volume),Day365=as.numeric(format(index(volume),format="%j")),NumericDate=as.numeric(index(volume)))
          fit <- mgcv::gam(TransVolume~s(Day365,bs="cc")+NumericDate,data=dat)
          PredData <- data.frame(EntryDate=date_sequence,Day365=as.numeric(format(date_sequence,format="%j")),NumericDate=as.numeric(date_sequence))
          pred_volume <- predict(fit,newdata=PredData)
          pred_volume <- data.frame(TransFcst=as.numeric(pred_volume))
        }
        
        ###gam path
        if(input$volmethod=="GAM No Weekend"){
          dat <- data.frame(coredata(volume),Day365=as.numeric(format(index(volume),format="%j")),NumericDate=as.numeric(index(volume)))
          dat <- dat[!(format(index(volume),format="%A") %in% c("Saturday","Sunday")),] ###remove weekends
          fit <- mgcv::gam(TransVolume~s(Day365,bs="cc")+NumericDate,data=dat)
          PredData <- data.frame(EntryDate=date_sequence,Day365=as.numeric(format(date_sequence,format="%j")),NumericDate=as.numeric(date_sequence))
          pred_volume <- predict(fit,newdata=PredData)
          pred_volume <- data.frame(TransFcst=as.numeric(pred_volume))
        }
        
        ###gam path
        if(input$volmethod=="GAM Weekly Max"){
          dat <- data.frame(coredata(volume),Day365=as.numeric(format(index(volume),format="%j")),
                            NumericDate=as.numeric(index(volume)),idx=1:nrow(volume))
          dat$week_groups <- paste(format(index(volume),format="%Y"),format(index(volume),format="%W"),sep="-")
          out_idx <- lapply(unique(dat$week_groups),function(b){
            tmp <- dat[dat$week_groups==b,]
            return(tmp$idx[which.max(tmp$TransVolume)])
          })
          out_idx <- unlist(out_idx)
          dat <- dat[out_idx,] ###select maximum
          fit <- mgcv::gam(TransVolume~s(Day365,bs="cc")+NumericDate,data=dat)
          PredData <- data.frame(EntryDate=date_sequence,Day365=as.numeric(format(date_sequence,format="%j")),NumericDate=as.numeric(date_sequence))
          pred_volume <- predict(fit,newdata=PredData)
          pred_volume <- data.frame(TransFcst=as.numeric(pred_volume))
        }
        

        pred_volume <- xts(pred_volume,date_sequence)
        
        ####drop in saved values here from model image
        isolate({
          if(!is.null(ModelImageUpdate[["VolumeDraw_data_extract"]])){
            q <- ModelImageUpdate[["VolumeDraw_data_extract"]]
            dimension <-ModelImageUpdate[["VolumeDraw_data_dimension_RowCol"]]
            q <- matrix(q,nrow=dimension[[1]],ncol=dimension[[2]],byrow = T)
            pred_volume <- data.frame(TransFcst=as.numeric(q[,2]))
            pred_volume <- xts(pred_volume,date_sequence)
          }
        })
        
        
        
        return(list(volume=volume,pred_volume=pred_volume))
      })

      
      output$VolumeDraw <- renderdyPencilgraph({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Rendering',
                     detail = 'Pencil Graph')
        on.exit(progress$close())
        
        if(is.null(input$CustomerSelect) & input$volbasis=="Specific Customer"){return(NULL)}
        if(is.null(input$CarrierSelect) & input$volbasis=="Specific Carrier"){return(NULL)}
        pred_volume <- TransactionalVolume()[["pred_volume"]]
        volume <- TransactionalVolume()[["volume"]]
        dyPencilgraph(pred_volume,"Draw Your Desired Volume Curve") %>%
        dyAxis("y",valueRange=c(0,max(volume))) %>%  
        dySeries("TransFcst",label="Transactional Volume FCST",
                   stepPlot = TRUE, fillGraph = TRUE) %>%
        dyRangeSelector()
          
      })
      
      
      PASSVOLUME <- reactive({
        pred_volume <- TransactionalVolume()[["pred_volume"]]
        #raw <- isolate(input$VolumeDraw_data_extract) 
        raw <- input$VolumeDraw_data_extract
        #dimension <- isolate(input$VolumeDraw_data_dimension_RowCol) 
        dimension <- input$VolumeDraw_data_dimension_RowCol
        out <- matrix(raw,nrow=dimension[[1]],ncol=dimension[[2]],byrow = T) 
        out <- as.data.frame(out) 
        coredata(pred_volume) <- out[,2]
        return(list(pred_volume=pred_volume))
      })
      
      
      
      VolumeDataPrep <- reactive({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Aggregaring Volume',
                     detail = 'Summarizing')
        on.exit(progress$close())
        
        preds <- PREDICTIONDATA()[["prediction_data"]]
        data <- PREDICTIONDATA()[["observed_summary"]]
        event <- PREDICTIONDATA()[["event"]]
        volume <- TransactionalVolume()[["volume"]]
        pred_volume <- PASSVOLUME()[["pred_volume"]]
        fit <- MODELFIT()
        response <- as.character(formula(fit))[2]
        idx_date_data <- colnames(data) %in% c("EntryDate")
        idx_date_preds <- colnames(preds) %in% c("EntryDate")
        preds <- xts(preds[,c(1,2,3),drop=F],preds[,idx_date_preds])
        data <- xts(data[,c(3,4,5),drop=F],data[,idx_date_data])
        vol_int_rate_fcst <- numeric(length=ncol(coredata(preds)))
        names(vol_int_rate_fcst) <- colnames(coredata(preds))
        for (i in 1:length(vol_int_rate_fcst)){
          vol_int_rate_fcst[i] <- weighted.mean(coredata(preds)[,i],coredata(pred_volume))
        }
        vol_int_rate_fcst <- round(vol_int_rate_fcst,2)
        series <- cbind(data,preds,volume,pred_volume)
        idx <- colnames(coredata(data))
        for(i in idx){
          series[index(series)<index(preds)[1],i] <- na.approx(series[index(series)<index(preds)[1],i])
        }

        name <- paste0("Volume Integrated Quote: $",vol_int_rate_fcst[1]," ($",vol_int_rate_fcst[2],", $",vol_int_rate_fcst[3],") Per Mile")
        return(list(series=series,vol_int_rate_fcst=vol_int_rate_fcst,event=event,
                    response=response,data=data,preds=preds,volume=volume,
                    name=name))
      })
      
      output$VolumeIntegrated <- renderDygraph({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Rendering',
                     detail = 'Volume Historical & Predicted Graph')
        on.exit(progress$close())
        
        if(is.null(input$CustomerSelect) & input$volbasis=="Specific Customer"){return(NULL)}
        if(is.null(input$CarrierSelect) & input$volbasis=="Specific Carrier"){return(NULL)}
        series <- VolumeDataPrep()[["series"]]
        response <- VolumeDataPrep()[["response"]]
        vol_int_rate <- VolumeDataPrep()[["vol_int_rate"]]
        data <- VolumeDataPrep()[["data"]]
        preds <- VolumeDataPrep()[["preds"]]
        volume <- VolumeDataPrep()[["volume"]]
        event <- VolumeDataPrep()[["event"]]
        name <- VolumeDataPrep()[["name"]]
        cdat <- coredata(series)
        idx <- index(series)
        volplot <- xts(cdat[,c("TransVolume","TransFcst")],idx)
        dygraph(volplot,"Historical and Predicted Volume for Lane") %>%
          dySeries("TransVolume",label="Historical Volume",
                   stepPlot = TRUE, fillGraph = TRUE) %>%
          dySeries("TransFcst",label="Forecast Volume",
                   stepPlot = TRUE, fillGraph = TRUE) %>%
          dyAxis("y", label = "Transacitonal Volume", 
                 independentTicks = TRUE, valueRange = c(0, max(volume))) %>%
          dyRoller(rollPeriod = 1) %>%
          dyEvent(date = event, "Observed/Predicted", labelLoc = "bottom")
      })
      
      
      
      ###########################################################
      #######Historical Volume Integrated Quote
      ###########################################################
      
      output$mile15<- renderValueBox({
        Fn <- mileECDF()
        pct <- 0.15
        valueBox(
          paste0(quantile(Fn,pct), " mi"), paste(pct*100,"th Mileage Percentile"), icon = icon("list"),
          color = "purple"
        )
      })
      
      output$mile50<- renderValueBox({
        Fn <- mileECDF()
        pct <- 0.5
        valueBox(
          paste0(quantile(Fn,pct), " mi"), paste(pct*100,"th Mileage Percentile"), icon = icon("list"),
          color = "purple"
        )
      })
      
      output$mile85<- renderValueBox({
        Fn <- mileECDF()
        pct <- 0.85
        valueBox(
          paste0(quantile(Fn,pct), " mi"), paste(pct*100,"th Mileage Percentile"), icon = icon("list"),
          color = "purple"
        )
      })
      
      
      HistoricalData <- reactive({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Computing',
                     detail = 'Patterned Historical Volume')
        on.exit(progress$close())
        
      series <- VolumeDataPrep()[["series"]]
      response <- VolumeDataPrep()[["response"]]
      vol_int_rate_fcst <- VolumeDataPrep()[["vol_int_rate_fcst"]]
      data <- VolumeDataPrep()[["data"]]
      preds <- VolumeDataPrep()[["preds"]]
      volume <- VolumeDataPrep()[["volume"]]
      event <- VolumeDataPrep()[["event"]]
      name <- VolumeDataPrep()[["name"]]
      date <- index(series)
      year <- format(date,format="%Y")
      month <- format(date,format="%m")
      day <- format(date,format="%d")
      data <- as.data.frame(coredata(series))
      idx <- !is.na(data$TransFcst)###get the forecast portion
      fcst <- data$TransFcst[idx]
      data$TransFcst[!idx] <- 0
      for(i in 1:length(date[!idx])){
        id <- day[idx]==day[i] & month[idx]==month[i]
        if(any(id)){data$TransFcst[i] <- fcst[id]}
      }
      data$TransFcst[!idx] <- na.approx(data$TransFcst[!idx])
      rm <- colnames(data) %in% "TransVolume"
      data <- data[,!rm]
      series <- xts(data,date)
      ###now get the date window to do a historical volume pass
      ###loop over all of the past values
      dte_window <- c(min(date[idx]),max(date[idx]))
      loop <- as.numeric(format(dte_window[1],format="%Y"))-as.numeric(format(min(index(series)),format="%Y"))
      quote <- data.frame()
      for(i in 1:loop){
        lower <- paste(as.numeric(format(dte_window[1],format="%Y"))-i,format(dte_window[1],format="%m-%d"),sep="-")
        upper <- paste(as.numeric(format(dte_window[2],format="%Y"))-i,format(dte_window[2],format="%m-%d"),sep="-")
        wdow <- paste(lower,upper,sep="::")
        tmp <- series[wdow]
        if(i==loop & (min(index(tmp))>as.Date(lower))){break}###dont add value if not complete cycle
        quote <- rbind(quote,data.frame(
                            "StartDate"=as.Date(lower),
                            "EndDate"=as.Date(upper),
                            "MidPoint"=as.Date(lower)+difftime(as.Date(upper),as.Date(lower))/2,
                            "WeightedY_LCL"=weighted.mean(tmp[,1],tmp$TransFcst,na.rm = T),
                            "WeightedY"=weighted.mean(tmp[,2],tmp$TransFcst,na.rm = T),
                            "WeightedY_UCL"=weighted.mean(tmp[,3],tmp$TransFcst,na.rm = T)
                            ))
      }


      ###add on the predicted quote
      quote <- rbind(data.frame(
        "StartDate"=dte_window[1],
        "EndDate"=dte_window[2],
        "MidPoint"=dte_window[1]+difftime(dte_window[2],dte_window[1])/2,
        "WeightedY_LCL"=as.numeric(vol_int_rate_fcst[2]),
        "WeightedY"=as.numeric(vol_int_rate_fcst[1]),
        "WeightedY_UCL"=as.numeric(vol_int_rate_fcst[3])),
         quote)
      
      colnames(quote)[c(4,5,6)] <- gsub("HIST","Percentile.",colnames(series)[c(1:3)])
      
      ####fix up series to add a section of 0's if needed to pad the plot  in the next step
      full_date_run <- data.frame(EntryDate=as.Date(seq(min(index(series)),max(index(series)),"days")),padit=NA)
      padit <- xts(full_date_run[,2,drop=F],full_date_run[,1])
      series <- cbind(series,padit)
      series$TransFcst[is.na(series$TransFcst)] <- 0
      series <- series[,-c(8)]
      return(list(series=series,vol_int_rate_fcst=vol_int_rate_fcst,event=event,
                  response=response,data=data,preds=preds,volume=volume,
                  name=name,quote=quote))
      })
      
      
      
      output$Historical <- renderDygraph({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Rendering',
                     detail = 'Past Data & Predictions')
        on.exit(progress$close())
        
        series <- HistoricalData()[["series"]]
        #series <- series[,c(2,4:7)]
        p <- colnames(series)
        response <- HistoricalData()[["response"]]
        vol_int_rate_fcst <- HistoricalData()[["vol_int_rate_fcst"]]
        data <- HistoricalData()[["data"]]
        preds <- HistoricalData()[["preds"]]
        volume <- HistoricalData()[["volume"]]
        event <- HistoricalData()[["event"]]
        name <- HistoricalData()[["name"]]
        dygraph(series,name) %>%
          dySeries(p[c(1,2,3)],label="Historical") %>%
          dySeries("TransFcst",label="Repeated Volume",
                   axis='y2',stepPlot = TRUE, fillGraph = TRUE) %>%  
          dySeries(p[c(5,4,6)],label="FCST") %>% ###turned off error bars... if desired but might crash
          dyAxis("y",label=response) %>%
          dyAxis("y2", label = "Transacitonal Volume", 
                 independentTicks = TRUE, valueRange = c(0, max(volume))) %>%
          dyRoller(rollPeriod = 1) %>%
          dyEvent(date = event, "Observed/Predicted", labelLoc = "bottom")
      })
      
      
      
      output$HistVolIntegrated<- renderDygraph({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Rendering',
                     detail = 'Integrated Quote')
        on.exit(progress$close())
        
        quote <- HistoricalData()[["quote"]]
        event <- HistoricalData()[["event"]]
        response <- HistoricalData()[["response"]]
        quote <- xts(quote[,c(4:6),drop=F],quote$MidPoint)
        p <- colnames(quote)
        dygraph(quote,paste(p)) %>%
          dySeries(p,label=response) %>%
          dyAxis("y",label=response) %>%
          dyOptions(drawPoints = TRUE, pointSize = 5) %>%
          dyEvent(date = event, "Observed/Predicted", labelLoc = "bottom")
      })
      
      tdat2 <- reactive({
        progress <- shiny::Progress$new(session, min=0, max=2)
        progress$set(message = 'Rendering',
                     detail = 'Table Values')
        on.exit(progress$close())
        
        table <- HistoricalData()[["quote"]]
        idx <- sapply(table,class)
        idx <- idx %in% c("numeric","array")
        table[,idx] <- round(table[,idx],2)
        pull <- colnames(table) %in% "MidPoint"
        Fn <- mileECDF()
        pct <- 0.5
        table[,pull] <- quantile(Fn,pct)
        colnames(table)[pull] <- paste0(pct*100,"th Percentile Mileage")
        return(table)
      })

      output$HistVolIntegratedTable<- DT::renderDataTable(
        DT::datatable(tdat2(),filter='bottom',extensions = 'TableTools',
                      options=list(scrollX=TRUE,    dom = 'T<"clear">lfrtip',
                                   tableTools = list(sSwfPath = copySWF())))
      )
      
      
      
      
      ###########################################################
      #######Plot Map of chosen data
      ###########################################################
      
      output$MapSelectedData <- renderPlot({
        if(is.null(DATA())){return(NULL)}
        layers <- input$maplayers
        SELECTED <- DATA()[["SELECTED"]]
        NOTSELECTED_IDX <- DATA()[["NOTSELECTED_IDX"]]
        
        map("state",interior=F)
        if("State Borders" %in% layers){map("state")}
        
        if("Selected Counties" %in% layers){
          selectCounties <- input$SelectDestCounties
          if(!is.null(selectCounties)){map("county",regions = selectCounties,plot=T,fill=T,col="yellow",add=T)}
          selectCounties <- input$SelectOrigCounties
          if(!is.null(selectCounties)){map("county",regions = selectCounties,plot=T,fill=T,col="yellow",add=T)}
          }
        
        if("Selected Circles" %in% layers){
          if(!is.null(input$SelectDestCircles)){
            tmp <- input$SelectDestCircles
            lapply(tmp,function(b){
              b <- strsplit(b,":")
              b <- unlist(b)
              plotcircle(r=radius_xyunits(miles=as.numeric(b[3])),mid=c(as.numeric(b[1]),as.numeric(b[2])),col="yellow",type="n")
            })
          }
          
          if(!is.null(input$SelectOrigCircles)){
            tmp <- input$SelectOrigCircles
            lapply(tmp,function(b){
              b <- strsplit(b,":")
              b <- unlist(b)
              plotcircle(r=radius_xyunits(miles=as.numeric(b[3])),mid=c(as.numeric(b[1]),as.numeric(b[2])),col="yellow",type="n")
            })
          }
        }
        
        if("Unselected Data" %in% layers){
          data <- RAWReduced()
          NOTSELECTED <- RAWReduced()[NOTSELECTED_IDX,]
          orig <- data.frame(x=NOTSELECTED$OrigLongitude,y=NOTSELECTED$OrigLatitude)
          orig <- unique(orig)
          dest <- data.frame(x=NOTSELECTED$DestLongitude,y=NOTSELECTED$DestLatitude)
          dest <- unique(dest)
          points(x=orig$x,y=orig$y,cex=0.1,col="grey",pch=19)
          points(x=dest$x,y=dest$y,cex=0.1,col="grey",pch=19)}
        
        if("Selected Data" %in% layers){
          orig <- data.frame(x=SELECTED$OrigLongitude,y=SELECTED$OrigLatitude)
          orig <- unique(orig)
          dest <- data.frame(x=SELECTED$DestLongitude,y=SELECTED$DestLatitude)
          dest <- unique(dest)
          points(x=orig$x,y=orig$y,cex=0.1,col="blue",pch=19)
          points(x=dest$x,y=dest$y,cex=0.1,col="red",pch=19)}
        
        if(("Unselected Data" %in% layers) & ("Selected Data" %in% layers)){
          legend("bottomright",c("Orig Transaction","Dest Transaction","Unselected"),pch=19,col=c("blue","red","grey"))}
        
        if(("Unselected Data" %in% layers) & !("Selected Data" %in% layers)){
          legend("bottomright",c("Unselected"),pch=19,col=c("grey"))}
        
        if(!("Unselected Data" %in% layers) & ("Selected Data" %in% layers)){
          legend("bottomright",c("Orig Transaction","Dest Transaction"),pch=19,col=c("blue","red"))}
      })
      
      
      
      ###########################################################
      #######Marginal Effects Plot
      ###########################################################
      
      output$MarginalEffect <- renderUI({
        linear <- input$LinearTerms
        spline <- input$SplineTerms
        splineCC <- input$SplineTermsCyclic
        factors <- input$FactorTerms
        terms <- c(linear,spline,splineCC,factors)
        selectInput("MarginalEffect","Select Variable to Display Marginal Effect",
                    choices=terms,selected=NULL)
      })
      
      
      
      MarginalData <- reactive({
        fit <- MODELFIT()
        data <- DATAFILTERED2()[["KEEP"]]###data brought in after filtering is complete
        idx <- input$MarginalEffect
        xs <- data[,idx,drop=F]
        isolate(factors <- input$FactorTerms)
        class_xs <- idx %in% factors
        if(!(class_xs)){
          tmp <- data.frame(seq(min(xs),max(xs),length.out = 50))
          colnames(tmp) <- colnames(xs)
          xs <- tmp
        }else{
          tmp <- unique(xs)
          colnames(tmp) <- colnames(xs)
          xs <- tmp
        }
        
        y_hat <- apply(xs,1,function(x){
          ndat <- data
          ndat[,idx] <- x
          tmp <- predict(fit,newdata=ndat)
          return(mean(tmp))
        })
        
        y_hat <- unlist(y_hat)
        name <- as.character(formula(fit))[2]
        y_hat <- data.frame(y_hat)
        colnames(y_hat) <- name
        tmp <- data.frame(y_hat,xs)
        return(tmp)
        
      })
      
      output$MarginalPlot <- renderPlot({
        MarginalData <- MarginalData()
        name <- colnames(MarginalData)
        eval(parse(text=paste0("plot <- ggplot(MarginalData,aes(x=",name[2],",y=",name[1],"))")))
        plot <- plot+geom_point()+geom_line()
        print(plot)
        
      })
      
      
      ###########################################################
      #######Partial Effects Plot (adjusted by nusiance factors)
      ###########################################################
      
      output$PartialEffect <- renderUI({
        data <- DATAFILTERED2()[["KEEP"]]###data brought in after filtering is complete
        terms <- colnames(data)
        selectInput("PartialEffect","Select Variable to Display Partial Effect",
                    choices=terms,selected="EntryDate")
      })
      
      output$NusianceEffect <- renderUI({
        linear <- input$LinearTerms
        spline <- input$SplineTerms
        splineCC <- input$SplineTermsCyclic
        factors <- input$FactorTerms
        terms <- c(linear,spline,splineCC,factors)
        selectizeInput("NusianceEffect","Select Nusiance Variables to Adjust Effect out of Plot",
                       choices=terms,selected=NULL,multiple = TRUE)
      })
      
      
      output$NusianceLevels = renderUI({
        data <- DATAFILTERED2()[["KEEP"]]###data brought in after filtering is complete
        if(is.null(input$PartialEffect)){return(NULL)}
        if(is.null(input$NusianceEffect)){return(NULL)}
        myUIs <- lapply(1:length(input$NusianceEffect), function(i) {
          inputname <- paste("NusianceLevels", i, sep="")
          levs <- data[,input$NusianceEffect[i]]
          levs <- unique(levs[order(levs)])
          selectInput(inputname, 
                      paste0("Fix ",input$NusianceEffect[i]," at:"),
                      levs,levs[1])
        })
        do.call(tagList, myUIs)
      })
      
      PARTIALDATA <- reactive({
        data <- DATAFILTERED2()[["KEEP"]]###data brought in after filtering is complete
        data2 <- data
        fit <- MODELFIT()
        if(is.null(input$PartialEffect)){return(NULL)}
        ids <- unlist(reactiveValuesToList(input))
        ids <- length(input$NusianceEffect)
        
        ####Run the partial predictions
        y_hat <- predict(fit,newdata=data)
        y <- data[,as.character(formula(fit))[2]]
        residual <- y-y_hat
        
        ####fix the data at the constant integration value
        if(ids>0){for(i in 1:ids){
          var <- input$NusianceEffect[i]
          eval(parse(text=paste0("value <- input$NusianceLevels",i)))
          class(value) <- class(data2[,var])
          data2[,var] <- value
        }}
        
        y_delta <- -(y_hat-predict(fit,newdata=data2))
        y_partial <- y_hat+y_delta
        y_residual <- y_partial+residual
        
        out <- data.frame("y_partial"=y_partial,
                          "y_residual"=y_residual,
                          "y_hat"=y_hat,
                          "residual"=residual,
                          data)
        return(out)
      })
      
      output$PartialPlot <- renderPlot({
        dat <- PARTIALDATA()
        if(is.null(input$PartialEffect)){return(NULL)}
        selected <- input$PartialSeries
        var <- input$PartialEffect
        fit <- MODELFIT()
        wkdata1 <- data.frame(y=dat[,"y_partial"],x=dat[,var],group="Partial Prediction")
        wkdata2 <- data.frame(y=dat[,"y_residual"],x=dat[,var],group="Adjusted Observation")
        if(("Fitted" %in% selected) & !("Observed" %in% selected)){wkdata <- wkdata1}
        if(!("Fitted" %in% selected) & ("Observed" %in% selected)){wkdata <- wkdata2}
        if(("Fitted" %in% selected) & ("Observed" %in% selected)){wkdata <- bind_rows(wkdata1,wkdata2)}
        if(!("Fitted" %in% selected) & !("Observed" %in% selected)){return(NULL)}
        p <- ggplot(wkdata,aes(y=y,x=x,color=group))
        p <- p+geom_point()+xlab(var)+ylab(paste("Partial",as.character(formula(fit))[2]))
        print(p)
      })
      
      
      
      
      
    #########################################################################
    ######Output Session Info
    #########################################################################
      output$session <- renderPrint({
        devtools::session_info()
      })
      
      output$session2 <- renderPrint({
        sessionInfo()
      })
      


      
      

  
})###end server here
