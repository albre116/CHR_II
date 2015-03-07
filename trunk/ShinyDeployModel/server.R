

flag <<-0
shinyServer(function(input, output, session) {
  
  

  ###########################################################
  #######Tab Panel 1:  Geography
  ###########################################################
  
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
      
      
      output$OrigPlotState <- renderPlot(function(){
        selectStates <- input$SelectOrigStates
        if(length(selectStates)>0){
          mapOrig <- map("state",regions = selectStates,plot=F,fill=T,col="grey95")} else{mapOrig <- NULL}
        map(states)
        if(!is.null(mapOrig)){map(mapOrig,fill=T,add=T,col="grey95")}
        if("State Names" %in% input$maplayersOrigStates){map.text(state_labs,add=T)}
        if("Data" %in% input$maplayersOrigStates){
          points(x=RAW$OrigLongitude,y=RAW$OrigLatitude,cex=0.1,col="grey",pch=19)
          }
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
      
      
      output$DestPlotState <- renderPlot(function(){
        selectStates <- input$SelectDestStates
        if(length(selectStates)>0){
          mapDest <- map("state",regions = selectStates,plot=F,fill=T,col="grey95")} else{mapDest <- NULL}
        map(states)
        if(!is.null(mapDest)){map(mapDest,fill=T,add=T,col="grey95")}
        if("State Names" %in% input$maplayersDestStates){map.text(state_labs,add=T)}
        if("Data" %in% input$maplayersDestStates){
          points(x=RAW$DestLongitude,y=RAW$DestLatitude,cex=0.1,col="grey",pch=19)
        }
      })
      
      ###########################################################
      #######All of the Selection functions for the Origin Counties
      ###########################################################
      CountiesOrigin <- reactive({
        pick <- input$SelectOrigStates
        if(is.null(pick)){return(NULL)}
        pick <- unlist(lapply(pick,function(x){strsplit(x,":")[[1]][1]}))
        counties <- map("county",regions = pick,plot=F,fill=TRUE)
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
        AddOrigCounties <- map.where(counties,x=OriginAddCounties$x,y=OriginAddCounties$y)
        AddOrigCounties <- AddOrigCounties[!is.na(AddOrigCounties)]
        if (length(AddOrigCounties)==0){AddOrigCounties <- NULL}
        return(AddOrigCounties)
      })
      
      output$SelectOrigCounties <- renderUI({
        counties <- CountiesOrigin()
        isolate(selected <- input$SelectOrigCounties)
        pick <- unlist(lapply(input$SelectOrigStates,function(x){strsplit(x,":")[[1]][1]}))
        pick <- c(pick,counties$names)
        pick <- pick[!is.null(pick)]
        selected <- c(selected,ClickCountiesAddOrig())
        selected <- unique(selected)
        selected <- selected[!is.null(selected)]
        selectizeInput("SelectOrigCounties","Selected Origin Counties or Entire State",choices=pick,selected=selected,multiple=T)
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
      
      
      output$OrigPlotCounties <- renderPlot(function(){
        counties <- CountiesOrigin()
        if(is.null(counties)){return(NULL)}
        selectCounties <- input$SelectOrigCounties
        if(length(selectCounties)>0){
          mapOrig <- map("county",regions = selectCounties,plot=F,fill=T,col="grey95")} else{mapOrig <- NULL}
        map(counties)
        if(!is.null(mapOrig)){map(mapOrig,fill=T,add=T,col="grey95")}
        if("Data" %in% input$maplayersOrigCounties){
          points(x=RAW$OrigLongitude,y=RAW$OrigLatitude,cex=0.1,col="grey",pch=19)
        }
      })
      
      ###########################################################
      #######All of the Selection functions for the Destination Counties
      ###########################################################
      CountiesDestination <- reactive({
        pick <- input$SelectDestStates
        if(is.null(pick)){return(NULL)}
        pick <- unlist(lapply(pick,function(x){strsplit(x,":")[[1]][1]}))
        counties <- map("county",regions = pick,plot=F,fill=TRUE)
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
        AddDestCounties <- map.where(counties,x=DestinationAddCounties$x,y=DestinationAddCounties$y)
        AddDestCounties <- AddDestCounties[!is.na(AddDestCounties)]
        if (length(AddDestCounties)==0){AddDestCounties <- NULL}
        return(AddDestCounties)
      })
      
      output$SelectDestCounties <- renderUI({
        counties <- CountiesDestination()
        isolate(selected <- input$SelectDestCounties)
        pick <- unlist(lapply(input$SelectDestStates,function(x){strsplit(x,":")[[1]][1]}))
        pick <- c(pick,counties$names)
        pick <- pick[!is.null(pick)]
        selected <- c(selected,ClickCountiesAddDest())
        selected <- unique(selected)
        selected <- selected[!is.null(selected)]
        selectizeInput("SelectDestCounties","Selected Destination Counties or Entire State",choices=pick,selected=selected,multiple=T)
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
      
      
      output$DestPlotCounties <- renderPlot(function(){
        counties <- CountiesDestination()
        if(is.null(counties)){return(NULL)}
        selectCounties <- input$SelectDestCounties
        if(length(selectCounties)>0){
          mapDest <- map("county",regions = selectCounties,plot=F,fill=T,col="grey95")} else{mapDest <- NULL}
        map(counties)
        if(!is.null(mapDest)){map(mapDest,fill=T,add=T,col="grey95")}
        if("Data" %in% input$maplayersDestCounties){
          points(x=RAW$DestLongitude,y=RAW$DestLatitude,cex=0.1,col="grey",pch=19)
        }
      })
      
      
      ###########################################################
      #######Tab Panel 2:  Data Conditioning
      ###########################################################
      
      ###########################################################
      #######Plot Map of chosen data
      ###########################################################
      
      DATA <- reactive({
        if(is.null(input$SelectDestCounties) | is.null(input$SelectOrigCounties)){return(NULL)}
        countiesDestination <- input$SelectDestCounties
        countiesOrigin <- input$SelectOrigCounties
        selectDestination <- map("county",regions = countiesDestination,fill=T,plot=F)
        selectOrigin <- map("county",regions = countiesOrigin,fill=T,plot=F)
        indexDestCounty <- map.where(selectDestination,x=RAW$DestLongitude,y=RAW$DestLatitude)
        indexOrigCounty <- map.where(selectOrigin,x=RAW$OrigLongitude,y=RAW$OrigLatitude)
        SELECTED <- RAW %>% filter(!is.na(indexOrigCounty),!is.na(indexDestCounty))
        NOTSELECTED <- RAW %>% filter(is.na(indexOrigCounty),is.na(indexDestCounty))
        return(list(SELECTED=SELECTED,NOTSELECTED=NOTSELECTED))
      })
      
      
      
      
      output$MapSelectedData <- renderPlot(function(){
        if(is.null(DATA())){return(NULL)}
        layers <- input$maplayers
        SELECTED <- DATA()[["SELECTED"]]
        NOTSELECTED <- DATA()[["NOTSELECTED"]]
        
        map("state",interior=F)
        if("State Borders" %in% layers){map("state")}
        
        if("Selected Counties" %in% layers){
          selectCounties <- input$SelectDestCounties
          map("county",regions = selectCounties,plot=T,fill=T,col="grey95",add=T)
          selectCounties <- input$SelectOrigCounties
          map("county",regions = selectCounties,plot=T,fill=T,col="grey95",add=T)}
        
        if("Unselected Data" %in% layers){
          points(x=NOTSELECTED$OrigLongitude,y=NOTSELECTED$OrigLatitude,cex=0.1,col="grey",pch=19)
          points(x=NOTSELECTED$DestLongitude,y=NOTSELECTED$DestLatitude,cex=0.1,col="grey",pch=19)}
        
        if("Selected Data" %in% layers){
        points(x=SELECTED$OrigLongitude,y=SELECTED$OrigLatitude,cex=0.1,col="blue",pch=19)
        points(x=SELECTED$DestLongitude,y=SELECTED$DestLatitude,cex=0.1,col="red",pch=19)}
        
        if(("Unselected Data" %in% layers) & ("Selected Data" %in% layers)){
            legend("bottomright",c("Orig Transaction","Dest Transaction","Unselected"),pch=19,col=c("blue","red","grey"))}
        
        if(("Unselected Data" %in% layers) & !("Selected Data" %in% layers)){
          legend("bottomright",c("Unselected"),pch=19,col=c("grey"))}
        
        if(!("Unselected Data" %in% layers) & ("Selected Data" %in% layers)){
          legend("bottomright",c("Orig Transaction","Dest Transaction"),pch=19,col=c("blue","red"))}
      })
      
      
      ###########################################################
      #######Select the Appropriate Time Window
      ###########################################################
      PERCENTILES <- reactive({
        if(is.null(DATA())){return(NULL)}
        SELECTED <- DATA()[["SELECTED"]]
        input$applyDygraph
        isolate(df <- input$dfspline)
        isolate(tau_lower <- input$lowerTau)
        isolate(tau_center <- input$centralTau)
        isolate(tau_upper <- input$upperTau)
        
        CLEAN <-  SELECTED %>% 
          select(RPM_NormalizedCustomer,EntryDate) %>%
          filter(!is.infinite(RPM_NormalizedCustomer),
                 !is.na(RPM_NormalizedCustomer)) %>%
          arrange(EntryDate)
        X <- model.matrix(CLEAN$RPM_NormalizedCustomer ~ bs(CLEAN$EntryDate, df=df))
        quantiles <- data.frame()
        params <- c(tau_lower,tau_center,tau_upper)
        for(tau in params){
          fit <- rq(RPM_NormalizedCustomer ~ bs(EntryDate, df=df), tau=tau, data=CLEAN)
          quantile.fit <- X %*% fit$coef
          quant <- data.frame(EntryDate=CLEAN$EntryDate,fit=quantile.fit,quantile=tau)
          quant <- quant %>%
            group_by(EntryDate) %>%
            summarise(fit=unique(fit),
                      quantile=unique(tau)) %>%
            arrange(EntryDate)
          quantiles <- bind_rows(quantiles,quant)
        }
        rm(fit,X)
        quantiles <- as.data.frame(quantiles)
        EntryDate<- unique(quantiles$EntryDate)
        quantiles <- unstack(quantiles,fit~quantile,data=quantiles)
        quantiles$EntryDate <- EntryDate
        colnames(quantiles) <- c(paste0(params*100,"th"),"Date")
        return(quantiles)
      })
      
      
      
      output$dygraph <- renderDygraph({
        quantiles <- PERCENTILES()
        plot_dat <- xts(quantiles[,-4],quantiles[,4])
        dygraph(plot_dat,main=paste(paste(colnames(quantiles)[1:3],collapse=" "),"Percentiles of Raw Data")) %>%
          dySeries(c(colnames(quantiles)[1:3]),label="Median RPM$") %>%
          dyAxis("y",label="Normalized Rate Per Mile ($)") %>%
          dyRangeSelector()
      })
      
      
      
      ###########################################################
      #######Remove fixed rate observations (if desired)
      ###########################################################
      
      output$UpperLower <- renderUI({
        DATA <- DATA()[["SELECTED"]]
        low <- min(DATA$RPM_NormalizedCustomer,na.rm = TRUE)
        low <- floor(low*100)/100
        high <- max(DATA$RPM_NormalizedCustomer,na.rm = TRUE)
        high <- ceiling(high*100)/100
        sliderInput("UpperLower","Rate Per Mile Limits",min=low,max=high,value=c(low,high))
      })
      
      
      
      DATAWINDOW <- reactive({
        SELECTED <- DATA()[["SELECTED"]]
        input$applyDygraph #this is the action button for the percentiles
        input$applyUpperLower #this is the action button for a RAW RPM filter
        if(is.null(SELECTED)){return(NULL)}
        isolate(if(is.null(input$dygraph_date_window)){return(list(SELECTED=SELECTED))})
        isolate(min_dte <- input$dygraph_date_window[1])
        isolate(max_dte <- input$dygraph_date_window[2])
        isolate(low <-input$UpperLower[1])
        isolate(high <-input$UpperLower[2]) 
        SELECTED <- SELECTED %>% 
          filter(EntryDate>=min_dte,EntryDate<=max_dte) %>%
          filter(RPM_NormalizedCustomer>=low,RPM_NormalizedCustomer<=high)
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
        SELECTED <- DATAWINDOW()[["SELECTED"]]
        if(is.null(SELECTED)){return(NULL)}
        if(is.null(RemoveGroups$x)){return(NULL)}
        train <- data.frame(as.numeric(SELECTED$EntryDate),SELECTED$RPM_NormalizedCustomer)
        idx <- complete.cases(train)
        train <- train[idx,]
        lower <- unlist(apply(train,2,min))
        upper <- unlist(apply(train,2,max))
        scaling <- upper-lower
        train <- scale(train,center=F,scale=scaling)
        #cl <- factor(SELECTED$CustomerCarrier[idx])
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
        SELECTED <- DATAWINDOW()[["SELECTED"]]
        if(is.null(SELECTED)){return(NULL)}
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
        SELECTED <- DATAWINDOW()[["SELECTED"]]
        if(is.null(SELECTED)){return(NULL)}
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
        SELECTED <- DATAWINDOW()[["SELECTED"]]
        if(is.null(SELECTED)){return(NULL)}
        if(is.null(RemoveGroupsHover$x)){return("Mouse Hover:")}
        train <- data.frame(as.numeric(SELECTED$EntryDate),SELECTED$RPM_NormalizedCustomer)
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
        if(is.null(SELECTED)){return(NULL)}
        pull <- input$RemoveCustomerCarrier
        pull2 <- input$RemoveIndividual
        idx_toss <- (SELECTED$CustomerCarrier %in% pull) | (SELECTED$loadnum %in% pull2)
        if(("Lower Quantile" %in% input$QuantileFilter) | ("Upper Quantile" %in% input$QuantileFilter)){
          quantiles <- PERCENTILES()
          ids <- colnames(quantiles)
          QUANT <-  select(SELECTED,EntryDate,RPM_NormalizedCustomer) %>% 
            left_join(quantiles,by=c("EntryDate"="Date"))
          
          if(c("Lower Quantile") %in% input$QuantileFilter){
            idx_toss <- idx_toss | (QUANT$RPM_NormalizedCustomer<=QUANT[ids[1]])
          }

          if(c("Upper Quantile") %in% input$QuantileFilter){
            idx_toss <- idx_toss | (QUANT$RPM_NormalizedCustomer>=QUANT[ids[3]])
          }
        }
        
        KEEP <- SELECTED %>% filter(!idx_toss)
        TOSS <- SELECTED %>% filter(idx_toss)
        return(list(KEEP=KEEP,TOSS=TOSS))
      })
      
      
      output$RemovalPlot <- renderPlot(function(){
        if(is.null(DATAFILTERED)){return(NULL)}
        KEEP <- DATAFILTERED()[["KEEP"]]
        TOSS <- DATAFILTERED()[["TOSS"]]
        LIMITS <- bind_rows(KEEP,TOSS)
        plot(x=LIMITS$EntryDate,y=LIMITS$RPM_NormalizedCustomer,type="n",pch=19,col="black",
             xlab="Date",ylab="Normalized Rate Per Mile")
        
        if("Percentiles" %in% input$plotControls){
          quantiles <- PERCENTILES()
          cord.x <- c(quantiles[,4],quantiles[nrow(quantiles):1,4])
          cord.y <- c(quantiles[,3],quantiles[nrow(quantiles):1,1])
          polygon(cord.x,cord.y,col='grey95')
        }
        
        if("Kept" %in% input$plotControls){
          points(x=KEEP$EntryDate,y=KEEP$RPM_NormalizedCustomer,pch=19,col="black")
        }
        
      if("Removed" %in% input$plotControls){
        points(x=TOSS$EntryDate,y=TOSS$RPM_NormalizedCustomer,type="p",pch=19,col="grey75")
        }

        legend("topright",c("Removed","Kept"),pch=19,col=c("grey75","black"))
      })
      
      
      ###########################################################
      #######Tab Panel 3:  Model Fitting
      ###########################################################
      
      ###########################################################
      #######Modeling Kernel
      ###########################################################
      
      
      output$LinearTerms <- renderUI({
        data <- DATAFILTERED()[["KEEP"]]
        terms <- colnames(data)
        selectizeInput("LinearTerms","Linear Terms in Model",
                       choices=terms,selected=c("NumericDate"),multiple=T)
      })
      
      output$FactorTerms <- renderUI({
        data <- DATAFILTERED()[["KEEP"]]
        terms <- colnames(data)
        selectizeInput("FactorTerms","Factors in Model",
                       choices=terms,selected=c("SumOfStops"),multiple=T)
      })
      
      output$SplineTerms <- renderUI({
        data <- DATAFILTERED()[["KEEP"]]
        terms <- colnames(data)
        selectizeInput("SplineTerms","Spline Terms in Model (non cyclic)",
                       choices=terms,selected=NULL,multiple=T)
      })
      
      output$SplineTermsCyclic <- renderUI({
        data <- DATAFILTERED()[["KEEP"]]
        terms <- colnames(data)
        selectizeInput("SplineTermsCyclic","Cyclical Spline Terms in Model",
                       choices=terms,selected=c("Day365"),multiple=T)
      })
      
      
      
      
      
      
      MODELFIT <- reactive({
        data <- DATAFILTERED()[["KEEP"]]###data brought in after filtering is complete
        if(is.null(data)){return(NULL)}
        input$FitModel
        if(flag==0){
          linear <- input$LinearTerms
          spline <- input$SplineTerms
          splineCC <- input$SplineTermsCyclic
          factors <- input$FactorTerms
          flag<<-flag+1
        }else{
        isolate(linear <- input$LinearTerms)
        isolate(spline <- input$SplineTerms)
        isolate(splineCC <- input$SplineTermsCyclic)
        isolate(factors <- input$FactorTerms)}
        
        if(is.null(linear) & is.null(spline) & is.null(splineCC)){return(NULL)}

        
        f <- formula(RPM_NormalizedCustomer~1)  ###place holder
        
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
        fit <- modelCPDS(f=f,data=data,kernel=isolate(input$ModelFamily),gamma=1.4)
        return(fit)
      })
      
      output$ModelSummary <- renderPrint({
        summary <- summary(MODELFIT())
        if (!is.null(summary)) {
          return(print(summary))
        }
      })
      
      output$ModelPlot <- renderPlot(function(){
        fit <- MODELFIT()
        if(is.null(fit)){return(NULL)}
        plot(fit,pages=1,all.terms=FALSE)
      })
      
      output$ModelDiagnostics <- renderPlot({
        gam.check(MODELFIT())
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
        data <- DATAFILTERED()[["KEEP"]]###data brought in after filtering is complete
        idx <- input$MarginalEffect
        if(is.null(idx)){return(NULL)}
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
        if(is.null(MarginalData)){return(NULL)}
        name <- colnames(MarginalData)
        eval(parse(text=paste0("plot <- ggplot(MarginalData,aes(x=",name[2],",y=",name[1],"))")))
        plot <- plot+geom_point()+geom_line()
        print(plot)
        
      })
      
      
      ###########################################################
      #######Partial Effects Plot (adjusted by nusiance factors)
      ###########################################################
      
      output$PartialEffect <- renderUI({
        data <- DATAFILTERED()[["KEEP"]]###data brought in after filtering is complete
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
        data <- DATAFILTERED()[["KEEP"]]###data brought in after filtering is complete
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
        data <- DATAFILTERED()[["KEEP"]]###data brought in after filtering is complete
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
        if(is.null(dat)){return(NULL)}
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
      
      
      
      ###########################################################
      #######Model Predictions
      ###########################################################
      output$DateRange <- renderUI({
        data <- DATAFILTERED()[["KEEP"]]###data brought in after filtering is complete
        start_date <-max(data$EntryDate)
        yr <- format(start_date,format="%Y")
        mo <- format(start_date,format="%m")
        day <- format(start_date,format="%d")
        yr <- as.numeric(yr)+1
        end_date <- as.Date(format(paste(yr,mo,day,sep="-"),
                                   format="%y-%m-%d"))
        dateRangeInput("DateRange","Select Prediction Date Range ",
                       start=start_date,
                       min=start_date,
                       end=end_date
                       )
      })
      
      
      output$PredictionLevels = renderUI({
        data <- DATAFILTERED()[["KEEP"]]###data brought in after filtering is complete
        fit <- MODELFIT()
        terms <- as.character(fit$pred.formula)[2]
        terms <- unlist(strsplit(terms," + ",fixed=T))
        terms <- terms[!(terms %in% c("NumericDate","Day365"))]###get rid of date terms
        myUIs <- lapply(1:length(terms), function(i) {
          inputname <- paste("PredictorTerms_", terms[i], sep="")
          levs <- data[,terms[i]]
          levs <- unique(levs[order(levs)])
          
          selectInput(inputname, 
                      paste0("Set ",terms[i]," at:"),
                      levs,levs[1])
        })
        do.call(tagList, myUIs)
      })
      
      
      
      PREDICTIONDATA <- reactive({
        data <- DATAFILTERED()[["KEEP"]]###data brought in after filtering is complete
        fit <- MODELFIT()
        terms <- as.character(fit$pred.formula)[2]
        terms <- unlist(strsplit(terms," + ",fixed=T))
        PredTerms <- terms[!(terms %in% c("NumericDate","Day365"))]###get rid of date terms
        DateTerms <- terms[(terms %in% c("NumericDate","Day365"))]###get rid of date terms
        if(is.null(terms)){return(NULL)}
        if(is.null(input$DateRange)){return(NULL)}
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
        
        for(i in PredTerms){
          eval(parse(text=paste("value <- input$PredictorTerms_", i, sep="")))
          class(value) <- class(data[,i])
          eval(parse(text=paste0("PredData <- cbind(PredData,",i,"=value)")))
        }
        preds <- predict(fit,newdata=PredData,se.fit=T)
        y_hat <- preds$fit
        y_se <- sqrt(preds$se.fit^2+fit$sig2)
        LCL <- y_hat+qnorm(input$ConfLimits[1])*y_se
        UCL <- y_hat+qnorm(input$ConfLimits[2])*y_se
        response <- as.character(formula(fit))[2]
        eval(parse(text=paste0("prediction_data <- data.frame(",response,"=y_hat,LCL=LCL,UCL=UCL,PredData)")))
        
        
        ###now we have to generate adjusted response data for this to work
        ###since there are nusiance factors that need to be integrated out
        ids <- length(PredTerms)
        data2 <- data
        
        ####Run the partial predictions
        y_hat <- predict(fit,newdata=data)
        y <- data[,as.character(formula(fit))[2]]
        residual <- y-y_hat
        
        ####fix the data at the constant integration value
        if(ids>0){for(i in 1:ids){
          var <- PredTerms[i]
          eval(parse(text=paste0("value <- input$PredictorTerms_",var)))
          class(value) <- class(data2[,var])
          data2[,var] <- value
        }}
        
        y_delta <- -(y_hat-predict(fit,newdata=data2))
        y_partial <- y_hat+y_delta
        y_residual <- y_partial+residual
        
        observed_data <- data.frame("y_partial"=as.numeric(y_partial),
                          "y_residual"=as.numeric(y_residual),
                          "y_hat"=as.numeric(y_hat),
                          "residual"=as.numeric(residual),
                          data)
        observed_summary <- observed_data %>% 
          group_by(EntryDate) %>%
          summarise(RPM_daily = mean(y_residual,na.rm=T),
                    Prediction = mean(y_partial,na.rm=T))
        observed_summary <- as.data.frame(observed_summary)
        event <- prediction_data$EntryDate[1]-0.5
        out <- list(prediction_data=prediction_data,
                    observed_data=observed_data,
                    event=event,
                    observed_summary=observed_summary)
        
        return(out)
      })
      
      
      output$PredictionPlotInteractive <- renderDygraph({
        preds <- PREDICTIONDATA()[["prediction_data"]]
        data <- PREDICTIONDATA()[["observed_summary"]]
        event <- PREDICTIONDATA()[["event"]]
        fit <- MODELFIT()
        if(is.null(preds)){return(NULL)}
        response <- as.character(formula(fit))[2]
        idx_date_data <- colnames(data) %in% c("EntryDate")
        idx_date_preds <- colnames(preds) %in% c("EntryDate")
        preds <- xts(preds[,c(response,"LCL","UCL"),drop=F],preds[,idx_date_preds])
        data <- xts(data[,c("RPM_daily","Prediction"),drop=F],data[,idx_date_data])
        series <- cbind(data,preds)
        dygraph(series,"Observed & Predicted RPM (Adjusted for Factors)") %>%
          dySeries("RPM_daily",label="Daily RPM") %>%
          dySeries("Prediction",label="Model Fit") %>%
          dySeries(c("LCL",response,"UCL"),label="Predicted RPM") %>%
          dyAxis("y",label="Normalized Rate Per Mile ($)") %>%
          dyRoller(rollPeriod = 1) %>%
          dyEvent(date = event, "Observed/Predicted", labelLoc = "bottom") %>%
          dyRangeSelector()
      })
      
      
      
      output$PredicitonTable <- renderDataTable({
        PREDICTIONDATA()[["prediction_data"]]
      })
      
      output$PredictionFullPlot <- renderPlot({
        dat <- PREDICTIONDATA()[["observed_data"]]
        preds <- PREDICTIONDATA()[["prediction_data"]]
        if(is.null(dat)){return(NULL)}
        if(is.null(input$PredictionPartial)){return(NULL)}
        selected <- input$PredictionPartial
        var <- "EntryDate"
        fit <- MODELFIT()
        wkdata <- data.frame()
        wkdata1 <- data.frame(y=dat[,"y_partial"],x=dat[,var],group="Model Fit")
        wkdata2 <- data.frame(y=dat[,"y_residual"],x=dat[,var],group="Adjusted Observation")
        wkdata3 <- data.frame(y=preds[,as.character(formula(fit))[2]],x=preds[,var],group="Predicted")
        if("Fitted" %in% selected){wkdata <- bind_rows(wkdata,wkdata1)}
        if("Observed" %in% selected){wkdata <- bind_rows(wkdata,wkdata2)}
        if("Predicted" %in% selected){wkdata <- bind_rows(wkdata,wkdata3)}
        p <- ggplot(wkdata,aes(y=y,x=x,color=group))
        p <- p+geom_point()+xlab(var)+ylab(paste(as.character(formula(fit))[2]))
        print(p)
      })
      
      
      ###########################################################
      #######Volume Integrated Quote
      ###########################################################
      
      output$fourierComp <- renderUI({
        numericInput("fourierComp","Number of Fourier Comps",8,min=2,max=40,step=2)
      })
      
      TransactionalVolume <- reactive({
        data <- DATAFILTERED()[["KEEP"]]###data brought in after filtering is complete
        date_window <- input$DateRange
        predDays <- difftime(date_window[2],date_window[1],units="days")
        date_sequence <- date_window[1]+1:predDays
        PredData <- data.frame(EntryDate=date_sequence)
        volume <- data %>% 
          group_by(EntryDate) %>%
          summarise(TransVolume= n())
        volume <- as.data.frame(volume)
        interval <- difftime(max(volume$EntryDate),min(volume$EntryDate),units = "days")
        dateseq <- min(volume$EntryDate)+1:interval
        JoinDat <- data.frame(EntryDate =dateseq)
        volume <- JoinDat %>% left_join(volume)
        volume <- as.data.frame(volume)
        volume$TransVolume[is.na(volume$TransVolume)] <- 0
        volume <- xts(volume[,"TransVolume",drop=F],volume$EntryDate)
        yr <- as.numeric(format(min(dateseq),format="%Y"))
        day <- as.numeric(format(min(dateseq),format="%j"))
        n <- length(volume)
        m <- 365 ###cyclic period for the volume
        components <- input$fourierComp
        volume_ts  <- ts(coredata(volume), f=m)
        fit <- Arima(volume_ts, order=c(0,0,0), xreg=fourier(1:n,components,m))###turn off AR error for now
        #fit <- auto.arima(volume_ts, seasonal=FALSE, xreg=fourier(1:n,components,m))
        #plot(forecast(fit, h=2*m, xreg=fourier(n+1:(2*m),4,m)))
        len <- as.numeric(predDays)
        pred_volume <- forecast(fit,h=len,xreg=fourier(n+1:len,components,len))
        pred_volume <- data.frame(TransFcst=pred_volume$mean)
        pred_volume <- xts(pred_volume,date_sequence)
        return(list(volume=volume,pred_volume=pred_volume))
      })

      
      output$VolumeDraw <- renderdyPencilgraph({
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
        if(input$DrawUpdate==0){
          return(list(pred_volume=pred_volume))
        }
        raw <- isolate(input$VolumeDraw_data_extract) 
        dimension <- isolate(input$VolumeDraw_data_dimension_RowCol) 
        out <- matrix(raw,nrow=dimension[[1]],ncol=dimension[[2]],byrow = T) 
        out <- as.data.frame(out) 
        coredata(pred_volume) <- out[,2]
        return(list(pred_volume=pred_volume))
      })
      
      
      
      VolumeDataPrep <- reactive({
        preds <- PREDICTIONDATA()[["prediction_data"]]
        data <- PREDICTIONDATA()[["observed_summary"]]
        event <- PREDICTIONDATA()[["event"]]
        volume <- TransactionalVolume()[["volume"]]
        pred_volume <- PASSVOLUME()[["pred_volume"]]
        fit <- MODELFIT()
        if(is.null(preds)){return(NULL)}
        response <- as.character(formula(fit))[2]
        idx_date_data <- colnames(data) %in% c("EntryDate")
        idx_date_preds <- colnames(preds) %in% c("EntryDate")
        preds <- xts(preds[,c("LCL",response,"UCL"),drop=F],preds[,idx_date_preds])
        data <- xts(data[,c("RPM_daily"),drop=F],data[,idx_date_data])
        vol_int_rate <- numeric(length=3)
        names(vol_int_rate) <- c("LCL",response,"UCL")
        vol_int_rate[1] <- weighted.mean(coredata(preds)[,1],coredata(pred_volume))
        vol_int_rate[2] <- weighted.mean(coredata(preds)[,2],coredata(pred_volume))
        vol_int_rate[3] <- weighted.mean(coredata(preds)[,3],coredata(pred_volume))
        vol_int_rate <- round(vol_int_rate,2)
        series <- cbind(data,preds,volume,pred_volume)
        series$RPM_daily[index(series)<index(preds)[1]] <- na.approx(series$RPM_daily[index(series)<index(preds)[1]])
        name <- paste0("Volume Integrated Quote: $",vol_int_rate[2]," ($",vol_int_rate[1],", $",vol_int_rate[3],") Per Mile")
        return(list(series=series,vol_int_rate=vol_int_rate,event=event,
                    response=response,data=data,preds=preds,volume=volume,
                    name=name))
      })
      
      output$VolumeIntegrated <- renderDygraph({
        series <- VolumeDataPrep()[["series"]]
        response <- VolumeDataPrep()[["response"]]
        vol_int_rate <- VolumeDataPrep()[["vol_int_rate"]]
        data <- VolumeDataPrep()[["data"]]
        preds <- VolumeDataPrep()[["preds"]]
        volume <- VolumeDataPrep()[["volume"]]
        event <- VolumeDataPrep()[["event"]]
        name <- VolumeDataPrep()[["name"]]
        dygraph(series,name) %>%
          dySeries("RPM_daily",label="Daily RPM") %>%
          dySeries("TransVolume",label="Transactional Volume",
                   axis='y2',stepPlot = TRUE, fillGraph = TRUE) %>%
          dySeries("TransFcst",label="Transactional Volume FCST",
                   axis='y2',stepPlot = TRUE, fillGraph = TRUE) %>%
          dySeries(c("LCL",response,"UCL"),label="Predicted RPM") %>%
          dyAxis("y",label="Normalized Rate Per Mile ($)",valueRange=c(0, max(max(data),max(preds)))) %>%
          dyAxis("y2", label = "Transacitonal Volume", 
                 independentTicks = TRUE, valueRange = c(0, max(volume)*1.5)) %>%
          dyRoller(rollPeriod = 1) %>%
          dyEvent(date = event, "Observed/Predicted", labelLoc = "bottom")
      })
      
      
      
      ###########################################################
      #######Historical Volume Integrated Quote
      ###########################################################
      HistoricalData <- reactive({
      if(is.null(VolumeDataPrep())){return(NULL)}
      series <- VolumeDataPrep()[["series"]]
      response <- VolumeDataPrep()[["response"]]
      vol_int_rate <- VolumeDataPrep()[["vol_int_rate"]]
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
        tmp <- series[paste(lower,upper,sep="::")]
        if(i==loop & (min(index(tmp))>as.Date(lower))){break}###dont add value if not complete cycle
        quote <- rbind(quote,data.frame(
                            "StartDate"=as.Date(lower),
                            "EndDate"=as.Date(upper),
                            "MidPoint"=as.Date(lower)+difftime(as.Date(upper),as.Date(lower))/2,
                            "WeightedRPM"=weighted.mean(tmp$RPM_daily,tmp$TransFcst,na.rm = T)))
      }
      

      ###add on the predicted quote
      quote <- rbind(data.frame(
        "StartDate"=dte_window[1],
        "EndDate"=dte_window[2],
        "MidPoint"=dte_window[1]+difftime(dte_window[2],dte_window[1])/2,
        "WeightedRPM"=as.numeric(vol_int_rate[2])),quote)
      
      return(list(series=series,vol_int_rate=vol_int_rate,event=event,
                  response=response,data=data,preds=preds,volume=volume,
                  name=name,quote=quote))
      })
      
      
      output$Historical <- renderDygraph({
        if(is.null(HistoricalData())){return(NULL)}
        series <- HistoricalData()[["series"]]
        response <- HistoricalData()[["response"]]
        vol_int_rate <- HistoricalData()[["vol_int_rate"]]
        data <- HistoricalData()[["data"]]
        preds <- HistoricalData()[["preds"]]
        volume <- HistoricalData()[["volume"]]
        event <- HistoricalData()[["event"]]
        name <- HistoricalData()[["name"]]
        dygraph(series,"Historical RPM & Volume Pattern") %>%
          dySeries("RPM_daily",label="Daily RPM") %>%
          dySeries("TransFcst",label="Repeated Volume FCST",
                   axis='y2',stepPlot = TRUE, fillGraph = TRUE) %>%
          dySeries(c("LCL",response,"UCL"),label="Predicted RPM") %>%
          dyAxis("y",label="Normalized Rate Per Mile ($)",valueRange=c(0, max(max(data),max(preds)))) %>%
          dyAxis("y2", label = "Transacitonal Volume", 
                 independentTicks = TRUE, valueRange = c(0, max(volume)*1.5)) %>%
          dyRoller(rollPeriod = 1) %>%
          dyEvent(date = event, "Observed/Predicted", labelLoc = "bottom")
      })
      
      
      
      output$HistVolIntegrated<- renderDygraph({
        if(is.null(HistoricalData())){return(NULL)}
        quote <- HistoricalData()[["quote"]]
        event <- HistoricalData()[["event"]]
        quote <- xts(quote[,"WeightedRPM",drop=F],quote$MidPoint)
        dygraph(quote,"Historical RPM") %>%
          dySeries("WeightedRPM",label="Volume Weighted RPM") %>%
          dyAxis("y",label="Normalized Rate Per Mile ($)") %>%
          dyOptions(drawPoints = TRUE, pointSize = 5) %>%
          dyEvent(date = event, "Observed/Predicted", labelLoc = "bottom")
      })
      

      output$HistVolIntegratedTable<- renderDataTable({
        if(is.null(HistoricalData())){return(NULL)}
        quote <- HistoricalData()[["quote"]]
        return(quote)
      })


      
      

  
})###end server here
