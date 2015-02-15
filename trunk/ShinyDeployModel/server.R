

# Define server logic required to plot various variables against mpg
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
        high <- max(DATA$RPM_NormalizedCustomer,na.rm = TRUE)
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
      
      
      
      
      
  
})###end server here
