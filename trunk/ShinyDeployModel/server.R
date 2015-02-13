

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
        if(!is.null(mapOrig)){
          map(mapOrig,fill=T,add=T,col="grey95")
        }
        map.text(state_labs,add=T)
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
        if(!is.null(mapDest)){
          map(mapDest,fill=T,add=T,col="grey95")
        }
        map.text(state_labs,add=T)
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
        if(!is.null(mapOrig)){
          map(mapOrig,fill=T,add=T,col="grey95")
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
        if(!is.null(mapDest)){
          map(mapDest,fill=T,add=T,col="grey95")
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
      #######Remove fixed rate observations (if desired)
      ###########################################################
      
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
        SELECTED <- DATA()[["SELECTED"]]
        if(is.null(SELECTED)){return(NULL)}
        if(is.null(RemoveGroups$x)){return(NULL)}
        train <- data.frame(as.numeric(SELECTED$EntryDate),SELECTED$RPM_NormalizedCustomer)
        lower <- unlist(apply(train,2,min))
        upper <- unlist(apply(train,2,max))
        scaling <- upper-lower
        train <- scale(train,center=F,scale=scaling)
        cl <- factor(SELECTED$CustomerCarrier)
        test <- data.frame(RemoveGroups$x,RemoveGroups$y)
        test <- scale(test,center=F,scale=scaling)
        p <- knn1(train, test, cl)
        p <- as.character(p)
        return(p)
      })
      
      output$RemoveCustomerCarrier <- renderUI({
        SELECTED <- DATA()[["SELECTED"]]
        if(is.null(SELECTED)){return(NULL)}
        pick <- SELECTED$CustomerCarrier
        remove <- ClickRemovalPoints()
        isolate(selected <- input$RemoveCustomerCarrier)
        selected <- selected[!is.null(selected)]
        selected <- c(selected,remove)
        selected <- unique(selected)
        selected <- selected[!is.null(selected)]
        selectizeInput("RemoveCustomerCarrier","Customer Carrier Groups to Remove",choices=pick,selected=selected,multiple=T)
      })
      
      
      DATAFILTERED <- reactive({
        SELECTED <- DATA()[["SELECTED"]]
        if(is.null(SELECTED)){return(NULL)}
        pull <- input$RemoveCustomerCarrier
        idx_toss <- SELECTED$CustomerCarrier %in% pull
        KEEP <- SELECTED %>% filter(!idx_toss)
        TOSS <- SELECTED %>% filter(idx_toss)
        return(list(KEEP=KEEP,TOSS=TOSS))
      })
      
      
      output$RemovalPlot <- renderPlot(function(){
        if(is.null(DATAFILTERED)){return(NULL)}
        KEEP <- DATAFILTERED()[["KEEP"]]
        TOSS <- DATAFILTERED()[["TOSS"]]
        plot(x=KEEP$EntryDate,y=KEEP$RPM_NormalizedCustomer,type="p",pch=19,col="grey75")
        points(x=TOSS$EntryDate,y=TOSS$RPM_NormalizedCustomer,type="p",pch=19,col="black")
        legend("topright",c("Kept","Removed"),pch=19,col=c("grey75","black"))
      })
      
      
      
      
      
  
})###end server here
