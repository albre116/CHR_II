

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output, session) {
  
      OriginAdd<- reactiveValues(x=NULL, y=NULL)
      OriginDelete<- reactiveValues(x=NULL, y=NULL)


      
      observe({
        # Initially will be empty
        if (is.null(input$AddOrigin)){
          return() 
        }
        isolate(OriginAdd$x <- c(OriginAdd$x, input$AddOrigin$x))
        isolate(OriginAdd$y <- c(OriginAdd$y, input$AddOrigin$y))
      })
      
      observe({
        # Initially will be empty
        if (is.null(input$DeleteOrigin)){
          return() 
        }
        isolate(OriginDelete$x <- c(OriginDelete$x, input$DeleteOrigin$x))
        isolate(OriginDelete$y <- c(OriginDelete$y, input$DeleteOrigin$y))
      })
      
      DATASTATES <- reactive({
        if(!is.null(input$AddOrigin)){
          AddOrigStates <- data.frame(state=map.where(states,
                                                      x=OriginAdd$x,
                                                      y=OriginAdd$y))
          AddOrigStates$Value <- 1
          AddOrigStates <- AddOrigStates[complete.cases(AddOrigStates),]
        } else{AddOrigStates <- data.frame(state="California",Value=0)}
        if(!is.null(input$DeleteOrigin)){
          DeleteOrigStates <- data.frame(state=map.where(states,
                                                         x=OriginDelete$x,
                                                         y=OriginDelete$y))
          DeleteOrigStates$Value <- -1
          DeleteOrigStates <- DeleteOrigStates[ complete.cases(DeleteOrigStates),]
        } else{DeleteOrigStates <- data.frame(state="California",Value=0)}
        
        ####figure out the current state of the clicks by mappking (k,v) pairs
        STATES <- bind_rows(AddOrigStates,DeleteOrigStates) %>% 
          group_by(state) %>%
          summarise(count=sum(Value) )%>% 
          filter(count>=1)
        if(nrow(STATES)>0){
          mapOrig <- map("state",regions = STATES$state,plot=F,fill=T,col="grey")} else{mapOrig <- NULL}
        list(mapOrig=mapOrig)
      })
      
      output$OrigPlotAdd <- renderPlot(function(){
        mapOrig <- DATASTATES()[["mapOrig"]]
        map(states)
        if(!is.null(mapOrig)){
          map(mapOrig,fill=T,add=T,col="grey")
        }
        map.text(state_labs,add=T)
      })
      
      
      output$OrigPlotDelete <- renderPlot(function(){
        mapOrig <- DATASTATES()[["mapOrig"]]
        map(states)
        if(!is.null(mapOrig)){
          map(mapOrig,fill=T,add=T,col="grey")
        }
        map.text(state_labs,add=T)
      })
      
      
      ####do the destination stuff
      DestAdd<- reactiveValues(x=NULL, y=NULL)
      DestDelete<- reactiveValues(x=NULL, y=NULL)
      
      observe({
        # Initially will be empty
        if (is.null(input$AddDest)){
          return() 
        }
        isolate(DestAdd$x <- c(DestAdd$x, input$AddDest$x))
        isolate(DestAdd$y <- c(DestAdd$y, input$AddDest$y))
      })
      
      observe({
        # Initially will be empty
        if (is.null(input$DeleteDest)){
          return() 
        }
        isolate(DestDelete$x <- c(DestDelete$x, input$DeleteDest$x))
        isolate(DestDelete$y <- c(DestDelete$y, input$DeleteDest$y))
      })
      
      
      DATASTATES2 <- reactive({
        if(!is.null(input$AddDest)){
        AddDestStates <- data.frame(state=map.where(states,
                                                    x=DestAdd$x,
                                                    y=DestAdd$y))
        AddDestStates$Value <- 1
        AddDestStates <- AddDestStates[complete.cases(AddDestStates),]
        } else{AddDestStates <- data.frame(state="California",Value=0)}
        if(!is.null(input$DeleteDest)){
        DeleteDestStates <- data.frame(state=map.where(states,
                                                       x=DestDelete$x,
                                                       y=DestDelete$y))
        DeleteDestStates$Value <- -1
        DeleteDestStates <- DeleteDestStates[ complete.cases(DeleteDestStates),]
        } else{DeleteDestStates <- data.frame(state="California",Value=0)}
        
        ####figure out the current state of the clicks by mappking (k,v) pairs
        STATES <- bind_rows(AddDestStates,DeleteDestStates) %>% 
          group_by(state) %>%
          summarise(count=sum(Value) )%>% 
          filter(count>=1)
        if(nrow(STATES)>0){
        mapDest <- map("state",regions = STATES$state,plot=F,fill=T,col="grey")} else{mapDest <- NULL}
        list(mapDest=mapDest)
      })
      
      output$DestPlotAdd <- renderPlot(function(){
        mapDest <- DATASTATES2()[["mapDest"]]
        map(states)
        if(!is.null(mapDest)){
          map(mapDest,fill=T,add=T,col="grey")
        }
        map.text(state_labs,add=T)
      })
        
        
      output$DestPlotDelete <- renderPlot(function(){
        mapDest <- DATASTATES2()[["mapDest"]]
        map(states)
        if(!is.null(mapDest)){
          map(mapDest,fill=T,add=T,col="grey")
        }
        map.text(state_labs,add=T)
        })
      

  
  
})###end server here
