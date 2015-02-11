

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
  
  
})###end server here
