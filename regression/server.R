library(ggplot2)
mtcars$cyl<-factor(mtcars$cyl)
mtcars$carb<-factor(mtcars$carb)
mtcars$gear<- factor(mtcars$gear)
mtcars$am<-factor(mtcars$am)


shinyServer(function(input, output, session) {
  

    # For storing which rows have been excluded
    vals <- reactiveValues(
      keeprows = rep(TRUE, nrow(mtcars))
    )
    
      
      
    output$plot1 <- renderPlot({
      # Plot the kept and excluded points as two separate data sets
      keep    <- mtcars[ vals$keeprows, , drop = FALSE]
      exclude <- mtcars[!vals$keeprows, , drop = FALSE]
      
      if (input$xcol=="cyl" || input$xcol=="am" || input$xcol=="carb" || input$xcol=="gear"){
        ggplot(keep, aes_string(input$xcol, input$ycol)) + geom_violin() +geom_point()
          
            
           } else{
        ggplot(keep, aes_string(input$xcol, input$ycol)) + geom_point() +
          geom_smooth(method = lm, fullrange = TRUE, shape = 21, color = "black") +
          geom_point(data = exclude, shape = 21, fill = NA, color = "black", alpha = 0.25) 
        
      }
    })

    
    # Toggle points that are clicked
    observeEvent(input$plot1_click, {
      res <- nearPoints(mtcars, input$plot1_click, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
    })
    
    # Toggle points that are brushed, when button is clicked
    observeEvent(input$exclude_toggle, {
      res <- brushedPoints(mtcars, input$plot1_brush, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
    })
    
    
    
    #Hover info
    output$point_hoverinfo <- renderPrint({
      cat("Car Info:\n")
      nearPoints(mtcars, input$point_hover, addDist = TRUE)
    })
   
    output$lm_info <-renderPrint({
      keep    <- mtcars[ vals$keeprows, , drop = FALSE]
      exclude <- mtcars[!vals$keeprows, , drop = FALSE]
      
      summary(lm(as.formula(paste(input$ycol,"~",input$xcol)), data=keep))
      
      
    })
     
    # Reset all points
    observeEvent(input$exclude_reset, {
      vals$keeprows <- rep(TRUE, nrow(mtcars))
    })
    
  }  )






