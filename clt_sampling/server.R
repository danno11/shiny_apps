library(gridExtra)
library(ggplot2)
shinyServer(function(input, output, session){
  goplot1<-eventReactive(input$click,{
    
    xbar<-0
    for (i in 1:50){
      char<-switch(input$dist,
                   "rexp("="rate = .2)",
                   "runif("="min = 2, max = 8)",
                   "rnorm(" = "mean = 5, sd = 2)")
      x<-eval(parse(text=paste(input$dist, input$n, ",", char)))
      xbar[i]<-mean(x)
      df<-data.frame(xbar)
      
    }
    sample_text <- paste("bar(x)==", round(mean(xbar),2))#, "sd==", round(sd(xbar),2))
    bottom.plot<-ggplot(df, aes(xbar)) + geom_histogram(binwidth=.25, 
                colour="black", fill="white") +
      scale_x_continuous(name = "Distribution of 50 Sample Means", limits = c(1,9), breaks = c(seq(1, 9, .5))) + 
      geom_vline(xintercept = 5, colour = "#009E73", size = 1.2) +  
      geom_vline(xintercept = mean(xbar), colour = "#D55E00", size= 1.2) + annotate("text", x=mean(xbar)+.4, y=2, label = sample_text, parse = TRUE)
                               
    x.points<-seq(1, 9, .01)
    y1<-dexp(x.points, .2)
    y2<-dunif(x.points, min = 2, max = 8 )
    y3<-dnorm(x.points, mean=5, sd = 2)
    ed<-data.frame(x.points, y1, y2, y3)
    plotmath_text <- "mu==5"
    
    exponential.emp<-ggplot(ed, aes(x=x.points, y=y1)) + geom_line() + 
      geom_vline(xintercept = 5, colour = "#009E73", size = 1.2) +
      scale_x_continuous(name = "True Empirical Distribution", limits = c(1,9), breaks = c(seq(1, 9, .5))) + 
      scale_y_continuous("density") + annotate("text",x = 4.5, y = .15, cex = 5, label = plotmath_text, parse = TRUE)   
      
    
    uniform.emp<- ggplot(ed, aes(x=x.points, y=y2)) + geom_line() + 
      geom_vline(xintercept = 5, colour = "#009E73", size = 1.2) +
      scale_x_continuous(name = "True Empirical Distribution", limits = c(1,9), breaks = c(seq(1, 9, .5))) + 
      scale_y_continuous("density") + annotate("text",x = 4.5, y = .15, cex = 5, label = plotmath_text, parse = TRUE)  
      
    
    normal.emp<-ggplot(ed, aes(x=x.points, y=y3)) + geom_line() + 
      geom_vline(xintercept = 5, colour = "#009E73", size = 1.2) +
      scale_x_continuous(name = "True Empirical Distribution", limits = c(1,9), breaks = c(seq(1, 9, .5))) + 
      scale_y_continuous("density") + annotate("text",x = 4.5, y = .15, cex = 5, label = plotmath_text, parse = TRUE)  
      
    
    top.plot<-switch(input$dist,
                     "rexp(" = exponential.emp,
                     "runif(" = uniform.emp,
                     "rnorm(" = normal.emp)
    
    bottom.plot <- ggplot_gtable(ggplot_build(bottom.plot))
    top.plot<- ggplot_gtable(ggplot_build(top.plot))
    maxWidth = unit.pmax(top.plot$widths[2:3], bottom.plot$widths[2:3])
    top.plot$widths[2:3] <- maxWidth
    bottom.plot$widths[2:3] <- maxWidth
    
    
    grid.arrange(top.plot, bottom.plot, ncol = 1)
    
  })
  
  output$plot1<-renderPlot({
    goplot1()
    
     
    
    
  })  
})




