rand.ci<-function(q,title){
  y<-0
  plot(0,1, xlim=c(-2,2), ylim=c(0,21), abline(v=0), las=1,xlab="", ylab = "")
  title(main = paste(title))
  for (i in 1:20){
    x<- rnorm(10)
    ci<-c((mean(x)-qt(q, 9)*(sd(x)/sqrt(10))),
          (mean(x)+qt(q, 9)*(sd(x)/sqrt(10))))
    y<-y+1
    
    if(ci[1]<=0&&ci[2]>=0){
      par(new=T)
      lines(c(ci[1],ci[2]), c(y,y), lty=1,lwd=3, col="steelblue")
    }
    else{
      par(new=T)
      lines(c(ci[1],ci[2]), c(y,y), lty=1,lwd=3, col="indianred")
    }
  }
}




shinyServer(function(input, output, session){

  goplot1<-eventReactive(input$click1,{rand.ci(.95, "90% C.I.'s")})
  goplot2<-eventReactive(input$click2,{rand.ci(.975, "95% C.I.'s")})
  goplot3<-eventReactive(input$click3,{rand.ci(.995, "99% C.I.'s")})
  goplot4<-eventReactive(input$click4,{rand.ci(1-((1-as.numeric(input$q))/2), "Choose Your \nOwn")})
  
  output$plot1<-renderPlot({
    goplot1()
  })
  output$plot2<-renderPlot({
    goplot2()
  })
  output$plot3<-renderPlot({
    goplot3()
  })
  output$plot4<-renderPlot({
    goplot4()
  })
  
  
})