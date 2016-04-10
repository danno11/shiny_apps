shinyServer(function(input, output, session){
  adj.norm.p<-function(lb,ub,mu,sigma,n){
    n<-as.numeric(input$n)
    p<-as.numeric(input$p)
    mu<-n*p
    sigma<-sqrt(n*p*(1-p))
    lb<-as.numeric(input$xrange[1])
    ub<-as.numeric(input$xrange[2])
    if (lb!=0){
      ad.lb<-lb-.5
    } else{
      ad.lb<-0
    }
    if (ub!=n){
      ad.ub<-ub+.5
    }else{
      ad.ub<-n
    }
    pnorm(ad.ub, mu, sigma)-pnorm(ad.lb, mu, sigma)  
  }
  
  observe({
    val<-input$n
    updateSliderInput(session, "xrange",
                      max = val,
                      value = c(2,val)
    )
  })
  
  goplot<-eventReactive(input$click,{
    n<-as.numeric(input$n)
    p<-as.numeric(input$p)
    mu<-n*p
    sigma<-sqrt(n*p*(1-p))
    lb<-input$xrange[1]
    ub<-input$xrange[2]
    if (lb!=0){
      ad.lb<-lb-.5
    } else{
      ad.lb<-0
    }
    if (ub!=n){
      ad.ub<-ub+.5
    }else{
      ad.ub<-n }
    
    x.bin<-seq(0,n, by=1)
    y.bin<-dbinom(x.bin,n,p)
    
    plot (function( x ) dnorm( x, mean=mu,sd=sigma), 
          0, n, xlim = c(0, n), 
          ylab= "",axes=F, lwd=2, lty=1, col="indianred")
    lines(c(ad.lb, ad.lb), c(0, dnorm(ad.lb, mean = mu, sd=sigma)), col="purple4", lwd=2)
    lines(c(ad.ub, ad.ub), c(0, dnorm(ad.ub, mean = mu, sd=sigma)), col="purple4", lwd=2)
    polygon(x=c(lb, seq(lb, ub, .01), ub), y=c(0, dnorm(seq(lb, ub, .01), mu, sigma), 0), col="grey80")
    polygon(x=c(ad.lb, seq(ad.lb, lb, .01),lb, ub, seq(ub, ad.ub, .01), ad.ub),
            y=c(0, dnorm(seq(ad.lb, lb, .01), mu, sigma), 0, 0, dnorm(seq(ub, ad.ub, .01), mu, sigma), 0), col="grey44") 
    par(new=TRUE)
    plot(x.bin ,y.bin, type = "h",
         xlim = c(0, n),ylab="", axes=F, lwd=2,
         xlab="", col="steelblue", main = "Binomial Probabilities \nand Normal Approximations")
    points(x=c(seq(lb, ub, 1)), y=c(dbinom(seq(lb, ub, 1), n, p)), col="dodgerblue4", cex=1.5, pch=18)
    
    axis(1)
    axis(2)
    
    
    
    
    
    
    
  })
  goprob<-eventReactive(input$click2,{
    n<-as.numeric(input$n)
    p<-as.numeric(input$p)
    mu<-n*p
    sigma<-sqrt(n*p*(1-p))
    lb<-input$xrange[1]
    ub<-input$xrange[2]
    norm.p.val<- pnorm(ub, mu, sigma)-pnorm(lb, mu, sigma)
    bin.p.val <- pbinom(ub, n, p) - pbinom(lb-1, n, p)
    str1<- withMathJax(helpText(paste("Probability of: $$", lb, "\\leq x \\leq", ub, "$$" )))
    str2<- paste("Normal Probability = ",round(norm.p.val,6))
    str3<- paste("Adjusted Normal = ",round(adj.norm.p(lb, ub, mu, sigma),6))
    str4<- paste("Exact Binomial Probability = ", round(bin.p.val,6))
    HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
    
  })
  
    
    
  
    
  output$plot<-renderPlot({
    goplot()
  })
  output$probabilities<-renderUI({
    goprob()
  })

  
  })
  
  
  
  
  
  
  
  
  
  
