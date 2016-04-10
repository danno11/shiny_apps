shinyServer(function(input, output, session){
  
  output$ui<-renderUI({
    switch(input$option,
           "Summary Stats" = HTML( 
             '<div class="form-group shiny-input-container">
               <label for="barx1">$$\\bar{x}$$</label>
           <input id="barx1" type="text" class="form-control" value="0"/>
           </div>
           <div class="form-group shiny-input-container">
           <label for="picksd1">$$sd$$</label>
           <input id="picksd1" type="text" class="form-control" value="1"/>
           </div>
           <div class="form-group shiny-input-container">
           <label for="n1">$$n$$</label>
           <input id="n1" type="text" class="form-control" value="10"/>
           </div>'
),
           "Input Data" = tags$textarea(id="foo", rows=10, cols=38))

  })
  
  
  
  goplot<-eventReactive(input$click,{
    
    if(input$option=="Summary Stats"){
      
      samp1.mean<-as.numeric(input$barx1)
      samp1.sd<-as.numeric(input$picksd1)
      samp1.min<- samp1.mean-3*samp1.sd
      samp1.max<- samp1.mean+3*samp1.sd
      df1<-as.numeric(input$n1)-1
      me1<-qt(.975,df1)*(samp1.sd/sqrt(as.numeric(input$n1)))
      low1<-samp1.mean - me1
      up1<- samp1.mean +me1
      plot (function( x ) dnorm( x, mean=samp1.mean,sd=samp1.sd), 
            samp1.min, samp1.max, ylim = c(0,1), xlim = c(samp1.min, samp1.max), 
            ylab= "",axes=F, lwd=2, col="indianred")
      lines(c(samp1.mean, samp1.mean), c(0,.7), col="indianred", lty = 2)
      lines(c(low1, up1), c(.7, .7), col="indianred",lwd=2.5)
      text(low1-.23,.65, paste("(",round(low1,2),")"))
      text(up1+.23,.65, paste("(",round(up1,2),")"))
      axis(1)
      axis(2)
      
      
    } else {
      x<-as.numeric(strsplit(input$foo, "\\,\\s|\\,|\\s")[[1]])
      
      samp1.mean<-mean(x)
      samp1.sd<-sd(x)
      samp1.min<- samp1.mean-3*samp1.sd
      samp1.max<- samp1.mean+3*samp1.sd
      df1<-length(x)-1
      me1<-qt(.975,df1)*(samp1.sd/sqrt(as.numeric(length(x))))
      low1<-samp1.mean - me1
      up1<- samp1.mean +me1
      plot (function( x ) dnorm( x, mean=samp1.mean,sd=samp1.sd), 
            samp1.min, samp1.max, ylim = c(0,1), xlim = c(samp1.min, samp1.max), 
            ylab= "",axes=F, lwd=2, lty=2, col="indianred")
      lines(c(samp1.mean, samp1.mean), c(0,.7), col="indianred",lty=2)
      lines(c(low1, up1), c(.7, .7), col="indianred",lwd=2.5)
      text(low1-.23,.65, paste("(",round(low1,2),")"))
      text(up1+.23,.65, paste("(",round(up1,2),")"))
      axis(1)
      axis(2)
    }
    
    
    
    
  })
  
  output$plot1<-renderPlot({
    goplot()
  })

  gotest<-eventReactive(input$test, {
    if(input$option=="Summary Stats"){
      
      samp1.mean<-as.numeric(input$barx1)
      samp1.sd<-as.numeric(input$picksd1)
      samp1.min<- samp1.mean-3*samp1.sd
      samp1.max<- samp1.mean+3*samp1.sd
      n1<-as.numeric(input$n1)
      df1<-n1-1
      
      }else {
        x<-as.numeric(strsplit(input$foo, "\\,\\s|\\,|\\s")[[1]])
        samp1.mean<-mean(x)
        samp1.sd<-sd(x)
        samp1.min<- samp1.mean-3*samp1.sd
        samp1.max<- samp1.mean+3*samp1.sd
        n1<-length(x)
        df1<-n1-1
      }
      
      ho.val<- as.numeric(input$ho)
      diff<- samp1.mean - ho.val
      standard.error<- samp1.sd/sqrt(n1)
      test.stat <- diff/standard.error
    
    p.val<- switch(input$alt,
                   "mu > "  = pt(test.stat, df1, lower.tail = FALSE),
                   "mu <" = pt(test.stat, df1),
                   "mu NOT equal to" = if (test.stat < 0) {
                     2*pt(test.stat, df1)
                   } else {
                     2*pt(test.stat, df1, lower.tail = FALSE)    
                   })
    
    
    alter.hyp<-switch(input$alt,
                      "mu > " = paste("\\(\\mu > \\)",input$ho),
                      "mu <" = paste("\\(\\mu < \\)",input$ho),
                      "mu NOT equal to" = paste("\\(\\mu\\neq\\) = ", input$ho))
    
    
    hoh <- switch(input$alt,
                  "mu > "  = paste("\\(\\mu\\leq\\)",input$ho),
                  "mu <" = paste("\\(\\mu\\geq\\)",input$ho),
                  "mu NOT equal to" = paste("\\(\\mu\\) = ", input$ho))
    
    
    str1<- withMathJax(paste("\\(H_0\\) : ", hoh))
    str2<- withMathJax(paste("\\(H_1\\) : ", alter.hyp))
    str3<- paste("Test Statistic = ", round(test.stat, 5), "df =", paste(df1))
    str4<- paste("p-value = ", signif(p.val))
    HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
    
  })
  
  goplot2<- eventReactive(input$tclick,{
    
    if(input$option=="Summary Stats"){
      
      samp1.mean<-as.numeric(input$barx1)
      samp1.sd<-as.numeric(input$picksd1)
      samp1.min<- samp1.mean-3*samp1.sd
      samp1.max<- samp1.mean+3*samp1.sd
      n1<-as.numeric(input$n1)
      df1<-n1-1
      
    }else {
      x<-as.numeric(strsplit(input$foo, "\\,\\s|\\,|\\s")[[1]])
      samp1.mean<-mean(x)
      samp1.sd<-sd(x)
      samp1.min<- samp1.mean-3*samp1.sd
      samp1.max<- samp1.mean+3*samp1.sd
      n1<-length(x)
      df1<-n1-1
    }
    
    ho.val<- as.numeric(input$ho)
    diff<- samp1.mean - ho.val
    standard.error<- samp1.sd/sqrt(n1)
    test.stat <- diff/standard.error
   
    ub<-abs(diff) + 3
    lb<- -1*abs(diff) - 3
    
    p.val<- switch(input$alt,
                   "mu > "  = pt(test.stat, df1, lower.tail = FALSE),
                   "mu <" = pt(test.stat, df1),
                   "mu NOT equal to" = if (test.stat < 0) {
                     2*pt(test.stat, df1)
                   } else {
                     2*pt(test.stat, df1, lower.tail = FALSE)    
                   })
    
    plot( function(x) dt(x, (df1)), xlim = c(-1*abs(diff) - 3, abs(diff)+3), 
          ylim = c(0,1), lwd = 2,lty = 1, col = "orange", ylab = "",
          main = paste("If the null hypothesis were TRUE, \nThe Probability of a Test Stat this Extreme \nis", signif(p.val)))
    abline(v = test.stat, col = "steelblue", lty = 2, lwd = 1.5)
    if (input$alt == "mu > "){
      polygon(x=c(min(test.stat, ub), seq(min(test.stat, ub), max(test.stat, ub), .01), max(test.stat, ub)),
              y=c(0, dt(seq(min(test.stat, ub), max(test.stat, ub), .01), df1), 0), col = "grey44")
    } else if (input$alt == "mu <") {
      polygon(x=c(min(test.stat, lb), seq(min(test.stat, lb), max(test.stat, lb), .01), max(test.stat, lb)),
              y=c(0, dt(seq(min(test.stat, lb), max(test.stat, lb), .01), df1), 0), col = "grey44")
    } else {
      polygon(x=c(min(lb,-1*abs(test.stat)), seq(min(lb,-1*abs(test.stat)), max(lb,-1*abs(test.stat)), .01), max(lb,-1*abs(test.stat)),
                  min(abs(test.stat), ub), seq(min(abs(test.stat), ub), max(abs(test.stat), max(abs(test.stat), ub)), .01), ub),
              y=c(0, dt(seq(min(lb,-1*abs(test.stat)), max(lb,-1*abs(test.stat)), .01), df1), 0,
                  0, dt(seq(min(abs(test.stat), ub), max(abs(test.stat), ub), .01), df1), 0), col = "grey44")
    }
    
    
  })
  
  
  
  
  output$answers<-renderUI({
    gotest()
  })
  
  output$tgraph<- renderPlot({ 
    
    goplot2() 
    
  })
  
  
  
})