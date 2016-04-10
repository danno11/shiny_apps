shinyServer(function(input, output, session){
  
  output$ui<-renderUI({
    switch(input$option,
           "Summary Stats" = HTML(
             '<div class="form-group shiny-input-container">
               <label for="barx1">$$\\bar{x}_1$$</label>
               <input id="barx1" type="text" class="form-control" value="0"/>
               </div>
             <div class="form-group shiny-input-container">
               <label for="picksd1">$$sd_1$$</label>
              <input id="picksd1" type="text" class="form-control" value="1"/>
              </div>
             <div class="form-group shiny-input-container">
               <label for="n1">$$n_1$$</label>
             <input id="n1" type="text" class="form-control" value="10"/>
             </div>
            <div class="form-group shiny-input-container">
              <label for="barx2">$$\\bar{x}_2$$</label>
             <input id="barx2" type="text" class="form-control" value="1"/>
             </div> 
            <div class="form-group shiny-input-container">
              <label for="picksd2">$$sd_2$$</label>
             <input id="picksd2" type="text" class="form-control" value="1"/>
             </div> 
            <div class="form-group shiny-input-container">
              <label for="n2">$$n_2$$</label>
             <input id="n2" type="text" class="form-control" value="10"/>
             </div>' 
           ),
           "Input Data" = HTML(
             '<div class="form-group shiny-input-container">
                  <label for="foo1">Sample 1</label>
             <textarea id="foo1" rows="10" cols="38"></textarea>
             </div>
             <div class="form-group shiny-input-container">
             <label for="foo2">Sample 2</label>
             <textarea id="foo2" rows="10" cols="38"></textarea>
             </div>'
           ))
    
    
  })
  
  
  
  
  goplot<-eventReactive (input$click,{ 
    
    if(input$option=="Summary Stats"){
      samp1.mean<-as.numeric(input$barx1)
      samp2.mean<- as.numeric(input$barx2)
      samp1.sd<-as.numeric(input$picksd1)
      samp2.sd<-as.numeric(input$picksd2)
      n1<-as.numeric(input$n1)
      n2<-as.numeric(input$n2)
      df1<-n1-1
      df2<-n2-1
    } else {
      x<-as.numeric(strsplit(input$foo1, "\\,\\s|\\,|\\s")[[1]])
      y<-as.numeric(strsplit(input$foo2, "\\,\\s|\\,|\\s")[[1]])
      samp1.mean<-mean(x)
      samp1.sd<-sd(x)
      n1<-length(x)
      df1<-n1-1
      samp2.mean<-mean(y)
      samp2.sd<-sd(y)
      n2<-length(y)
      df2<-n2-1
    }
    
    samp1.min<- samp1.mean-3*samp1.sd
    samp1.max<- samp1.mean+3*samp1.sd
    samp2.min <- samp2.mean-3*samp2.sd
    samp2.max <- samp2.mean+3*samp2.sd
    xmin<- min(samp1.min, samp2.min)
    xmax<-max(samp1.max, samp2.max)
    me1<-qt(.975,df1)*(samp1.sd/sqrt(n1))
    me2<-qt(.975,df2)*(samp2.sd/sqrt(n2))
    low1<-samp1.mean - me1
    up1<- samp1.mean +me1
    low2<-samp2.mean -me2
    up2<- samp2.mean +me2
    
    plot (function( x ) dnorm( x, mean=samp1.mean,sd=samp1.sd), 
          xmin, xmax, ylim = c(0,1), xlim = c(xmin, xmax), 
          ylab= "",axes=F, lwd=2, lty=2, col="indianred")
    par(new=TRUE)
    plot(function(x) dnorm(x, mean=samp2.mean, sd=samp2.sd), xmin, xmax, 
         xlim = c(xmin, xmax),ylim = c(0,1),ylab="", axes=F, lwd=2,lty=1, col="steelblue")
    
    lines(c(samp1.mean, samp1.mean), c(0,.7), col="indianred",lty=2)
    lines(c(samp2.mean, samp2.mean), c(0,.8), col="steelblue", lty=2)
    lines(c(low1, up1), c(.7, .7), col="indianred",lwd=2.5)
    lines(c(low2, up2), c(.8, .8), col="steelblue",lwd=2.5)
    text(low1-.23,.65, paste("(",round(low1,2),")"))
    text(up1+.23,.65, paste("(",round(up1,2),")"))
    text(low2-.23,.85, paste("(",round(low2,2),")"))
    text(up2+.23,.85, paste("(",round(up2,2),")"))
    par(xpd = TRUE)
    legend(min(c(low1, low2) -2), 1.2, c("Sample 1", "Sample 2"), lwd=c(2,2), 
           col = c("indianred", "steelblue"))
    axis(1)
    axis(2)                
    
  })  
  
  
  
  output$plot1 <-renderPlot({
    goplot()
    
  })
  
  
  gotest<-eventReactive(input$test, {
    if(input$option=="Summary Stats"){
      samp1.mean<-as.numeric(input$barx1)
      samp2.mean<- as.numeric(input$barx2)
      samp1.sd<-as.numeric(input$picksd1)
      samp2.sd<-as.numeric(input$picksd2)
      n1<-as.numeric(input$n1)
      n2<-as.numeric(input$n2)
      df1<-n1-1
      df2<-n2-1
    } else {
      x<-as.numeric(strsplit(input$foo1, "\\,\\s|\\,|\\s")[[1]])
      y<-as.numeric(strsplit(input$foo2, "\\,\\s|\\,|\\s")[[1]])
      samp1.mean<-mean(x)
      samp1.sd<-sd(x)
      n1<-length(x)
      df1<-n1-1
      samp2.mean<-mean(y)
      samp2.sd<-sd(y)
      n2<-length(y)
      df2<-n2-1
    }
    
    diff<- samp1.mean - samp2.mean
    pooled.se <- sqrt(((df1*(samp1.sd^2)+df2*(samp2.sd^2))/(df1+df2))*(1/n1+1/n2))
    test.stat <- diff/pooled.se
    
    p.val<- switch(input$alt,
                   "Population 1 > Population 2"  = pt(test.stat, df1 + df2, lower.tail = FALSE),
                   "Population 1 < Population 2" = pt(test.stat, df1 + df2),
                   "Population 1 NOT Equal to Population 2" = if (test.stat < 0) {
                     2*pt(test.stat, df1 + df2)
                   } else {
                     2*pt(test.stat, df1 + df2, lower.tail = FALSE)    
                   })
    
    
    
    
    
    hoh <- switch(input$alt,
                  "Population 1 > Population 2"  = "Population 1 \\(\\leq\\) Population 2",
                  "Population 1 < Population 2" = "Population 1 \\(\\geq\\) Population 2",
                  "Population 1 NOT Equal to Population 2" = "Population 1 = Population 2")
    
    
    str1<- withMathJax(paste("\\(H_0\\) : ", hoh))
    str2<- withMathJax(paste("\\(H_1\\) : ", input$alt))
    str3<- paste("Test Statistic = ", round(test.stat, 5), "df =", paste(df1 + df2))
    str4<- paste("p-value = ", signif(p.val))
    HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
    
  })
  
  
  goplot2<- eventReactive(input$tclick,{
    
    if(input$option=="Summary Stats"){
      samp1.mean<-as.numeric(input$barx1)
      samp2.mean<- as.numeric(input$barx2)
      samp1.sd<-as.numeric(input$picksd1)
      samp2.sd<-as.numeric(input$picksd2)
      n1<-as.numeric(input$n1)
      n2<-as.numeric(input$n2)
      df1<-n1-1
      df2<-n2-1
    } else {
      x<-as.numeric(strsplit(input$foo1, "\\,\\s|\\,|\\s")[[1]])
      y<-as.numeric(strsplit(input$foo2, "\\,\\s|\\,|\\s")[[1]])
      samp1.mean<-mean(x)
      samp1.sd<-sd(x)
      n1<-length(x)
      df1<-n1-1
      samp2.mean<-mean(y)
      samp2.sd<-sd(y)
      n2<-length(y)
      df2<-n2-1
    }
    diff<- samp1.mean - samp2.mean
    ub<-abs(diff) + 3
    lb<- -1*abs(diff) - 3
    
    test.stat<- diff/(sqrt(((df1*(samp1.sd^2)+df2*(samp2.sd^2))/(df1+df2))*(1/n1+1/n2)))
    
    p.val<- switch(input$alt,
                   "Population 1 > Population 2"  = pt(test.stat, df1 + df2, lower.tail = FALSE),
                   "Population 1 < Population 2" = pt(test.stat, df1 + df2),
                   "Population 1 NOT Equal to Population 2" = if (test.stat < 0) {
                     2*pt(test.stat, df1 + df2)
                   } else {
                     2*pt(test.stat, df1 + df2, lower.tail = FALSE)    
                   })
    plot( function(x) dt(x, (df1 + df2)), xlim = c(-1*abs(diff) - 3, abs(diff)+3), 
          ylim = c(0,1), lwd = 2,lty = 1, col = "orange", ylab = "",
          main = paste("If the null hypothesis were TRUE, \nThe Probability of a Test Stat this Extreme \nis", signif(p.val)))
    abline(v = test.stat, col = "steelblue", lty = 2, lwd = 1.5)
    if (input$alt == "Population 1 > Population 2"){
      polygon(x=c(min(test.stat, ub), seq(min(test.stat, ub), max(test.stat, ub), .01), max(test.stat, ub)),
              y=c(0, dt(seq(min(test.stat, ub), max(test.stat, ub), .01), df1+df2), 0), col = "grey44")
    } else if (input$alt == "Population 1 < Population 2") {
      polygon(x=c(min(test.stat, lb), seq(min(test.stat, lb), max(test.stat, lb), .01), max(test.stat, lb)),
              y=c(0, dt(seq(min(test.stat, lb), max(test.stat, lb), .01), df1+df2), 0), col = "grey44")
    } else {
      polygon(x=c(min(lb,-1*abs(test.stat)), seq(min(lb,-1*abs(test.stat)), max(lb,-1*abs(test.stat)), .01), max(lb,-1*abs(test.stat)),
                  min(abs(test.stat), ub), seq(min(abs(test.stat), ub), max(abs(test.stat), max(abs(test.stat), ub)), .01), ub),
              y=c(0, dt(seq(min(lb,-1*abs(test.stat)), max(lb,-1*abs(test.stat)), .01), df1+df2), 0,
                  0, dt(seq(min(abs(test.stat), ub), max(abs(test.stat), ub), .01), df1 + df2), 0), col = "grey44")
    }
    
    
  })
  
  
  output$answers<- renderUI({
    
    gotest()
    
  })
  
  output$tgraph<- renderPlot({ 
    
    goplot2() 
    
  })
  
  
  
  
  
  
})