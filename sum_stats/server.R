shinyServer(function(input, output, session){
 
  output$uiput<-renderUI({
    switch(input$dist,
           "rexp(" = 
HTML('<div class="form-group shiny-input-container">
               <label for="rate">&#955 (&#956 = 1/&#955)</label>
           <input id="rate" type="text" class="form-control" value="1"/>
           </div>
           <div class="form-group shiny-input-container">
           <label for="n1">n</label>
           <input id="n1" type="text" class="form-control" value="10"/>
           </div>'),
             "runif(" = 
HTML('<div class="form-group shiny-input-container">
               <label for="min">Min</label>
           <input id="min" type="text" class="form-control" value="0"/>
           </div>
           <div class="form-group shiny-input-container">
           <label for="max">Max</label>
           <input id="max" type="text" class="form-control" value="1"/>
           </div>
           <div class="form-group shiny-input-container">
           <label for="n1">n</label>
           <input id="n1" type="text" class="form-control" value="10"/>
           </div>'),
             "rnorm(" = 
  HTML('<div class="form-group shiny-input-container">
               <label for="barx1">&#956</label>
           <input id="barx1" type="text" class="form-control" value="0"/>
           </div>
           <div class="form-group shiny-input-container">
           <label for="picksd1">&#963</label>
           <input id="picksd1" type="text" class="form-control" value="1"/>
           </div>
           <div class="form-group shiny-input-container">
           <label for="n1">n</label>
           <input id="n1" type="text" class="form-control" value="10"/>
           </div>'))
  })
  
  ob<-reactiveValues( r = NULL)
  dat<-reactiveValues(A = NULL)
  
  
  
    observeEvent(input$click1,{
      char_dist<-switch(input$dist,
                        "rexp("=as.numeric(input$rate),
                        "runif("= paste(as.numeric(input$min),",", as.numeric(input$max)),
                        "rnorm(" = paste(as.numeric(input$barx1),",", as.numeric(input$picksd1)))
      
      ranvals<-eval(parse(text=paste(input$dist, input$n1, ",", char_dist, ")")))
      isolate({
               ob$r<-c(ob$r, ranvals)
              })
  })
   

  observeEvent(input$save,{
    slz<-strsplit(input$foo, "\\,\\s|\\,|\\s")[[1]]
    isolate({
      dat$A<-c(ob$r, slz)
    })
  })

  observeEvent(input$reset,{
    ob$r<-NULL
  })
  
    
  output$plot1<-renderPlot({
    
    
    plz<-strsplit(input$foo, "\\,\\s|\\,|\\s")[[1]]
    plz<-as.numeric(plz)
    plz<-c(ob$r, plz) 
    dat$A<-as.numeric(dat$A)
     
    v1<-fivenum(plz)
    #plot(0, 0, 
     #    xlim = c(min(c(dat$A,plz)), max(c(dat$A,plz))), 
    #     ylim = c(.8, 1.5), axes = F)
    #title(main = "Data 1 (Current)", line = 2.5)
    if(input$ptype=="Box Plot"){
    boxplot(plz, horizontal = TRUE, main = "")
    } else{
      hist(plz,
          xlim = c(min(plz), max(plz)), main = "")
      abline(v = median(plz), col = "steelblue", lwd = 2)
  legend("topright", c("Mean", "Median"), col = c("steelblue", "indianred"), pch = c(19, 19))
    }
    abline(v = mean(plz), col = c("indianred"), lwd = 2)
    if(input$datpoints=="TRUE"){
    par(new = TRUE)
    stripchart(plz, method = "jitter", col = "#29635F", pch = 19, xlim = c(min(plz), max(plz)), axes = F)
    }
    #points(plz, rep(1.4, length(plz)), pch = 19)
    #points(fivenum(plz), rep(1.4, 5), pch = 19, col = "limegreen")
    axis(3, at = v1, labels = round(v1, 2))
    })
  
  output$test<-renderUI({
    plz<-strsplit(input$foo, "\\,\\s|\\,|\\s")[[1]]
    plz<-as.numeric(plz)
    plz<-c(ob$r, plz)
    
    str1<-paste("Data 1 (Current Data)")
    str2<-paste("n = ", length(plz))
    str3<-paste("Min = ", round(min(plz), 3))
    str4<-paste("Lower Quartile = ", round(fivenum(plz)[2],3))
    str5<-paste("Median = ", round(median(plz),3))
    str6<-paste("Mean = ", round(mean(plz),3))
    str7<-paste("sd = ", round(sd(plz),3))
    str8<-paste("Upper Quartile = ", round(fivenum(plz)[4],3))
    str9<-paste("Max = ", round(max(plz),3))
    
    HTML(paste(str1, str2, str3, str4, str5, str6,
               str7, str8, str9, sep = '<br/>'))
  })
  
  
  
  saverplot<-eventReactive(input$save,{
    slz<-strsplit(input$foo, "\\,\\s|\\,|\\s")[[1]]
    slz<-as.numeric(slz)
    slz<-c(ob$r, slz)
    v1<-fivenum(slz)
    
    if(input$ptype=="Box Plot"){
      #plot(median(slz), type = "n", xlim = c(min(slz), max(slz)), type = F)
      boxplot(slz, horizontal = TRUE, main = "")
    } else{
      hist(slz,
           xlim = c(min(slz), max(slz)), main = "")
      abline(v = median(slz), col = "steelblue", lwd = 2)
      legend("topright", c("Mean", "Median"), col = c("steelblue", "indianred"), pch = c(19, 19))
    }
    abline(v = mean(slz), col = c("indianred"), lwd = 2)
    par(new = TRUE)
    if(input$datpoints=="TRUE"){
    stripchart(slz, method = "jitter", col = "#29635F", pch = 19, xlim = c(min(slz), max(slz)), axes = F)
    }
    #points(plz, rep(1.4, length(plz)), pch = 19)
    #points(fivenum(plz), rep(1.4, 5), pch = 19, col = "limegreen")
    axis(3, at = v1, labels = round(v1, 2))
  })
  
 
  
  output$plot2<-renderPlot({
    saverplot()
  })
 output$savedstats<-renderUI({
   newdat<-as.numeric(dat$A)
   str1<-paste("Data 2 (Saved Data)")
   str2<-paste("n = ", round(length(newdat),3))
   str3<-paste("Min = ", round(min(newdat),3))
   str4<-paste("Lower Quartile = ", round(fivenum(newdat)[2],3))
   str5<-paste("Median = ", round(median(newdat),3))
   str6<-paste("Mean = ", round(mean(newdat),3))
   str7<-paste("sd = ", round(sd(newdat),3))
   str8<-paste("Upper Quartile = ", round(fivenum(newdat)[4],3))
   str9<-paste("Max = ", round(max(newdat),3))
   
   HTML(paste(str1, str2, str3, str4, str5, str6,
              str7, str8, str9, sep = '<br/>'))
 }) 
  
})