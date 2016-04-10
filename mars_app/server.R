shinyServer(function(input, output, session){
  library(earth)
  output$ui<-renderUI({
    switch(input$option,
           "Summary Stats" = HTML(
             '<div class="form-group shiny-input-container">
             <label for="Score_MAX">Max Credit Score</label>
             <input id="Score_MAX" type="text" class="form-control" value="0"/>
             </div>
             <div class="form-group shiny-input-container">
             <label for="ProjectedMonthlyDisposableIncome2">Projected Monthly Disposable Income</label>
             <input id="ProjectedMonthlyDisposableIncome2" type="text" class="form-control" value="1"/>
             </div>
             <div class="form-group shiny-input-container">
             <label for="TotalPayment">$$n_1$$</label>
             <input id="TotalPayment" type="text" class="form-control" value="10"/>
             </div>
             <div class="form-group shiny-input-container">
             <label for="TotalMonthlyIncome2">$$\\bar{x}_2$$</label>
             <input id="TotalMonthlyIncome2" type="text" class="form-control" value="1"/>
             </div> 
             <div class="form-group shiny-input-container">
             <label for="MonthlyUnSecDI2">$$sd_2$$</label>
             <input id="MonthlyUnSecDI2" type="text" class="form-control" value="1"/>
             </div> 
             <div class="form-group shiny-input-container">
             <label for="PublicRecords">$$n_2$$</label>
             <input id="PublicRecords" type="text" class="form-control" value="10"/>
             </div>
             <div class="form-group shiny-input-container">
             <label for="PublicRecords">$$n_2$$</label>
             <input id="PublicRecords" type="text" class="form-control" value="10"/>
             </div>
             <div class="form-group shiny-input-container">
             <label for="HighCreditAmount">$$n_2$$</label>
             <input id="HighCreditAmount" type="text" class="form-control" value="10"/>
             </div>
             <div class="form-group shiny-input-container">
             <label for="Num30DaysLate24">$$n_2$$</label>
             <input id="Num30DaysLate24" type="text" class="form-control" value="10"/>
             </div>
             <div class="form-group shiny-input-container">
             <label for="CurrentMonthlyDisposableIncome2">$$n_2$$</label>
             <input id="CurrentMonthlyDisposableIncome2" type="text" class="form-control" value="10"/>
             </div>
             <div class="form-group shiny-input-container">
             <label for="LoanToPurchaseRatio">$$n_2$$</label>
             <input id="LoanToPurchaseRatio" type="text" class="form-control" value="10"/>
             </div>'  
             
           ))
        })
  
  
  
  
  gotest<-eventReactive (input$click,{ 
       test_frame<-data.frame(as.numeric(input$Score_MAX),
                              as.numeric(input$ProjectedMontlyDisposableIncome2),
                              as.numeric(input$TotalPayment),
                              as.numeric(input$TotalMonthlyIncome2),
                              as.numeric(input$MonthlyUnSecDI2),
                              as.numeric(input$PublicRecords),
                              as.numeric(input$HighCreditAmount),
                              as.numeric(input$Num30DaysLate24),
                              as.numeric(input$CurrentMonthlyDisposableIncome2),
                              as.numeric(input$LoanToPurchaseRatio))
       test_frame$CMDI2_over_TMI2<-(test_frame$CurrentMonthlyDisposableIncome2+.001)/(test_frame$TotalMonthlyIncome2+.001)
       test_frame$PMDI_over_CMDI2<-(test_frame$ProjectedMonthlyDisposableIncome2+.001)/(test_frame$CurrentMonthlyDisposableIncome2+.001)

   prob<-predict(newMarsmod, newdata = test_frame, type = "response")

    HTML(paste(prob))      
    
  })  
  


  output$answers<- renderUI({
    
    gotest()
    
  })
  
 
  
  
  
  
  
  
})