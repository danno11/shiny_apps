shinyUI(fluidPage(
  titlePanel("Two Independent Samples"),
  sidebarLayout(
    sidebarPanel(
      withMathJax(),
        htmlOutput("ui")
      
    ),
    
    mainPanel(
         actionButton("click", "Run Test"),
         htmlOutput("answers")
                           
                  ))
    )
  )

