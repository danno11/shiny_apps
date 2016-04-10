shinyUI(fluidPage(
  titlePanel("Two Independent Samples"),
    sidebarLayout(
      sidebarPanel(
        withMathJax(),
        radioButtons("option", "Input Method", choices = c("Summary Stats", "Input Data")),
        htmlOutput("ui")
        
      ),
    
      mainPanel(
        tabsetPanel(type = "tabs",
        tabPanel("Plot and C.I.'s", 
                 actionButton("click", "Plot!"), plotOutput('plot1')),
        tabPanel("Hypothesis Testing",
                 withMathJax(),
          selectInput("alt", "State Alternative Hypothesis", 
                      c("Population 1 > Population 2" ,
                        "Population 1 < Population 2" ,
                        "Population 1 NOT Equal to Population 2")),
          actionButton("test", "Run Test"),
          actionButton("tclick", "T-Distribution Graphic "),
          htmlOutput("answers"),
          plotOutput("tgraph")
          
          
          
          
        
        
           ))
      )
    )
   )
)
