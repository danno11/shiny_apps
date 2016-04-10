shinyUI(fluidPage(
  titlePanel("One Sample CI and Hypothesis Testing"),
  sidebarLayout(
    sidebarPanel(
      withMathJax(),
      radioButtons("option", "Input Method", choices = c("Summary Stats", "Input Data")),
      uiOutput("ui")
      
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
          tabPanel("Plot and C.I.",
              actionButton("click","go"), plotOutput('plot1')),
          tabPanel("Hypothesis Test",
           p("State Alternative Hypothesis"),
           #div(style="display:inline-block",p("$$mu$$")),
           withMathJax(),
           div(style="display:inline-block", selectInput("alt", "", choices = c("mu > ", "mu <", "mu NOT equal to"))),       
           div(style="display:inline-block", tags$label("", `for` = "ho"), 
               tags$input(id = "ho", type = "text", value = "0", class="input-small")),#textInput("ho", "$$H_0$$", value = 0)),
           br(),        
              actionButton("test", "Run Test"),
              actionButton("tclick", "T-Distribution Graphic "),
              htmlOutput("answers"),
              plotOutput("tgraph")))
      
      
      
      
      
      
    )
  )
))