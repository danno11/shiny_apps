shinyUI(fluidPage(
  titlePanel("Summary Stats and Visualizing Distributions"),
  sidebarPanel(tags$textarea(id="foo", rows=10, cols=38),
               actionButton("save", "Save Current State"),
               actionButton("reset", "Reset Data"),
               checkboxInput("datpoints", "Show Datapoints", value = FALSE),
               radioButtons("ptype", "Plot Type", choices = c("Histogram", "Box Plot")),
               actionButton("click1", "Generate Random Samples"),
               radioButtons("dist", "Choose Distribution", 
                           choices = c("Exponential" = "rexp(", "Uniform"= "runif(", "Normal" = "rnorm("), selected = "rnorm("),
                         uiOutput("uiput")),
  
  mainPanel(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    plotOutput('plot1', height = 500),
    plotOutput('plot2', height = 500)
    ),
  fluidRow(
    column(4),
    column(4,
           htmlOutput('test', width = "50%")),
    column(4,
           htmlOutput('savedstats', width = "50%"))
  )
    )
)
  

  
  
  
  
  
  
