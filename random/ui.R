shinyUI(
  fluidPage(
    h4("Interpreting Confidence Intervals"),
    h6("t-based C.I.'s built 
                  from Random Samples of 10 from normal (0,1) 
                  distribution"),
    fluidRow(
      column(3,
             actionButton("click1","Plot 90% C.I.'s")),
      column(3,
             actionButton("click2","Plot 95% C.I.'s")),
      column(3,
             actionButton("click3","Plot 99% C.I.'s")),
      column(3,
             textInput("q","Input Confidence Level (as decimal)"),
             actionButton("click4","Plot C.I.'s",NULL))
    ),
    fluidRow(
      column(3,
             plotOutput('plot1')),
      column(3,
             plotOutput('plot2')),
      column(3,
             plotOutput('plot3')),
      column(3,
             plotOutput('plot4'))
    ),
    fluidRow(
      withMathJax(),
      p("The way this app works is that we draw random 
               samples of 10 from a normal (0,1) distribution, then build 
               a confidence interval for the true mean from this sample 
               (using the t-distribution and pretending we don't know what the true mean is).  
               In this case, we know the true mean is 0!  How often do the confidence intervals
               contain the true value of mu?  A 90% C.I. will contain mu about 90% of the time, 
               A 95% C.I. will contain mu about 95% of the time, etc.  What else do you notice or 
               observe about the difference between the C.I.'s?  Try inputting a confidence level
               of your own (as a decimal) in the last column to see what a __% confidence interval might look like.")
    )
  )
)