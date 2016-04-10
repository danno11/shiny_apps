shinyUI(fluidPage(
  titlePanel("Binomial Plots and Calculations"),
  sidebarPanel(textInput("n", "Sample Size n",
                           "30"
                           #min=0"20"
                           #max=500,
                           #step=1,
                           #value = 20
                         ),
               sliderInput("p", "Probability of Success",
                           min=.01,
                           max=.99,
                           step=.01,
                           value=.5),
               sliderInput("xrange", "Prob of X between..",
                           min=0,
                           max=30,
                           step = 1,
                           value = c(2,30)),
               actionButton("click","plot!!"),
               actionButton("click2", "calculate"),
               withMathJax(),
               p("A general rule of thumb is to not use Normal Approximations when 
                 \\(n(p)(1-p) < 5 \\).  Try some examples of this situation out for yourself 
                 and notice the difference.  The blue sticks here represent the binomial probability; 
                 so imagine the true probability as being equal to the sums of the heights of the sticks.  
                 The normal approximations usually come very close to this, the dark grey area is the .5 area added 
                 to the normal probability to help get us closer to the true value.  ")
               ),
  mainPanel(
    plotOutput('plot'),
    withMathJax(htmlOutput("probabilities"))
    
    
    
    
  )
  
  
  
  
  
  
  
  
))
