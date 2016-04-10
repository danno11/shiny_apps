shinyUI(
  fluidPage(
    titlePanel("Demonstrating CLT"),
    sidebarLayout(
      sidebarPanel(
        selectInput("dist", "Choose Distribution", 
                    choices = c("Exponential" = "rexp(", "Uniform"= "runif(", "Normal" = "rnorm(")),
                               # "Normal" = "rnorm", "Poisson" = "rpois", "Gamma" = "rgamma")),
        sliderInput("n", "Sample Size (per sample)", 
                    min = 5, max = 100, step = 5, value = 5),
        actionButton("click", "Go")
      ),
      mainPanel(
        h3("Distribution of Sample Averages From Different Types of Populations"),
        plotOutput('plot1'), 
        withMathJax(),
        p('The Central Limit Theorem: \\(\\bar{x} ~ is ~ N(\\mu, \\frac{\\sigma}{\\sqrt(n)})\\)
           states that the histogram of averages tends to look like the normal curve even when
          the histogram of individuals does not (and gets closer to the normal curve as the sample size gets larger).
          For the above demonstration,  choose a distribution and click \'Go\'.  The top plot is the true 
          distribution of the population.  For the bottom plot, we take fifty random samples from the top distribution,
          and plot the averages of the samples with a histogram.  As we can see, no matter what the shape of the true distribution is, 
          we can use the normal distribution and sample averages to find the true mean!  Our approximation gets better when we increase sample size.')
      )
    )
  )
)