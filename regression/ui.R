shinyUI(fluidPage(
  fluidRow(
    column(width = 6,
           plotOutput("plot1", height = 350,
                      click = "plot1_click",
                      hover = "point_hover",
                      brush = brushOpts(
                        id = "plot1_brush"
                      )
           ),
           actionButton("exclude_toggle", "Toggle points"),
           actionButton("exclude_reset", "Reset")
    ),
    column(width = 6,
           selectInput('xcol', 'X Variable', c("Miles/Gallon"="mpg", "Num of Cylinders"="cyl", "Displacement(cu.in.)"="disp",
                                               "Gross Horsepower"="hp", "Rear Axle Ratio"="drat", "Weight (lb/1000)"="wt", 
                                               "1/4 Mile Time"="qsec", "V/S"="vs", "Transmission"="am", "Num Forward Gears"="gear", 
                                               "Num Carburetors"="carb"),
                       selected = names(mtcars[[1]])),
           selectInput('ycol', 'Y Variable', c("Miles/Gallon"="mpg", "Num of Cylinders"="cyl", "Displacement(cu.in.)"="disp",
                                               "Gross Horsepower"="hp", "Rear Axle Ratio"="drat", "Weight (lb/1000)"="wt", 
                                               "1/4 Mile Time"="qsec", "V/S"="vs", "Transmission"="am", "Num Forward Gears"="gear", 
                                               "Num Carburetors"="carb"),
                       selected=names(mtcars)[[6]])
    ),
    column(width = 6,
           verbatimTextOutput("point_hoverinfo")
    ),
    fluidRow(
      
    ),
    fluidRow(
      column(width = 6,
             h3("Description"),
             p("A dataset of 32 cars.  Highlight points to remove 
               and click Toggle to explore how outliers effect 
               parameter estimates, p-values, and R-square.  Hover 
               mouse over specific points to explore the car's full info.")),
      column(width = 6,
           verbatimTextOutput("lm_info"))
   )
  ) 
 )
)

