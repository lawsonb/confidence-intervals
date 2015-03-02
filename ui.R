library(shiny)

# first attempt at confidence interval app
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Confidence Intervals for a Normal Distribution"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("cr",
                  "Coverage rate:",min = 0.500,max = 0.999,value = 0.95),
      sliderInput("trials",
                  "Number of samples:",min = 20,max = 200,value = 100),
      sliderInput("size",
                  "Size of sample:",min = 10,max = 1000,value = 50),
      sliderInput("mean",
                  "Mean:",min = -10,max = 10,value = 0),
      sliderInput("sd",
                  "Standard Deviation:",min = 0.1,max = 10.0,value = 2),
      actionButton("action", label="Refresh")
    ),
    # Show a plot of the trials
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
