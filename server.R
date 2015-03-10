library(shiny)

#
# to do
#   CI for var (sd)
#   CI for other distributions and their means, vars, parameters of interest?
#
# question, though, is what do we want people to get out of the visualization?
#

shinyServer(function(input, output) {
  
  
  both.unknown = function(cr, n) qt( (cr + 1)/2, n-1 ) / sqrt(n)
  
  mean.unknown = function(cr, n) qnorm( (cr + 1)/2 ) / sqrt(n)
  
  # var.unknown
  
  # blue if interval contains the parameter, red if it does not
  infocol = c("blue", "red")
  
  make.plots = function(type.unknown = "Mean and Variance") {
    cr = input$cr
    t = input$trials
    n = input$size
    m = input$mean
    sd = input$sd
    a = input$action # causes refresh of plots (a not used)
    
    # the value c.sqrtn is the same for every coverage rate cr and n
    if (type.unknown == "Mean and Variance") c.sqrtn = both.unknown(cr, n) 
    if (type.unknown == "Only Mean") c.sqrtn = mean.unknown(cr, n) 
    xrangelen = c(0,4*sd*c.sqrtn)
    
    # calculate interval for t trials and coverage
    interval = sapply(1:t, function(k) {
      sample = rnorm(n, m, sd)
      sample.mean = mean(sample)
      if (type.unknown == "Mean and Variance") 
        half.length = c.sqrtn * sd(sample) else {
          half.length = c.sqrtn * sd 
        }
      lower = sample.mean - half.length
      upper = sample.mean + half.length
      if (lower <= m &  m <= upper ) cover = 1 else cover = 2
      c(lower, upper, 2*half.length, cover)
    })
    
    # draw first trial separately to set up the plot
    par(mfrow=c(1,2))
    plot(interval[1:2,1], c(1,1), type = "l", col = infocol[ interval[4,1] ],
         ylim = c(0,t), xlim = range( interval[1:2,] ), 
         ylab = "Trial", xlab = "",
         main = paste( type.unknown, "Unknown" ) )
    abline(v = m, col = "green")
    mtext( paste("Coverage",cr,"  ",t, "trials of sample of size",n) )
    notcover = t - sum(interval[4,]-1)
    mtext( paste0(notcover, " (",round(notcover/t,4),") CI include the mean of ", m), 1, 2 )
    
    # this plots the remaining 2:t trials
    for (k in 2:t) lines( interval[1:2, k], c(k, k), 
                          col = infocol[ interval[4,k] ] )
    
    # this does the second plot -- density plot of interval lengths
    if (type.unknown == "Only Mean") {
      plot(0,0, main="Interval Lengths", type = "n",
           xlim = xrangelen, ylim = c(0,1),
           xlab = "interval length")
      abline( v = interval[3,1] )
      mtext(paste("All intervals length",
                  round(mean(interval[3,]),3)))  
    }
     else {
      plot(density(interval[3,]), main="Interval Lengths",
           xlim = xrangelen, xlab = "interval length")
      mtext(paste0("Interval mean=",round(mean(interval[3,]),3),
                   " sd=",round(sd(interval[3,]),3)))
    }
  }
  
  output$unknown.both.Plot <- renderPlot( make.plots("Mean and Variance") )
  output$unknown.mean.Plot <- renderPlot( make.plots("Only Mean") )
  
})
