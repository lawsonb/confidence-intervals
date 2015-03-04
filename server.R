library(shiny)

# questions
#   this is for mean and var unknown, right?
#   what's good way to title that?
#
# to do
#   CI for var (sd)
#   CI's for various things known or not (I think these won't
#        be visually that different)
#   CI for other distributions and their means, vars, parameters of interest?
#
# question, though, is what do we want people to get out of the visualization?
#

shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    cr = input$cr
    t = input$trials
    n = input$size
    m = input$mean
    sd = input$sd
    a = input$action # causes refresh of plots (a not used)
    
    # the value c.sqrtn is the same for every coverage rate cr and n
    c.sqrtn = qt( (cr + 1)/2, n-1 ) / sqrt(n)
    # this calculates interval for t trials
    intervals = sapply(1:t, function(k) {
      sample = rnorm(n, m, sd)
      sample.mean = mean(sample)
      half.length = c.sqrtn * sd(sample)
      c(sample.mean - half.length, sample.mean + half.length, 2*half.length)
    })
    lth = 1 # this is the line width (maybe change size if t really big or small?)
    cover = "blue"
    nocover = "red"
    
    # draw first trial separately to set up the plot
    # (this has to be inefficient, must be better way to do this,
    #  also, I think there must be a more efficient way to count # of CI
    #  that don't cover mean)
    if (intervals[1,1] > m | intervals[2,1] < m) {
      notcover1 = 1
      clr = nocover } else {
        clr = cover
        notcover1 = 0
      }
    par(mfrow=c(1,2))
    plot(intervals[1:2,1], c(1,1), type = "l", col = clr, lwd = lth,
         ylim = c(0,t), xlim = range( intervals[1:2,] ), ylab = "Trial",
         xlab = "",
         main = paste("CIs for coverage rate", cr) )
    mtext( paste(t, "trials of sample of size",n) )
    abline(v = m, col = "green")
    
    # this plots the remaining 2:t trials
    notcover = sapply(2:t, function(k) {
      if (intervals[1,k] > m | intervals[2,k] < m) {
        notcover = 1
        clr = nocover } else {
          clr = cover
          notcover = 0
        }
      lines(intervals[1:2,k], c(k,k), col = clr, lwd = lth)
      notcover
    })
    notcover = sum(notcover) + notcover1
    mtext( paste0(notcover, " (",round(notcover/t,4),") CI's do not include the mean of ", m), 1, 2 )
    
    # this does the second plot -- density plot of interval lengths
    plot(density(intervals[3,]), main="Interval Lengths")
    })
})
