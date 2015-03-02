library(shiny)

shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    cr = input$cr
    t = input$trials
    n = input$size
    m = input$mean
    sd = input$sd
    a = input$action # causes refresh of plots (a not used)
    c.sqrtn = qt( (cr + 1)/2, n-1 ) / sqrt(n)
    intervals = sapply(1:t, function(k) {
      sample = rnorm(n, m, sd)
      sample.mean = mean(sample)
      sigma.prime = sd(sample) 
      half.length = c.sqrtn * sigma.prime
      c(sample.mean - half.length, sample.mean + half.length, 2*half.length)
    })
    lth = 1
    cover = "blue"
    nocover = "red"
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
    plot(density(intervals[3,]), main="Interval Lengths")
    })
})
