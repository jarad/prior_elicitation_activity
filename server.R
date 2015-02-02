library(shiny)
library(plyr)

ns_dt = function(x,v,m,s) dt((x-m)/s,v)/s
dinvgamma = function(x, a, b) dgamma(1/x, a, b)/x^2
dsqrtinvgamma = function(x, a, b) dinvgamma(x^2,a,b)*2*x



d = read.csv("heights.csv")

shinyServer(function(input,output) {
  
  
  
  output$prior = renderPlot({
    
    par(mfrow=c(1,2))
    
    curve(dsqrtinvgamma(x, input$v/2, input$v*input$s^2/2), 
          from = 0, 
          to   = input$s*4,
          main = "Prior on height standard deviation",
          xlab = "Height standard deviation",
          ylab = "Density for height standard deviation",
          lwd  = 2)
    
    curve(ns_dt(x, input$v, input$m, input$s/sqrt(input$k)), 
          from = input$m-input$s/sqrt(input$k)*qt(.99, input$v), 
          to   = input$m+input$s/sqrt(input$k)*qt(.99, input$v),
          ylim = c(0,ns_dt(input$m, input$v, input$m, input$s/sqrt(input$k))),
          main = "Prior on mean height",
          xlab = "Mean height", 
          ylab = "Density for mean height",
          lwd  = 2)
    
  })
  
  output$prior_ci = renderTable({
    quantile = c(.01,.05,.25,.5,.75,.95,.99)
    df = data.frame(quantile = quantile,
                    mean_height = qt(quantile, input$v)*input$s/sqrt(input$k)+input$m,
                    sd_heights  = 1/sqrt(qgamma(sort(quantile,decreasing=TRUE), input$v/2, input$v*input$s^2/2)))
    t(df)
  })

    
  
  # Randomly select data
  experiment = reactive({
    rdply(input$n_exp, {
      rows = sample(nrow(d), input$n, replace=TRUE)
      d[rows,]
    }, .id = "experiment")
  })
  
  
  ss = reactive({
    k  = input$k
    m  = input$m
    v  = input$v
    s2 = input$s^2
    
    ddply(experiment(), .(experiment), summarize,
          ybar = mean(height),
          n    = length(height),
          kp   = k + n,
          mp   = (k*m + n*ybar)/kp,
          vp   = v + n,
          ap   = vp/2,
          bp   = (v*s2 + sum((height-ybar)^2) + (k*n)/kp * (ybar-m)^2)/2,
          sp   = sqrt(2*bp/vp))
  })
  
  output$posterior = renderPlot({
    
    o = ss()
    
    if (input$n_exp == 1) {
      # If there is only one experiment, just plot prior vs posterior.

      # Prior v posterior plot
      par(mfrow=c(1,2))
      
      curve(dsqrtinvgamma(x, o$ap, o$bp), 
            from = 1/sqrt(qgamma(.99,o$ap,o$bp)), 
            to   = 1/sqrt(qgamma(.01,o$ap,o$bp)),
            n    = 1001, 
            main = "Prior vs Posterior SD",
            xlab = "Height standard deviation",
            ylab = "Density for height standard deviation",
            lwd  = 2)

      curve(dsqrtinvgamma(x, input$v/2, input$v*input$s^2/2), lwd=2, col='gray', add=TRUE)
      
      legend("topright", c("Prior","Posterior"), lwd=2, col=c("gray","black"))
      
      
      curve(ns_dt(x, o$vp, o$mp, o$sp/sqrt(o$kp)), 
            from = o$mp-o$sp/sqrt(o$kp)*qt(.99, o$vp), 
            to   = o$mp+o$sp/sqrt(o$kp)*qt(.99, o$vp),
            ylim = c(0,ns_dt(o$mp, o$vp, o$mp, o$sp/sqrt(o$kp))),
            main = "Prior vs Posterior Mean",
            xlab = "Mean height", 
            ylab = "Density for mean height",
            lwd  = 2)
      
      
      curve(ns_dt(x, input$v, input$m, input$s/sqrt(input$k)), 
            add = TRUE,
            lwd = 2,
            col="gray")
      
    } else {
      # If there are more than one experiment, plot credible intervals using both default and informative prior.
      
    }
  })
  
})
