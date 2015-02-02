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
    })
  })
  
  
  output$posterior = renderPlot({
    
    o = experiment()
    
    if (input$n_exp == 1) {
      # If there is only one experiment, just plot prior vs posterior.
      
      # Sufficient statistics
      ybar = mean(o$height)
      
      kp = input$k + input$n
      mp = (input$k*input$m+input$n*ybar)/kp
      vp = input$v + input$n
      
      ap = (input$v+input$n)/2
      bp = (input$v*input$s^2 + sum((o$height-mean(o$height))^2) + (input$k*input$n)/(input$k+input$n)*(mean(o$height)-input$m)^2)/2
      
      sp = sqrt(bp/vp)
      
      
      # Prior v posterior plot
      par(mfrow=c(1,2))
      
      curve(dsqrtinvgamma(x, ap, bp), 
            from = 1/sqrt(qgamma(.99,ap,bp)), 
            to   = 1/sqrt(qgamma(.01,ap,bp)),
            n = 1001, 
            main = "Prior vs Posterior SD",
            xlab = "Height standard deviation",
            ylab = "Density for height standard deviation",
            lwd  = 2)

      curve(dsqrtinvgamma(x, input$v/2, input$v*input$s^2/2), lwd=2, col='gray', add=TRUE)
      
      legend("topright", c("Prior","Posterior"), lwd=2, col=c("gray","black"))
      
      
      curve(ns_dt(x, vp, mp, sp/sqrt(kp)), 
            from = mp-sp/sqrt(kp)*qt(.99, vp), 
            to   = mp+sp/sqrt(kp)*qt(.99, vp),
            ylim = c(0,ns_dt(mp, vp, mp, sp/sqrt(kp))),
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
