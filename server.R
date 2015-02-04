library(shiny)
library(plyr)
library(ggplot2)

ns_dt = function(x,v,m,s) dt((x-m)/s,v)/s
dinvgamma = function(x, a, b) dgamma(1/x, a, b)/x^2
dsqrtinvgamma = function(x, a, b) dinvgamma(x^2,a,b)*2*x



height_df = read.csv("heights.csv")
grand_mean = mean(height_df$height)
grand_sd   = sd(height_df$height)

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
  data = reactive({
    rdply(input$n_exp, {
      rows = sample(nrow(height_df), input$n, replace=TRUE)
      height_df[rows,]
    }, .id = "experiment")
  })
  
  
  output$data = renderDataTable({
    data()[,1:2]
  })
  
  
  sufficient_statistics = reactive({
    d = ddply(data(), .(experiment), summarize, 
              n              = length(height),
              mean_height    = mean(height),
              sum_of_squares  = sum((height-mean_height)^2))
    mutate(d, 
           kp = input$k + n,
           mp = (input$k*input$m + n*mean_height)/kp,
           vp = input$v + n,
           ap = vp/2,
           bp = input$v*input$s^2 + sum_of_squares + (input$k*n)/kp*(mean_height-input$m)^2/2,
           sp = sqrt(2*bp/vp))
  })
  
  output$ss = renderDataTable({
    sufficient_statistics()
  })

  
  credible_intervals = reactive({
    o = sufficient_statistics()
    
    alpha = input$alpha
    
    # limits for informative prior
    informative_hw     = o$sp/sqrt(o$kp) * qt(1-alpha/2, o$vp) # half-width
    informative_limits = cbind(o$mp - informative_hw, o$mp + informative_hw)
    informative_df     = data.frame(prior = "informative", lcl = informative_limits[,1], ucl = informative_limits[,2])
    informative_df$experiment = 1:nrow(informative_df)
    
    if (input$n == 1) { # default prior needs n>1
      informative_df
    } else {
      # limits for default prior 
      default_hw     = sqrt(o$sum_of_squares/o$n) * qt(1-alpha/2, o$n-1)
      default_limits = cbind(o$mean_height - default_hw, o$mean_height + default_hw)
      default_df     = data.frame(prior = "default", lcl = default_limits[,1], ucl = default_limits[,2])
      default_df$experiment = 1:nrow(default_df)
      
      rbind(informative_df, default_df)
    }
  })
  
  

  output$posterior = renderPlot({
    
    if (input$n_exp == 1) {
      # If there is only one experiment, just plot prior vs posterior.
      o = sufficient_statistics()
      
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
      
      if (input$include_truth) abline(v=grand_sd)
      
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
      
      if (input$include_truth) abline(v=grand_mean)
      
    } else {
      # If there is more than one experiment, plot credible intervals using both default and informative prior.
      g = ggplot(credible_intervals(), aes(x=lcl, y=experiment, xend=ucl, yend=experiment, col=prior)) + 
        geom_segment(size=I(2), alpha=0.5) +
        labs(title="Credible intervals", x="Mean height", y="Experiment")
      if (input$include_truth) g = g + geom_vline(xintercept = grand_mean)
      print(g)
    }
  })
  
  
  output$coverage = renderTable({
    if (!input$include_truth) return(NULL)
    d = credible_intervals()
    d$cover = d$lcl < grand_mean & grand_mean < d$ucl
    ddply(d, .(prior), summarize, coverage = mean(cover))
  })
  
  
})
