library(shiny)

ns_dt = function(x,v,m,s) dt((x-m)/s,v)/s
dinvgamma = function(x, a, b) dgamma(1/x, a, b)/x^2
dsqrtinvgamma = function(x, a, b) dinvgamma(x^2,a,b)*2*x


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

    
  
  output$posterior = renderPlot({
    if (input$n_exp == 1) {
      
    } else {
      
    }
  })
  
})
