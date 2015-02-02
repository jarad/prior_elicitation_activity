library(shiny)

shinyUI(fluidPage(

  titlePanel("Informative Bayesian analysis of mean student height"),
  
  sidebarLayout(
  
    sidebarPanel(
      #radioButtons("units", "Units", c(Inches="inches", Centimeters="cms")),
      #hr(),
      helpText("Prior:"),
      numericInput("m", "Mean height", 68.5),
      numericInput("k", "Mean height sample size", 1),
      numericInput("s", "Standard deviation of heights", 6),
      numericInput("v", "SD sample size", 1),
      hr(),
      helpText("Data:"),
      numericInput("n", "Number of observations", 1),
      numericInput("n_exp", "Number of experiments", 1)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Prior", plotOutput("prior"), br(), helpText("Prior quantiles:"), tableOutput("prior_ci") ),
        tabPanel("Posterior", plotOutput("posterior"))
      )
    )
  )
))
