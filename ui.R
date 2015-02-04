library(shiny)

shinyUI(fluidPage(

  titlePanel("Informative Bayesian analysis of mean student height"),
  
  sidebarLayout(
  
    sidebarPanel(
      #radioButtons("units", "Units", c(Inches="inches", Centimeters="cms")),
      #hr(),
      helpText("Prior:"),
      numericInput("m", "Mean height", 1),
      numericInput("k", "Mean height sample size", 1),
      numericInput("s", "Standard deviation of heights", 1),
      numericInput("v", "SD sample size", 1),
      hr(),
      helpText("Data:"),
      numericInput("n_exp", "Number of experiments", 1),
      numericInput("n", "Number of observations per experiment", 2),
      hr(),
      helpText("Inference:"),
      numericInput("alpha", "CI error rate (alpha):", .05, 0, 1),
      checkboxInput("include_truth", "Include truth?", value=FALSE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Prior",     plotOutput("prior"), br(), helpText("Prior quantiles:"), tableOutput("prior_ci") ),
        tabPanel("Posterior", plotOutput("posterior"), br(), helpText("Coverage:"), tableOutput("coverage")),
        tabPanel("Data",      dataTableOutput("data")),
        tabPanel("Sufficient statistics",      dataTableOutput("ss"))
      )
    )
  )
))
