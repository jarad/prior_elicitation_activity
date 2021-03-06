library(shiny)

cm_per_inch = 2.54

shinyUI(fluidPage(

  titlePanel("Informative Bayesian analysis of mean student height"),
  
  sidebarLayout(
  
    sidebarPanel(
      helpText("Prior:"),
      numericInput("m", "Mean height (m)", 0*cm_per_inch),
      numericInput("k", "Mean height sample size (k)", 1),
      numericInput("s", "Standard deviation of heights (s)", 1*cm_per_inch),
      numericInput("v", "SD sample size (v)", 1),
      hr(),
      helpText("Data:"),
      numericInput("n_exp", "Number of experiments", 1),
      numericInput("n", "Number of observations per experiment", 2),
      hr(),
      helpText("Inference:"),
      numericInput("alpha", "CI error rate (alpha):", .05, 0, 1),
      checkboxInput("include_truth", "Include truth?", value=FALSE),
      hr(),
      helpText("Other options:"),
      radioButtons("units", "Units", c(Inches="inches", Centimeters="cms")),
      textInput("qts", "Prior quantiles", ".01,.05,.25,.5,.75,.95,.99"),
      numericInput("seed", "Experiment seed (for reproducibility):", 1)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Prior",     plotOutput("prior"), br(), helpText("Prior quantiles:"), tableOutput("prior_ci") ),
        tabPanel("Posterior", plotOutput("posterior"), br(), helpText("Coverage:"), tableOutput("coverage")),
        tabPanel("Experiments",      dataTableOutput("data")),
        tabPanel("Experiment summaries",      dataTableOutput("ss"))
      )
    )
  )
))
