#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  tags$head(tags$style(HTML("
    .shiny-text-output {
      background-color:#fff;
    }
  "))),
  
  h1("Simple Monte Carlo by Douglas Hubbard", 
     style = "font-family: 'Source Sans Pro';
        color: #000; text-align: center;
        padding: 20px"),
  br(),
  
  fluidRow(
    column(12,
           p("Douglas Hubbard's very simple but incredibly useful Monte Carlo needs three parameters: 
the maximum, the minimum, and the average of the observations you currently have.  
This tool will also calculate risk based on you risk boundary (the point at an outcome can be determined a loss).  
             The simulation results will update as you submit new parameters.", 
             style = "font-family: 'Source Sans Pro';")
    )
  ),
  
  br(),
  
  fluidRow(
    column(3, wellPanel(
      numericInput("maximum", "Maximum:", value = 2),
      numericInput("average", "Average:", value = 1),
      numericInput("minimum", "Minimum:", value = 0),
      numericInput("confidence", "Confidence Level:", value = 90),
      numericInput("trials", "Trials:", value = 10000),
      numericInput("riskBoundary", "Risk Boundary:", value = 0),
      submitButton("Submit")
    )),
    column(9,
           plotOutput("monteCarloPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Draw histogram for rendering
  output$monteCarloPlot <- renderPlot({
    # Run Monte Carlo
    MCTable <- as.data.frame(matrix(0, ncol = 1, nrow = input$trials))
    names(MCTable) <- c("results")
    i <- 1
    zscore <- qnorm((1 - (input$confidence/100))/2, lower.tail = FALSE) * 2
    while (i <= input$trials) {
      MCTable[i,1] <- qnorm(runif(1), input$average, (input$maximum - input$minimum)/zscore)
      i <- i + 1 
    }
    # Draw histogram
    MCHistogram <- ggplot(data = MCTable, 
           aes(MCTable$results)) + 
      geom_histogram(col = "black",
                     fill = "forestgreen",
                     alpha = .8) +
      labs(title = "Monte Carlo Results",
           x = "Simulated Result",
           y = "Count")
    MCTable$belowRisk <- ifelse(MCTable$results < input$riskBoundary, 1, 0)
    riskProbability <- as.double(mean(MCTable$belowRisk)) * 100
    riskGraph <- MCHistogram +
      geom_vline(aes(xintercept = input$riskBoundary),
                 color = "red", 
                 linetype = "dashed", 
                 size = 1) +
      labs(subtitle = paste("There is a", riskProbability, "percent chance the real world result will be below your risk boundary."))
    riskGraph
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

