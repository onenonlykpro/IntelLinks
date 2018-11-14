#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Application title
  tags$head(tags$style(HTML("
    .shiny-text-output {
      background-color:#fff;
    }"))),
  
  h1("Multi Criteria Intelligence Matrix by Lindsey Jakubchak", 
     style = "font-family: 'Source Sans Pro';
        color: #000; text-align: center;
        padding: 20px"),
  br(),
  
  fluidRow(
    column(12,
           p("The Multi-Criteria Intelligence Matrix (MCIM) is an analytic method that helps to assess possible decisions from a 
competitor or an external party.  The MCIM includes three components: courses of action (COA), screening criteria, and evaluation criteria.  
Based on the Multi-Criteria Decision Making (MCDM), a process which breaks down a problem into subcomponents, the MCIM forces you 
to evaluate each subcomponent individually, and reassembles the subcomponents in order to make a decision.  The MCIM helps to 
produce an estimate by generating final scores of each COA, which are based on the evaluation criteria and represented in the matrix.",
             style = "font-family: 'Source Sans Pro';"),
           p("Read Lindsey Jakubchak's thesis on MCIM at https://www.scribd.com/document/18026945/The-Effectiveness-of-Multi-Criteria-Intelligence-Matrices-In-Intelligence-Analysis.",
             style = "font-family: 'Source Sans Pro';"),
           p("Read more about the strengths and weaknesses of an MCIM at http://advat.blogspot.com/2015/10/multi-criteria-intelligence-matrices.html.",
             style = "font-family: 'Source Sans Pro';"))),
  br()
   
  )

# Define server logic required to create table
server <- function(input, output) {
   
  output$RiskScorePrintout <- renderDataTable({
    # input$userfile will be NULL initially. After the user selects and uploads a file, head of that data file by default, or all rows if selected, will be shown.
    req(input$userfile)
    
    df <- read.csv(input$userfile$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

