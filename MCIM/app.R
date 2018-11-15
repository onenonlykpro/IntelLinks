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
library(rhandsontable)

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
  br(),
  
  fluidRow(
    column(4,
           actionButton("add_btn_screen", "Add Screening Criteria"),
           actionButton("rm_btn_screen", "Remove Screening Criteria"),
           textOutput("counter")),
    column(4, 
           actionButton("add_btn_eval", "Add Evaluation Criteria"),
           actionButton("rm_btn_eval", "Remove Evaluation Criteria"),
           textOutput("counter")),
    column(4,
           actionButton("add_btn_dec", "Add Decision"),
           actionButton("rm_btn_dec", "Remove Decision"),
           textOutput("counter"))),
  br(),
  
  mainPanel(uiOutput("ScreeningCriteria_ui"))
)

# Define server logic required to create table
server <- function(input, output, session) {
  # Track the number of input boxes to render
  counter <- reactiveValues(n = 0)
  
  #Track the number of input boxes previously
  prevcount <-reactiveValues(n = 0)
  observeEvent(input$add_btn_screen, {
    counter$n <- counter$n + 1
    prevcount$n <- counter$n - 1})
  observeEvent(input$rm_btn_screen, {
    if (counter$n > 0) {
      counter$n <- counter$n - 1 
      prevcount$n <- counter$n + 1
    }
    
  })
  observeEvent(input$add_btn_eval, {
    counter$n <- counter$n + 1
    prevcount$n <- counter$n - 1})
  observeEvent(input$rm_btn_eval, {
    if (counter$n > 0) {
      counter$n <- counter$n - 1 
      prevcount$n <- counter$n + 1
    }
    
  })
  observeEvent(input$add_btn_dec, {
    counter$n <- counter$n + 1
    prevcount$n <- counter$n - 1})
  observeEvent(input$rm_btn_dec, {
    if (counter$n > 0) {
      counter$n <- counter$n - 1 
      prevcount$n <- counter$n + 1
    }
    
  })
  output$counter <- renderPrint(print(counter$n))
  
  ScreeningCriterias <- reactive({
    
    n <- counter$n
    
    if (n > 0) {
      # If the no. of ScreeningCriterias previously where more than zero, then 
      #save the text inputs in those text boxes 
      if(prevcount$n > 0) {
        
        vals = c()
        if(prevcount$n > n) {
          lesscnt <- n
          isInc <- FALSE
        } else {
          lesscnt <- prevcount$n
          isInc <- TRUE
        }
        for(i in 1:lesscnt) {
          inpid = paste0("textin", i)
          vals[i] = input[[inpid]] 
        }
        if(isInc) {
          vals <- c(vals, "New text box")
        }
        
        lapply(seq_len(n), function(i) {
          textInput(inputId = paste0("textin", i),
                    label = paste0("Screening Criteria ", i), value = vals[i])
        })
        
      } else {
        lapply(seq_len(n), function(i) {
          textInput(inputId = paste0("textin", i),
                    label = paste0("Screening Criteria ", i), value = "New text box")
        }) 
      }
      
    }
    
  })
  
  output$ScreeningCriteria_ui <- renderUI({ ScreeningCriterias() })
}

# Run the application 
shinyApp(ui = ui, server = server)

