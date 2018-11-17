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
    column(4, wellPanel(
      textInput("decision1", "Decision A", value = "A: ", width = NULL, placeholder = NULL),
      textInput("decision2", "Decision B", value = "B: ", width = NULL, placeholder = NULL),
      textInput("decision3", "Decision C", value = "C: ", width = NULL, placeholder = NULL),
      textInput("decision4", "Decision D", value = "D: ", width = NULL, placeholder = NULL),
      textInput("decision5", "Decision E", value = "E: ", width = NULL, placeholder = NULL))),
    column(4, wellPanel(
      textInput("screening1", "Screening Criteria #1", value = "SC1: ", width = NULL, placeholder = NULL),
      textInput("screening2", "Screening Criteria #2", value = "SC2: ", width = NULL, placeholder = NULL),
      textInput("screening3", "Screening Criteria #3", value = "SC3: ", width = NULL, placeholder = NULL),
      textInput("screening4", "Screening Criteria #4", value = "SC4: ", width = NULL, placeholder = NULL),
      textInput("screening5", "Screening Criteria #5", value = "SC5: ", width = NULL, placeholder = NULL))),
    column(4, wellPanel(
      textInput("evaluation1", "Evaluation Criteria #1", value = "EC1: ", width = NULL, placeholder = NULL),
      textInput("evaluation2", "Evaluation Criteria #2", value = "EC2: ", width = NULL, placeholder = NULL),
      textInput("evaluation3", "Evaluation Criteria #3", value = "EC3: ", width = NULL, placeholder = NULL),
      textInput("evaluation4", "Evaluation Criteria #4", value = "EC4: ", width = NULL, placeholder = NULL),
      textInput("evaluation5", "Evaluation Criteria #5", value = "EC5: ", width = NULL, placeholder = NULL)))),
  br(),
  
  fluidRow(
    column(12, DTOutput("MCIMTable"))
  )
)

# Define server logic required to create table
server <- function(input, output, session) {
  # Create interactive table for using decisions as row names
  # dfrows <- c(input$decision1, input$decision2, input$decision3, input$decision4, input$decision5)
  # dfcolumns <- c(input$screening1, input$screening2, input$screening3, input$screening4, input$screening5, input$evaluation1, input$evaluation2, input$evaluation3, input$evaluation4, input$evaluation5)
  df <- as.data.frame(matrix(0, ncol = 10, nrow = 5))
  # row.names(df) <- dfrows
  # colnames(df)   <- dfcolumns
  
  # Create interactive table for using screening criteria and evaluation criteria as column names
  output$MCIMTable <- renderDataTable({datatable(df, selection = "none", editable = TRUE, rownames = list(
    list(title = paste0("A: ", input$decision1)),
    list(title = paste0("B: ", input$decision2)),
    list(title = paste0("C: ", input$decision3)),
    list(title = paste0("D: ", input$decision4)),
    list(title = paste0("E: ", input$decision5))),
    colnames = list(
      list(title = paste0("SC1: ", input$screening1)),
      list(title = paste0("SC2: ", input$screening2)),
      list(title = paste0("SC3: ", input$screening3)),
      list(title = paste0("SC4: ", input$screening4)),
      list(title = paste0("SC5: ", input$screening5)),
      list(title = paste0("EC1: ", input$evaluation1)),
      list(title = paste0("EC2: ", input$evaluation2)),
      list(title = paste0("EC3: ", input$evaluation3)),
      list(title = paste0("EC4: ", input$evaluation4)),
      list(title = paste0("EC5: ", input$evaluation5)))
    )})
  
  # Enable table to be editted by user
  proxy <- dataTableProxy("MCIMTable")
  observeEvent(input$MCIMTable_cell_edit, {
    info <- input$MCIMTable_cell_edit
    str(info)
    i <- info$row
    j <- info$col + 1  # column index offset by 1
    v <- info$value
    df[i, j] <<- DT::coerceValue(v, df[i, j])
    replaceData(proxy, df, resetPaging = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

