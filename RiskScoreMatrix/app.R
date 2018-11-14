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
  
  h1("Relative Risk Matrix", 
     style = "font-family: 'Source Sans Pro';
     color: #000; text-align: center;
     padding: 20px"),
  br(),
  
  fluidRow(
    column(12,
           p("Measuring risk with a traditional risk matrix may be tempting, but it is a flawed approach.  Arbitrarily assigning 'risk'
             scores to anything will include cognitive biases that are almost certain to negatively affect the accuracy of your findings.  
             However, risk matricies has set 'a standard' among decision makers.  They expect a red-to-green table showing them where their risk
             lies.",
             style = "font-family: 'Source Sans Pro';"),
           p("Rather than assigning arbitrary risk scores, let's standardize measurements that represent different aspects of risk you 
             have identified.  Then, we will use those standardized scores to determine a 'risk score'.",
             style = "font-family: 'Source Sans Pro';"),
           p("Please note that we do not
             recommend any kind of risk score method, because it does not clearly portary what the probably of loss is, nor does it
             portray what those losses are likely to be.  It only shows risk of certain items relative to one another.  Consequently, 
             we recommend this method be used as a last resort.",
             style = "font-family: 'Source Sans Pro'; color: red;"))),
  br(),
  
  fluidRow(
    column(6,
           fileInput("userfile", "Upload your CSV file:",
                     multiple = FALSE,
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
           
           # Horizontal line ----
           tags$hr()),
    column(6,
           # Input: Checkbox if file has header ----
           checkboxInput("header", "Header", TRUE),
           
           # Input: Select separator ----
           radioButtons("sep", "Separator",
                        choices = c(Comma = ",",
                                    Semicolon = ";",
                                    Tab = "\t"),
                        selected = ","),
           
           # Input: Select quotes ----
           radioButtons("quote", "Quote",
                        choices = c(None = "",
                                    "Double Quote" = '"',
                                    "Single Quote" = "'"),
                        selected = '"'))),
  br(),
  
  fluidRow(column(12,
                  # Input: Select number of rows to display ----
                  radioButtons("disp", "Display",
                               choices = c(Head = "head",
                                           All = "all"),
                               selected = "head"),
                  dataTableOutput("RiskPrintout"))))

# Define server logic required to create table
server <- function(input, output) {
  
  output$RiskPrintout <- renderDataTable({
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

