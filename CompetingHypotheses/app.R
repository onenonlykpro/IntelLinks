#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require("shiny")
require("condformat")
require("DT")
require("data.table")
require("dplyr")

# Define UI for application
ui <- fluidPage(
  # Application title
  tags$head(tags$style(HTML("
                            .shiny-text-output {
                            background-color:#fff;
                            }"))),
  
  h1("Analysis of Competing Hypotheses by Richards (Dick) J. Heuer, Jr.", 
     style = "font-family: 'Source Sans Pro';
     color: #000; text-align: center;
     padding: 20px"),
  br(),
  
  fluidRow(
    column(12,
           p("Making judgement on an issue that involves a high risk of error in reasoning?  Richards (Dick) J. Heuer, Jr.'s
             Analysis of Competing Hypotheses (ACH) might be useful to you.  This method consists of the following steps:",
             style = "font-family: 'Source Sans Pro';"),
           p("1. Identify all potential hypotheses (this will discourage you from choosing one hyptohesis that you beleive is likely and using evidence to confirm it).",
             style = "font-family: 'Source Sans Pro';"),
           p("2. List evidence and/or findings that are relevant to any of the hypotheses.",
             style = "font-family: 'Source Sans Pro';"),
           p("3. Evaluate each peice of evidence and/or findings against each hypothesis and aim to disprove as many of them as possible.",
             style = "font-family: 'Source Sans Pro';"),
           p("4. Review evidence and hypotheses to determine if you need to find more evidence to disprove any of the hypothesese.",
             style = "font-family: 'Source Sans Pro';"),
           p("5. Consider how your estimate would be affected if evidence and/or findings were wrong or incorrectly interpreted.",
             style = "font-family: 'Source Sans Pro';"))),
  br(),
  
  fluidRow(
    column(6, wellPanel(
      textInput("hypothesis1", "Hypothesis #1", value = "", width = NULL, placeholder = NULL),
      textInput("hypothesis2", "Hypothesis #2", value = "", width = NULL, placeholder = NULL),
      textInput("hypothesis3", "Hypothesis #3", value = "", width = NULL, placeholder = NULL),
      textInput("hypothesis4", "Hypothesis #4", value = "", width = NULL, placeholder = NULL)
      # textInput("hypothesis5", "Hypothesis #5", value = "", width = NULL, placeholder = NULL),
      # textInput("hypothesis6", "Hypothesis #6", value = "", width = NULL, placeholder = NULL)
    )),
    column(6, wellPanel(
      textInput("evidence", "Evidence", value = "", width = NULL, placeholder = NULL),
      textInput("source", "Source / Link", value = "", width = NULL, placeholder = NULL),
      selectInput("credibility", "Credibility", 
                  choices = list("High" = "High", 
                                 "Moderate" = "Moderate",
                                 "Low" = "Low"), 
                  selected = "Moderate"),
      selectInput("relevance", "Relevance", 
                  choices = list("High" = "High", 
                                 "Moderate" = "Moderate",
                                 "Low" = "Low"), 
                  selected = "Moderate"),
      selectInput("consistency1", textOutput("hypothesis1"), 
                  choices = list("Not Applicable or Neutral" = "N", 
                                 "Highly consistent with hypothesis" = "CC",
                                 "Consistent with hypothesis" = "C",
                                 "Inconsistent with hypothesis" = "I",
                                 "Highly inconsistent with hypothesis" = "II"), 
                  selected = "N"),
      selectInput("consistency2", textOutput("hypothesis2"), 
                  choices = list("Not Applicable or Neutral" = "N", 
                                 "Highly consistent with hypothesis" = "CC",
                                 "Consistent with hypothesis" = "C",
                                 "Inconsistent with hypothesis" = "I",
                                 "Highly inconsistent with hypothesis" = "II"), 
                  selected = "N"),
      selectInput("consistency3", textOutput("hypothesis3"), 
                 choices = list("Not Applicable or Neutral" = "N", 
                                "Highly consistent with hypothesis" = "CC",
                                "Consistent with hypothesis" = "C",
                                "Inconsistent with hypothesis" = "I",
                               "Highly inconsistent with hypothesis" = "II"), 
                 selected = "N"),
      selectInput("consistency4", textOutput("hypothesis4"),
                  choices = list("Not Applicable or Neutral" = "N",
                                 "Highly consistent with hypothesis" = "CC",
                                 "Consistent with hypothesis" = "C",
                                 "Inconsistent with hypothesis" = "I",
                                 "Highly inconsistent with hypothesis" = "II"),
                  selected = "N"),
      # selectInput("consistency5", textOutput("hypothesis5"), 
      #             choices = list("Not Applicable or Neutral" = "N", 
      #                            "Highly consistent with hypothesis" = "CC",
      #                            "Consistent with hypothesis" = "C",
      #                            "Inconsistent with hypothesis" = "I",
      #                            "Highly inconsistent with hypothesis" = "II"), 
      #             selected = "N"),
      # selectInput("consistency6", textOutput("hypothesis6"), 
      #             choices = list("Not Applicable or Neutral" = "N", 
      #                            "Highly consistent with hypothesis" = "CC",
      #                            "Consistent with hypothesis" = "C",
      #                            "Inconsistent with hypothesis" = "I",
      #                            "Highly inconsistent with hypothesis" = "II"), 
      #             selected = "N"),
      actionButton("update", "Add ACH Entry")))),
  br(),
  
  fluidRow(column(12,
                  dataTableOutput("ACHPrintout"))),
  br(),
  
  fluidRow(column(2, textOutput("H1Total")),
           column(2, textOutput("H2Total")),
           column(2, textOutput("H3Total")),
           column(2, textOutput("H4Total"))),
  br())

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  
  # Add user input to ACH data frame
  values <- reactiveValues()
  values$df <- data.frame(Evidence = numeric(0), 
                          Source = numeric(0),
                          Credibility = numeric(0),
                          Relevance = numeric(0),
                          H1 = numeric(0),
                          H2 = numeric(0),
                          H3 = numeric(0),
                          H4 = numeric(0))
  # H5 = numeric(0),
  # H6 = numeric(0))
  newEntry <- observe({
    if(input$update > 0) {
      newLine <- isolate(c(input$evidence, input$source, input$credibility, input$relevance, input$consistency1, input$consistency2, input$consistency3, input$consistency4))
      isolate(values$df[nrow(values$df) + 1,] <- c(input$evidence, input$source, input$credibility, input$relevance, input$consistency1, input$consistency2, input$consistency3, input$consistency4))
    }
  })
  
  calcACH <- reactive({
    calcACH <- values$df
    calcACH$CredMultiplier[calcACH$Credibility == "High"] <- 1.414
    calcACH$CredMultiplier[calcACH$Credibility == "Moderate"] <- 1.000
    calcACH$CredMultiplier[calcACH$Credibility == "Low"] <- 0.707
    calcACH$RelaMultiplier[calcACH$Relevance == "High"] <- 1.414
    calcACH$RelaMultiplier[calcACH$Relevance == "Moderate"] <- 1.000
    calcACH$RelaMultiplier[calcACH$Relevance == "Low"] <- 0.707
    calcACH$ConsisScoreH1[calcACH$H1 == "II"] <- -2
    calcACH$ConsisScoreH1[calcACH$H1 == "I"] <- -1
    calcACH$ConsisScoreH1[calcACH$H1 == "CC"] <- 1
    calcACH$ConsisScoreH1[calcACH$H1 == "C" | calcACH$H.1 == "N"] <- 0
    calcACH$ConsisScoreH2[calcACH$H2 == "II"] <- -2
    calcACH$ConsisScoreH2[calcACH$H2 == "I"] <- -1
    calcACH$ConsisScoreH2[calcACH$H2 == "CC"] <- 1
    calcACH$ConsisScoreH2[calcACH$H2 == "C" | calcACH$H.2 == "N"] <- 0
    calcACH$ConsisScoreH3[calcACH$H3 == "II"] <- -2
    calcACH$ConsisScoreH3[calcACH$H3 == "I"] <- -1
    calcACH$ConsisScoreH3[calcACH$H3 == "CC"] <- 1
    calcACH$ConsisScoreH3[calcACH$H3 == "C" | calcACH$H.3 == "N"] <- 0
    calcACH$ConsisScoreH4[calcACH$H4 == "II"] <- -2
    calcACH$ConsisScoreH4[calcACH$H4 == "I"] <- -1
    calcACH$ConsisScoreH4[calcACH$H4 == "CC"] <- 1
    calcACH$ConsisScoreH4[calcACH$H4 == "C" | calcACH$H.4 == "N"] <- 0
    calcACH$OverallScoreH1 <- (calcACH$ConsisScoreH1 * calcACH$CredMultiplier) * calcACH$RelaMultiplier
    calcACH$OverallScoreH2 <- (calcACH$ConsisScoreH2 * calcACH$CredMultiplier) * calcACH$RelaMultiplier
    calcACH$OverallScoreH3 <- (calcACH$ConsisScoreH3 * calcACH$CredMultiplier) * calcACH$RelaMultiplier
    calcACH$OverallScoreH4 <- (calcACH$ConsisScoreH4 * calcACH$CredMultiplier) * calcACH$RelaMultiplier
    calcACH
  })
    
  
  output$ACHPrintout <- renderDataTable({
    calcACH <- calcACH()
    datatable(calcACH, colnames = c("Evidence", "Source / Link", "Credibility", "Relevance", "H1", "H2", "H3", "H4", "Credibility Multiplier", "Relevance Multipier", "H1 Consistency Score", "H2 Consistency Score", "H3 Consistency Score", "H4 Consistency Score", "H1 Score", "H2 Score", "H3 Score", "H4 Score"), 
                          options = list(columnDefs = list(list(visible = FALSE, targets = c(9,10,11,12,13,14))))) %>% formatStyle(names(calcACH), 
                                                                                                                           backgroundColor = styleEqual(c("II", "I", "N", "C", "CC"), c("red", "pink", "white", "lightgreen", "forestgreen"))
                          )
  })
  
  # Define all outputs for UI
  output$hypothesis1 <- renderText({ paste0("Consistency with H1: ", input$hypothesis1)})
  output$H1Total <- renderText({
    calcACH <- calcACH()
    paste0("ACH Score of H1: '", input$hypothesis1, "' is ", sum(calcACH$OverallScoreH1, na.rm = TRUE))
  })
  output$hypothesis2 <- renderText({ paste0("Consistency with H2: ", input$hypothesis2)})
  output$H2Total <- renderText({
    calcACH <- calcACH()
    paste0("ACH Score of H2: '", input$hypothesis2, "' is ", sum(calcACH$OverallScoreH2, na.rm = TRUE))
  })
  output$hypothesis3 <- renderText({ paste0("Consistency with H3: ", input$hypothesis3)})
  output$H3Total <- renderText({
    calcACH <- calcACH()
    paste0("ACH Score of H3: '", input$hypothesis3, "' is ", sum(calcACH$OverallScoreH3, na.rm = TRUE))
  })
  output$hypothesis4 <- renderText({ paste0("Consistency with H4: ", input$hypothesis4)})
  output$H4Total <- renderText({
    calcACH <- calcACH()
    paste0("ACH Score of H4: '", input$hypothesis4, "' is ", sum(calcACH$OverallScoreH4, na.rm = TRUE))
  })
  # output$hypothesis5 <- renderText({ paste0("Consistency with H5: ", input$hypothesis5)})
  # output$hypothesis6 <- renderText({ paste0("Consistency with H3: ", input$hypothesis6)})
  
}
# Run the application 
shinyApp(ui = ui, server = server)