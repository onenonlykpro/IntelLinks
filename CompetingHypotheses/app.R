#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(condformat)
library(rhandsontable)
library(DT)
library(data.table)

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
      textInput("hypothesis2", "Hypothesis #2", value = "", width = NULL, placeholder = NULL)
      # textInput("hypothesis3", "Hypothesis #3", value = "", width = NULL, placeholder = NULL),
      # textInput("hypothesis4", "Hypothesis #4", value = "", width = NULL, placeholder = NULL),
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
      # selectInput("consistency3", textOutput("hypothesis3"), 
      #             choices = list("Not Applicable or Neutral" = "N", 
      #                            "Highly consistent with hypothesis" = "CC",
      #                            "Consistent with hypothesis" = "C",
      #                            "Inconsistent with hypothesis" = "I",
      #                            "Highly inconsistent with hypothesis" = "II"), 
      #             selected = "N"),
      # selectInput("consistency4", textOutput("hypothesis4"), 
      #             choices = list("Not Applicable or Neutral" = "N", 
      #                            "Highly consistent with hypothesis" = "CC",
      #                            "Consistent with hypothesis" = "C",
      #                            "Inconsistent with hypothesis" = "I",
      #                            "Highly inconsistent with hypothesis" = "II"), 
      #             selected = "N"),
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
      actionButton("update", "Update ACH")))),
  br(),
  
  fluidRow(column(12,
                  dataTableOutput("ACHPrintout"))))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  values <- reactiveValues()
  values$df <- data.frame(Evidence = numeric(0), 
                          Source = numeric(0),
                          Credibility = numeric(0),
                          Relevance = numeric(0),
                          H1 = numeric(0),
                          H2 = numeric(0))
                          # H3 = numeric(0),
                          # H4 = numeric(0),
                          # H5 = numeric(0),
                          # H6 = numeric(0))
  newEntry <- observe({
    if(input$update > 0) {
      newLine <- isolate(c(input$evidence, input$source, input$credibility, input$relevance, input$consistency1, input$consistency2))
      isolate(values$df[nrow(values$df) + 1,] <- c(input$evidence, input$source, input$credibility, input$relevance, input$consistency1, input$consistency2))
    }
  })
  
  # Define all outputs for UI
  output$ACHTable <- renderTable({values$df})
  output$hypothesis1 <- renderText({ paste0("Consistency with H1: ", input$hypothesis1)})
  output$hypothesis2 <- renderText({ paste0("Consistency with H2: ", input$hypothesis2)})
  # output$hypothesis3 <- renderText({ paste0("Consistency with H3: ", input$hypothesis3)})
  # output$hypothesis4 <- renderText({ paste0("Consistency with H4: ", input$hypothesis4)})
  # output$hypothesis5 <- renderText({ paste0("Consistency with H5: ", input$hypothesis5)})
  # output$hypothesis6 <- renderText({ paste0("Consistency with H3: ", input$hypothesis6)})
  
  output$ACHPrintout <- renderDataTable({
    TestACH <- values$df
    TestACH$CredMultiplier[TestACH$Credibility == "High"] <- 1.414
    TestACH$CredMultiplier[TestACH$Credibility == "Moderate"] <- 1.000
    TestACH$CredMultiplier[TestACH$Credibility == "Low"] <- 0.707
    TestACH$RelaMultiplier[TestACH$Relevance == "High"] <- 1.414
    TestACH$RelaMultiplier[TestACH$Relevance == "Moderate"] <- 1.000
    TestACH$RelaMultiplier[TestACH$Relevance == "Low"] <- 0.707
    TestACH$ConsisScoreH1[TestACH$H1 == "II"] <- -2
    TestACH$ConsisScoreH1[TestACH$H1 == "I"] <- -1
    TestACH$ConsisScoreH1[TestACH$H1 == "CC"] <- 1
    TestACH$ConsisScoreH1[TestACH$H1 == "C" | TestACH$H.1 == "N"] <- 0
    TestACH$ConsisScoreH2[TestACH$H2 == "II"] <- -2
    TestACH$ConsisScoreH2[TestACH$H2 == "I"] <- -1
    TestACH$ConsisScoreH2[TestACH$H2 == "CC"] <- 1
    TestACH$ConsisScoreH2[TestACH$H2 == "C" | TestACH$H.2 == "N"] <- 0
    TestACH$OverallScoreH1 <- (TestACH$ConsisScoreH1 * TestACH$CredMultiplier) * TestACH$RelaMultiplier
    TestACH$OverallScoreH2 <- (TestACH$ConsisScoreH2 * TestACH$CredMultiplier) * TestACH$RelaMultiplier
    Printout <- datatable(TestACH,  colnames = c("Evidence", "Source / Link", "Credibility", "Relevance", "H1", "H2", "Credibility Multiplier", "Relevance Multipier", "H1 Consistency Score", "H2 Consistency Score", "H1 Score", "H2 Score"), 
              options = list(columnDefs = list(list(visible = FALSE, targets = c(7,8,9,10))))) %>% formatStyle(names(TestACH), 
                                                                                                               backgroundColor = styleEqual(c("II", "I", "N", "C", "CC"), c("red", "pink", "white", "lightgreen", "forestgreen"))
              )
    Printout
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

