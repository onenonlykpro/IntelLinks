#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)
require(timevis)
# Simple timeline with 4 items
dataBasic <- data.frame(
  id = 1:4,
  content = c("Item one", "Item two" ,"Ranged item", "Item four"),
  start   = c("2016-01-10", "2016-01-11", "2016-01-20", "2016-02-14"),
  end    = c(NA, NA, "2016-02-04", NA)
)


# Template for world cup HTML of each item
prettyDate <- function(d) {
  suppressWarnings(format(as.POSIXct(gsub("T", " ", d), "%Y-%m-%d %H:%M")))
}
randomID <- function() {
  paste(sample(c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = "")
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  tags$head(tags$style(HTML("
                            .shiny-text-output {
                            background-color:#fff;
                            }"))),
  
  h1("Event and Timeline Analysis", 
     style = "font-family: 'Source Sans Pro';
     color: #000; text-align: center;
     padding: 20px"),

  fluidRow(
    column(12,
           p("Event and timeline (E&T) analysis displays events sequentially in a visual manner. Successful application of this 
technique can uncover important trends about a firm’s competitive strategy, an adversary's M.O., and serve as an early-warning 
system by highlighting when an external force takes an action that is unique from its usual or expected behavior.  This method 
is also useful for scenarios in which there are a series of complicated events, which can overwhelm and analyst 
             or decision maker.",
             style = "font-family: 'Source Sans Pro';"),
           p("The code of the app is created (mostly) by Dean Attali.  Be sure to support him by starring his repositories at 
             https://github.com/daattali.",
             style = "font-family: 'Source Sans Pro';"))),
  br(),
   
   fluidRow(column(9,
                   timevisOutput("timelineInteractive")),
            column(3, wellPanel(
              div(id = "interactiveActions",
                  class = "optionsSection",
                  tags$h4("Actions:"),
                  actionButton("fit", "Fit all items"),
                  actionButton("setWindowNoAnim", "Set window without animation"),
                  actionButton("focusSelection", "Focus current selection"))))),
   br(),
   
   fluidRow(column(4,wellPanel(div(class = "optionsSection",
                                   uiOutput("selectIdsOutput", inline = TRUE),
                                   actionButton("selectItems", "Select"),
                                   checkboxInput("selectFocus", "Focus on selection", FALSE)))),
           column(4,wellPanel(div(class = "optionsSection",
                                  textInput("addText", tags$h4("Add item:"), "New item"),
                                  dateInput("addDate", NULL, "2016-01-15"),
                                  actionButton("addBtn", "Add")))),
           column(4,wellPanel(div(class = "optionsSection",
                                  uiOutput("removeIdsOutput", inline = TRUE),
                                  actionButton("removeItem", "Remove"))))),
  fluidRow(column(12,
              div(id = "timelinedata",
                  class = "optionsSection",
                  tags$h4("Data:"),
                  tableOutput("table"),
                  hr(),
                  div(tags$strong("Visible window:"),
                      textOutput("window", inline = TRUE)),
                  div(tags$strong("Selected items:"),
                      textOutput("selected", inline = TRUE))))))

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$timelineInteractive <- renderTimevis({
    config <- list(
      editable = TRUE,
      multiselect = TRUE
    )
    timevis(dataBasic, options = config)})
  
  output$selected <- renderText(
    paste(input$timelineInteractive_selected, collapse = " "))
  
  output$window <- renderText(
    paste(prettyDate(input$timelineInteractive_window[1]),
          "to",
          prettyDate(input$timelineInteractive_window[2])))
  
  output$table <- renderTable({
    data <- input$timelineInteractive_data
    data$start <- prettyDate(data$start)
    if(!is.null(data$end)) {
      data$end <- prettyDate(data$end)
    }
    data
  })
  
  output$selectIdsOutput <- renderUI({
    selectInput("selectIds", tags$h4("Select items:"), input$timelineInteractive_ids,
                multiple = TRUE)})

  output$removeIdsOutput <- renderUI({
    selectInput("removeIds", tags$h4("Remove item"), input$timelineInteractive_ids)})
  
  observeEvent(input$fit, {
    fitWindow("timelineInteractive")})
  
  observeEvent(input$setWindowNoAnim, {
    setWindow("timelineInteractive", "2016-01-07", "2016-01-25",
              options = list(animation = FALSE))})
  
  observeEvent(input$focusSelection, {
    centerItem("timelineInteractive", input$timelineInteractive_selected)})
  
  observeEvent(input$selectItems, {
    setSelection("timelineInteractive", input$selectIds,
                 options = list(focus = input$selectFocus))})
  
  observeEvent(input$addBtn, {
    addItem("timelineInteractive",
            data = list(id = randomID(),
                        content = input$addText,
                        start = input$addDate))})
  
  observeEvent(input$removeItem, {
    removeItem("timelineInteractive", input$removeIds)})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

