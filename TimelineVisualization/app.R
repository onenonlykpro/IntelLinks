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
templateWC <- function(stage, team1, team2, score1, score2) {
  sprintf(
    '<table><tbody>
      <tr><td colspan="3"><em>%s</em></td></tr>
      <tr>
        <td>%s</td>
        <th>&nbsp;%s - %s&nbsp;</th>
        <td>%s</td>
      </tr>
      <tr>
        <td><img src="flags/%s.png" width="31" height="20" alt="%s"></td>
        <th></th>
        <td><img src="flags/%s.png" width="31" height="20" alt="%s"></td>
      </tr>
    </tbody></table>',
    stage, team1, score1, score2, team2, gsub("\\s", "", tolower(team1)),
    team1, gsub("\\s", "", tolower(team2)), team2
  )
}

prettyDate <- function(d) {
  suppressWarnings(format(as.POSIXct(gsub("T", " ", d), "%Y-%m-%d %H:%M")))
}

randomID <- function() {
  paste(sample(c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = "")
}


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Timeline Analysis by Dean Attali"),
   br(),
   
   fluidRow(column(12,
                   timevisOutput("timelineInteractive"))),
   br(),
   
   fluidRow(column(12, wellPanel(
     div(id = "interactiveActions",
         class = "optionsSection",
         tags$h4("Actions:"),
         actionButton("fit", "Fit all items"),
         actionButton("setWindowAnim", "Set window 2016-01-07 to 2016-01-25"),
         actionButton("setWindowNoAnim", "Set window without animation"),
         actionButton("center", "Center around 2016-01-23"),
         actionButton("focusSelection", "Focus current selection"),
         actionButton("addTime", "Add a draggable vertical bar 2016-01-17"))))),
   fluidRow(column(8,
       fluidRow(column(4,
           div(class = "optionsSection",
               uiOutput("selectIdsOutput", inline = TRUE),
               actionButton("selectItems", "Select"),
               checkboxInput("selectFocus", "Focus on selection", FALSE))),
           column(4,
                  div(class = "optionsSection",
                      textInput("addText", tags$h4("Add item:"), "New item"),
                      dateInput("addDate", NULL, "2016-01-15"),
                      actionButton("addBtn", "Add"))),
           column(4,
                  div(class = "optionsSection",
                      uiOutput("removeIdsOutput", inline = TRUE),
                      actionButton("removeItem", "Remove"))))),
       column(4,
              div(id = "timelinedata",
                  class = "optionsSection",
                  tags$h4("Data:"),
                  tableOutput("table"),
                  hr(),
                  div(tags$strong("Visible window:"),
                      textOutput("window", inline = TRUE)),
                  div(tags$strong("Selected items:"),
                      textOutput("selected", inline = TRUE))))),
   div(class = "sourcecode",
       "The code of the app is created (mostly) by Dean Attali.  Be sure to support him by starring his repositories ",
       tags$a(href = "https://github.com/daattali", "on GitHub")))

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
  
  observeEvent(input$setWindowAnim, {
    setWindow("timelineInteractive", "2016-01-07", "2016-01-25")})
  
  observeEvent(input$setWindowNoAnim, {
    setWindow("timelineInteractive", "2016-01-07", "2016-01-25",
              options = list(animation = FALSE))})
  
  observeEvent(input$center, {
    centerTime("timelineInteractive", "2016-01-23")})
  
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
  
  observeEvent(input$addTime, {
    addCustomTime("timelineInteractive", "2016-01-17", randomID())})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

