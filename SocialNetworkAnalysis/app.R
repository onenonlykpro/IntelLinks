#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require("shiny")
require("igraph")
require("readr")
require("DT")
require("data.table")

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  tags$head(tags$style(HTML("
                            .shiny-text-output {
                            background-color:#fff;
                            }"))),
  
  h1("Social Network Analysis", 
     style = "font-family: 'Source Sans Pro';
     color: #000; text-align: center;
     padding: 20px"),
  br(),
  
  fluidRow(
    column(12,
           p("Social network analysis is the process of investigating organizations or other social structures through the use of 
            networks and graph theory. It can help produce interesting insights into the entities and people within the organization or social 
             structure by analyzing the ties, edges, or links that connect them.  There a only a few steps you must do to perform a 
             simple social network analysis.",
             style = "font-family: 'Source Sans Pro';"),
           p("1. In a spreadsheet, create a list of ties (i.e. edges or links) of the organization or social structure you wish to analyze.  You may download a 
             template from https://github.com/onenonlykpro/Atheneos/blob/master/SocialNetworkAnalysis/Template.csv.  If you only want a demo of this tool, you can download and use a dataset of Game of Thrones families 
             produced by Shirin Glander (support her by starring her repository at https://github.com/ShirinG).",
             style = "font-family: 'Source Sans Pro';"),
           p("2. Upload your .csv file and confirm your data was uploaded correctly.",
             style = "font-family: 'Source Sans Pro';"))),
  br(),
  
  # Social network analysis setup
  fluidRow(
    column(12,wellPanel(
      fileInput("file1", "Choose CSV File: ",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      checkboxInput("header", "Headers / Column names are in the first row", TRUE)
    ))),
  
  fluidRow(column(3,
                  wellPanel(
                    checkboxInput("directed", "Is this a directed social network (node1 is the source, node2 is the target).", FALSE),
                    radioButtons("layoutChoice", "Select a graph layout:",
                                c("Random" = "layout_randomly",
                                  "Circle" = "layout_in_circle",
                                  "Star" = "layout_as_star",
                                  "Tree" = "layout_as_tree",
                                  "Grid" = "layout_on_grid",
                                  "Force-directed" = "layout_with_fr")),
                    radioButtons("colorChoice", "Select a measure to color nodes by:",
                                 c("Degrees" = "measuredNetwork$degrees",
                                   "Closeness" = "measuredNetwork$closeness",
                                   "Betweenness" = "measuredNetwork$betweenness",
                                   "Eigenvector" = "measuredNetwork$eigenvector")))),
    column(9,
           plotOutput("snaGraph"))
  ),
  
  fluidRow(column(12,
                  dataTableOutput("measuredNetwork")))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  edgelist <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    edgelist <- read.csv(inFile$datapath, header = input$header)
    edgelist
  })
  
  # Construct list of nodes from user's edgelist
  nodelist <- reactive({
    edgelist <- edgelist()
    nodelist1 <- as.data.frame(edgelist$node1)
    nodelist2 <- as.data.frame(edgelist$node2)
    names(nodelist1) <- names(nodelist2)
    nodelist <- rbind(nodelist1, nodelist2)
    nodelist <- unique(nodelist)
    names(nodelist) <- "node"
    nodelist
  })
  
  # Construct social network from edge and node list
  network <- reactive({
    edgelist <- edgelist()
    nodelist <- nodelist()
    network <- graph_from_data_frame(edgelist,
                                 vertices = nodelist,
                                 directed = input$directed)
    network
  })
  
  # Create data frame of centrality measures
  measuredNetwork <- reactive({
    network <- network()
    nodelist <- nodelist()
    degrees <- as.data.frame(degree(network, mode = "all"))
    names(degrees) <- "degrees"
    closeness <- as.data.frame(closeness(network, mode = "all", weights = NA, normalized = TRUE))
    names(closeness) <- "closeness"
    betweennessCentrality <- as.data.frame(betweenness(network, directed = input$directed, weights = NA, normalized = TRUE))
    names(betweennessCentrality) <- "betweenness"
    eigenvector <- as.data.frame(evcent(network, directed = input$directed, scale = TRUE, weights = NULL))
    eigenvector <- eigenvector$vector
    names(eigenvector) <- "eigenvector"
    measuredNetwork <- cbind(nodelist, degrees, closeness, betweennessCentrality, eigenvector)
    measuredNetwork
  })
  
  # Print table of centrality measures
  output$measuredNetwork <- renderDataTable({
    measuredNetwork <- measuredNetwork()
    datatable(measuredNetwork)
  })
  
  # Print SNA graph
  output$snaGraph <- renderPlot({
    network <- network()
    ## Set layout from user choice
    par(mar=c(0,0,0,0))
    if (input$layoutChoice == "layout_randomly") {
      layoutChoice <- layout_randomly
    } else if (input$layoutChoice == "layout_in_circle") {
      layoutChoice <- layout_in_circle
    } else if (input$layoutChoice == "layout_as_star") {
      layoutChoice <- layout_as_star
    } else if (input$layoutChoice == "layout_as_tree") {
      layoutChoice <- layout_as_tree
    } else if (input$layoutChoice == "layout_on_grid") {
      layoutChoice <- layout_on_grid
    } else {
      layoutChoice <- layout_with_fr
    }
    
    ## Set heatmap coloring on user input
    measuredNetwork <- measuredNetwork()
    if (input$colorChoice == "measuredNetwork$degrees") {
      colorChoice <- measuredNetwork$degrees
    } else if (input$colorChoice == "measuredNetwork$closeness") {
      colorChoice <- measuredNetwork$closeness
    } else if (input$colorChoice == "measuredNetwork$betweenness") {
      colorChoice <- measuredNetwork$betweenness
    } else if (input$colorChoice == "measuredNetwork$eigenvector") {
      colorChoice <- measuredNetwork$eigenvector
    }
    oranges <- colorRampPalette(c("lightgray", "dark red"))
    col <- oranges(max(colorChoice) * 5)
    col <- col[colorChoice * 5]
    
    ## Print graph
    snaGraph <- plot(network,
                     vertex.size = 5.0,
                     vertex.color = col,
                     vertex.label.color = "black",
                     vertex.label.cex = .75,
                     edge.curved = .25,
                     edge.color = "grey20",
                     layout = layoutChoice)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

