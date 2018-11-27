#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)
require(plotly)
require(twitteR)
require(ROAuth)
require(ggplot2)
require(httr)
require(rjson)
require(tm)
require(gridExtra)
require(SnowballC)
require(wordcloud)
require(wordcloud2)
require(RColorBrewer)
require(topicmodels)
require(data.table)
require(dplyr)
require(ngram)
require(RWeka)
require(Rmpfr)
require(Rgraphviz)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  tags$head(tags$style(HTML("
                            .shiny-text-output {
                            background-color:#fff;
                            }"))),
  
  h1("Twitter Account Analysis by Evan Moore", 
     style = "font-family: 'Source Sans Pro';
     color: #000; text-align: center;
     padding: 20px"),
  br(),
  
  fluidRow(
    column(12,
           p("Twitter is one of the most popular social media platforms around the world.  Consequently, businesses and prominent 
             individuals use Twitter to express their emotions and opinions on a variety of topics.  They will also use to it to 
             promote products and ideologies to a mass audience.  This tool will analyze tweets of an account of interest.",
             style = "font-family: 'Source Sans Pro';"),
           p("However, in order to use this tool, you wil need to create an application on Twitter.  This will give you a consumer key, a 
             consumer secret key, an access token, and an access secret key.  If you have created an application on Twitter already, input 
             your keys below.  If you have not, please follow the steps below.",
             style = "font-family: 'Source Sans Pro';"),
           p("1. Go to https://dev.twitter.com/ and login.",
             style = "font-family: 'Source Sans Pro';"),
           p("2. Hover over your profile picture and click on 'My applications'.",
             style = "font-family: 'Source Sans Pro';"),
           p("3. Click on the 'Create a new application' button.",
             style = "font-family: 'Source Sans Pro';"),
           p("4. After you have the completed the form, click on the 'Create my access token' button.",
             style = "font-family: 'Source Sans Pro';"),
           p("5. Enter your consumer key, consumer secret key, access token, and access secret key in the appropriate fields below. 
             Don't worry - this application runs locally.  We do not record your keys in any database.",
             style = "font-family: 'Source Sans Pro';"))),
  br(),
  
  # Prompt user to input Twiiter keys and tokens
  fluidRow(column(12, wellPanel(
    textInput("consumer_key", "Consumer key", value = "xxxxxxxxxxxxxxxxxxxxxxxxx", width = NULL, placeholder = NULL),
    textInput("consumer_secret", "Consumer secret", value = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", width = NULL, placeholder = NULL),
    textInput("access_token", "Access token", value = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", width = NULL, placeholder = NULL),
    textInput("access_secret", "Access secret", value = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", width = NULL, placeholder = NULL),
    textInput("twitter_name", "Enter the Twitter handle of the account of interest without the '@' symbol", value = "xxxxxxxxxx", width = NULL, placeholder = "Twitter handle"),
    actionButton("submit", "Analyze Twitter Account")))),
  br(),
  
  #loading message
  sidebarPanel(
    tags$head(tags$style(type="text/css", "
                         #loadmessage {
                         position: fixed;
                         top: 0px;
                         left: 0px;
                         width: 100%;
                         padding: 5px 0px 5px 0px;
                         text-align: center;
                         font-weight: bold;
                         font-size: 100%;
                         color: #000000;
                         background-color: #CCFF66;
                         z-index: 105;
                         }
                         ")),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div("Loading...",id="loadmessage")),
    #side panel 1
    conditionalPanel(condition="input.tabselected==1",
                     textInput(inputId = "user",
                               label = "Twitter User or Hashtag:"),
                     radioButtons("type", "Search type:",
                                  c("User" = "user2",
                                    "Hashtag" = "hashtag")),
                     actionButton("newUser", "Generate user"),
                     actionButton("newHashtag", "Generate hashtag"),
                     actionButton("update", "Gather data"),
                     hr(),
                     sliderInput("tws", "Maximum number of tweets to obtain:", 
                                 min=100, max= 500, value=250, step = 1),
                     sliderInput("numWords", "Words in bar graph:", 
                                 min=1, max= 35, value=10, step = 1),
                     sliderInput("max",
                                 "Size of wordcloud:",
                                 min = 5,  max = 100, value = 50, step = 5),
                     actionButton("newCloud", "Generate new wordcloud")),
    
    #side panel 2
    conditionalPanel(
      condition="input.tabselected==2",
      hr(),
      helpText("First adjust the word frequency to fit the current data, 
               then experiment with the correlation threshold until the plot is most readable."),
      sliderInput("lowFreq", "Lowest word frequency to appear in plot:", 
                  min=5, max= 100, value=10, step = 1),
      sliderInput("corThreshold", "Minimum correlation to appear in plot:", 
                  min=.05, max= .8, value=.2, step = .01),
      textInput(inputId = "word",
                label = "Find word associations among data:"),
      actionButton("randWord", "Generate word from corpus"),
      sliderInput("corLimit",
                  "Correlation limit in associations:",
                  min = .01,  max = .6, value = .15, step = 0.01)),
    #side panel 3
    conditionalPanel(condition="input.tabselected==3",
                     hr(),
                     helpText("Topic modeling utilizes latent Dirichlet allocation to analyze the corpus, 
                              generate potential topics, and assign each document a topic based on similar words in the text. 
                              The algorithm is run up to 30 times to determine the optimal number of topics."),
                     sliderInput("timesToRun", "Number of topics to try:", 
                                 min=5, max= 30, value=15, step = 1),
                     actionButton("newTopics", "Generate topics"),
                     sliderInput("topicNum", "Number of topics to display:", 
                                 min=1, max= 30, value=10, step = 1),
                     sliderInput("terms", "Number of terms per topic:", 
                                 min=2, max= 10, value=3, step = 1),
                     sliderInput("topicNum2", "Number of topics in heatmap:", 
                                 min=2, max= 30, value=5, step = 1),
                     sliderInput("tweetNum", "Number of tweets in heatmap:", 
                                 min=2, max= 500, value=50, step = 1)),
    #side panel 4
    conditionalPanel(condition="input.tabselected==4",
                     hr(),
                     helpText("An n-gram is a continuous sequence of n words from a given text. 
                              Tweet generation uses a Markov chain to generate random tweets from 
                              the corpus based on the selected n-gram value."),
                     sliderInput("nGram", "N-gram value (Tweet):", 
                                 min=1, max= 10, value=5, step = 1),
                     sliderInput("length", "Tweet length:", 
                                 min=2, max= 10, value=5, step = 1),
                     actionButton("tweet", "Generate tweet from corpus"),
                     sliderInput("nGram2", "N-gram value (Plot):", 
                                 min=2, max= 4, value=2, step = 1),
                     sliderInput("size", "Size of plot:",
                                 min = 1, max = 35, value = 10, step = 1),
                     sliderInput("size2", "Size of wordcloud:",
                                 min = 5, max = 30, value = 15, step = 5),
                     actionButton("newCloud2", "Generate new wordcloud"))),
  
  #conditional main panel
  mainPanel(
    tabsetPanel(
      tabPanel("Word Plots", plotOutput("freqPlot"), wordcloud2Output("wordPlot"), value = 1,  
               conditionalPanel(condition="input.tabselected==1")),
      tabPanel("Word Correlations", textOutput("corrPlotText"),plotOutput("corrPlot"), 
               textOutput("corrTableText"), verbatimTextOutput("corrTable"), value = 2,  conditionalPanel(condition="input.tabselected==2")),
      tabPanel("Topic Modeling" , textOutput("numTopics"), verbatimTextOutput("topics"), 
               plotlyOutput("topicPlot"), value = 3,  conditionalPanel(condition="input.tabselected==3")),
      tabPanel("N-Gram Segmentation", value = 4, conditionalPanel(condition="input.tabselected==4"), 
               verbatimTextOutput("babbleTable"), plotOutput("plot"), wordcloud2Output("plot2")),
      id = "tabselected")))

# Define server logic required to do Twitter account analysis
server <- function(input, output) {
  # Use user's keys to pull tweets of account of interest
   consumerKey <- renderText({input$consumer_key})
   consumerKey <- reactive({consumerKey()})
   consumerSecret <- renderText({input$consumer_secret})
   consumerSecret <- reactive({consumerSecret()})
   accessToken <- renderText({input$access_token})
   accessToken <- reactive({accessToken()})
   accessSecret <- renderText({input$access_secret})
   accessSecret <- reactive({accessSecret()})
   targetAccount <- renderText({paste0("'", input$twitter_name, "'")})
   user <- reactive({targetAccount()})
   
   #input update - user
   observeEvent(input$newUser, handlerExpr = {
     user <- sample(users, size = 1, replace = F)
     updateTextInput(session, "user", value=user)
     updateRadioButtons(session, "type", selected="user2")
   })
   
   #input update - hashtag
   observeEvent(input$newHashtag, handlerExpr = {
     tag <- sample(hashtags, size = 1, replace = F)
     updateTextInput(session, "user", value=tag)
     updateRadioButtons(session, "type", selected="hashtag")
   })
   
   observeEvent(input$update, once = T, handlerExpr = {
     #api validation at beginning of session
     api_key <- consumerKey
     api_secret <- consumerSecret
     access_token <- accessToken
     access_token_secret <- accessSecret
     setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
     
     twitteR:::setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
   })
   
   #data gathering and plot generation
   observeEvent(input$update, handlerExpr = {
     #tweet gathering and error handling based on input type
     if (input$type == "user2") {
       tweets <- try(userTimeline(user = input$user,
                                  n = input$tws, includeRts = FALSE, retryOnRateLimit = 2000))
       
       if(inherits(tweets ,'try-error')){
         return(NULL)
       }
       
       
     } else if (input$type == "hashtag") {
       tweets <- try(searchTwitter(input$user, lang = "en",
                                   n = input$tws, retryOnRateLimit = 2000))
       
       if(inherits(tweets ,'try-error')){
         return(NULL)
       }
     }
     #put tweets and metadata into dataframe
     tweets.df <- try(twListToDF(tweets))
     if(inherits(tweets.df ,'try-error')){
       return(NULL)
     }
     #copy of dataframe for use later
     tweets.df.copy <- tweets.df
     
     #clean text
     cleanStrVec <- function(string_vec) {
       clean_vec <- c()
       for (k in c(1:length(string_vec))) {
         n <-iconv(string_vec[k], "latin1", "ASCII",sub='')
         clean_vec <- c(clean_vec, n)
       }
       return(clean_vec)
     } 
     
     text <- tweets.df$text
     cleaned_text<-cleanStrVec(text)
     
     #create the Corpus object
     myCorpus <- Corpus(VectorSource(cleaned_text))
     # convert to lower case 
     myCorpus <- tm_map(myCorpus, content_transformer(tolower))
     # remove punctuation
     myCorpus <- tm_map(myCorpus, removePunctuation) 
     # remove numbers
     myCorpus <- tm_map(myCorpus, removeNumbers)
     # remove URLs
     removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
     myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
     # add two extra stop words
     myCorpusFull <- myCorpus
     myStopwords <- c(stopwords("english"),stopwords("SMART"), "amp", "use", "see", "used", "will", "im")
     # remove stopwords from corpus
     myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
     #extra stopwords based on input hashtag
     word <- tolower(input$user)
     stop <- gsub("#", "", word)
     moreStopwords <- c(stop, "rt", "fb", "retweet", "a", "b" , "c", "d", "e", "f", "g", "h", "i", 
                        "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t","u","v", "w", "x", "y", "z")
     myCorpus <- tm_map(myCorpus, removeWords, moreStopwords)
     #create copy
     myCorpusCopy <- myCorpus
     # stem words
     myCorpus <- tm_map(myCorpus, stemDocument)
     
     #complete stems
     stemCompletion2 <- function(x, dictionary) {
       x <- unlist(strsplit(as.character(x), " "))
       x <- x[x != ""]
       x <- stemCompletion(x, dictionary=dictionary)
       x <- paste(x, sep="", collapse=" ")
       PlainTextDocument(stripWhitespace(x))
     }
     myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
     myCorpus <- Corpus(VectorSource(myCorpus))
     
     #word frequency dataframe function  
     getConditionedDataFrame <- function(myCorpus) {
       #create the term matrix
       tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
       # calculate the frequency of words and sort it by frequency
       word.freq <- sort(rowSums(as.matrix(tdm)), decreasing = T)
       word.freq <- subset(word.freq, word.freq >=1)
       df <- data.frame(term = names(word.freq), freq = word.freq)
       return(df)
     }
     
     #single string of all tweets for tweet generation function
     getNGramStr <- function(myCorpus) {
       str <- sapply(myCorpus, function(x){x$content})
       str <- concatenate(lapply(str,"[",1))
       return(str)
     }
     
     #dataframe of n-grams for table function
     getNGramDf <- function(myCorpus) {
       token_delim <- " \\t\\r\\n.!?,;\"()"
       sample_df <- data.frame(text=unlist(sapply(myCorpus, '[',"content")), stringsAsFactors=F)
       token <- NGramTokenizer(sample_df, Weka_control(min=input$nGram,max=input$nGram, delimiters = token_delim))
       grams <- data.frame(table(token))
       sorted <- grams[order(grams$Freq,decreasing=TRUE),]
       colnames(sorted) <- c("term", "freq")
       return(sorted)
     }
     
     #dataframe of n-grams for plot function
     getNGramDf2 <- function(myCorpus) {
       token_delim <- " \\t\\r\\n.!?,;\"()"
       sample_df <- data.frame(text=unlist(sapply(myCorpus, '[',"content")), stringsAsFactors=F)
       token <- NGramTokenizer(sample_df, Weka_control(min=input$nGram2,max=input$nGram2, delimiters = token_delim))
       grams <- data.frame(table(token))
       sorted <- grams[order(grams$Freq,decreasing=TRUE),]
       colnames(sorted) <- c("term", "freq")
       return(sorted)
     }
     
     #word frequency dataframe
     tweets.df <- getConditionedDataFrame(myCorpus)
     
     #TDM/DTM for topic modeling 
     tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
     dtm <- as.DocumentTermMatrix(tdm)
     #remove empty values from DTM and original dataframe copy
     rowTotals <- apply(dtm , 1, sum) 
     dtm <- dtm[rowTotals> 0, ]
     tweets.df.copy <- tweets.df.copy[rowTotals> 0,]
     
     #transpose dataframe for random word gathering
     tweets.df.t <- tweets.df[,-2]
     
     #n-gram string
     str <- getNGramStr(myCorpus)
     
     #n-gram dataframe
     df2 <- getNGramDf2(myCorpus)
     
     #randomized color values for plots
     hues <- c(60:330)
     pals <- c(3:8)
     
     observeEvent(input$update, once = T, handlerExpr = {
       #sample one word at random from the text
       word <- sample(tweets.df.t, size = 1, replace = F)
       updateTextInput(session, "word", value=word)
       
       #word frequency barplot
       output$freqPlot <- renderPlot({
         ggplot(tweets.df[1:input$numWords,], aes(x=reorder(term, freq), y=freq, fill = as.factor(term))) +
           geom_bar(stat = "identity", position = "dodge", col= "black") + xlab("Terms") + ylab("Count") +
           coord_flip() + scale_fill_hue(c = sample(hues, 1)) + guides(fill=FALSE)
       })
       
       #wordcloud
       output$wordPlot <- renderWordcloud2({
         validate(
           need(input$max <= nrow(tweets.df), "Selected size greater than number of elements in data")
         )
         new_df <- tweets.df[1:input$max,]
         wordcloud2(new_df, color="random-light", size = .6, shuffle=T, rotateRatio = sample(c(1:100) / 100))
       })
       
       #wordcloud generation
       observeEvent(input$newCloud, handlerExpr = {
         output$wordPlot <- renderWordcloud2({
           validate(
             need(input$max <= nrow(tweets.df), "Selected size greater than number of elements in data")
           )
           new_df <- tweets.df[1:input$max,]
           wordcloud2(new_df, color = "random-light", shuffle=T, size = .6, rotateRatio = sample(c(1:100) / 100))
         })
       })
       
       output$corrPlotText <- renderText ({
         paste("Word Correlation Plot")
       })
       
       #word correlation plot
       output$corrPlot <- renderPlot({
         validate(
           need(input$lowFreq < tweets.df[1,2], "Selected frequency is greater than highest value in data,
                please choose a lower value.")
           )
         plot((tdm),
              terms=findFreqTerms(tdm, lowfreq=input$lowFreq),
              corThreshold=input$corThreshold,
              weighting = T)
       })
       
       output$corrTableText <- renderText ({
         paste("Word Correlation Table")
       })
       
       #word correlation table
       output$corrTable <- renderPrint({
         as.data.frame(findAssocs(tdm, input$word, corlimit=input$corLimit))
       })
       
       #random word gathering from dataframe
       observeEvent(input$randWord, handlerExpr = {
         word <- sample(tweets.df.t, size = 1, replace = F)
         updateTextInput(session, "word", value=word)
       })
       
       #topic modeling 
       observeEvent(input$newTopics, handlerExpr = {
         
         #a kind of average to judge log-liklihood by
         harmonicMean <- function(logLikelihoods, precision = 2000L) {
           llMed <- median(logLikelihoods)
           as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                                prec = precision) + llMed))))
         }
         
         #run LDA model n times
         seqk <- seq(2, input$timesToRun, 1)
         burnin <- 1000
         iter <- 1000
         keep <- 50
         fitted_many <- lapply(seqk, function(k) LDA(dtm, k = k, method = "Gibbs",
                                                     control = list(burnin = burnin, iter = iter, keep = keep)))
         
         # extract logliks from each topic
         logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])
         
         # compute harmonic means
         hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))
         
         #create model
         lda.mod <- LDA(dtm, seqk[which.max(hm_many)], method = "Gibbs", control = list(iter=2000))
         
         #gather topics
         topics <- topics(lda.mod, 1)
         
         output$numTopics <- renderText ({
           paste("Optimal number of topics is", seqk[which.max(hm_many)])
         })
         
         #topic table  
         output$topics <- renderPrint({
           validate(
             need(try(seqk[which.max(hm_many)] >= input$topicNum), "Selected number of topics greater than optimal number.")
           )
           terms <- as.data.frame(terms(lda.mod, input$terms), stringsAsFactors = FALSE)
           t(terms[1:input$topicNum])
         })
         
         #topic/tweet probability heatmap
         output$topicPlot <- renderPlotly({
           validate(
             need(try(seqk[which.max(hm_many)] >= input$topicNum2), "Selected number of topics greater than optimal number.")
           )
           validate(
             need(try(nrow(tweets.df.copy) >= input$tweetNum), "Selected number of tweets greater than amount gathered.")
           )
           #get probabilities
           topics <- as.matrix(posterior(lda.mod, dtm)[["topics"]])
           #trim matrix
           topics <- topics[1:input$tweetNum,1:input$topicNum2]
           #gather and clean tweet text
           text1 <- tweets.df.copy$text
           text1 <- text1[1:input$tweetNum]
           text1<-cleanStrVec(text1)
           text1 <- gsub('http\\S+\\s*', '', text1)
           #copy tweet data for use in heatmap
           n <- nrow(topics) - 1
           text1 = cbind(text1, replicate(n,text1))
           #x-axis parameters
           x <- list(
             title = "Topic number",
             autotick = F,
             ticks = "outside",
             range = c(0.5, input$topicNum2 + 0.5),
             dtick = 1,
             tickcolor = toRGB("blue")
           )
           #y-axis parameters
           y <- list(
             title = "Tweet number",
             autotick = T,
             ticks = "outside",
             range = c(1, input$tweetNum),
             tickcolor = toRGB("blue")
           )
           plot_ly(z = topics,  x = colnames(topics), y = rownames(topics), type = "heatmap", 
                   hoverinfo = 'text', text = text1, colorbar = list(title = "Probability of topic given tweet")) %>% 
             layout(title = "Topic/Tweet Association Heatmap", xaxis = x, yaxis = y)
         })
         
       })
       
       #generate single random tweet at beginning of session
       output$babbleTable <- renderPrint({
         validate(
           need(try(input$nGram <= input$length), "Selected N-Gram value greater than requested tweet length")
         )
         ng <-  ngram(str, input$nGram)
         babble(ng, input$length)
       })
       
       #generate new random tweet    
       observeEvent(input$tweet, handlerExpr = {
         output$babbleTable <- renderPrint({
           validate(
             need(try(input$nGram <= input$length), "Selected N-Gram value greater than requested tweet length")
           )
           ng <-  ngram(str, input$nGram)
           babble(ng, input$length)
         })
       })
       
       #n-gram barplot
       output$plot <- renderPlot({ 
         df2 <- getNGramDf2(myCorpus)
         ggplot(df2[1:input$size,], aes(x=reorder(term, freq), y=freq, fill = as.factor(term))) + 
           geom_bar(stat = "identity", position = "dodge", col= "black") + xlab("Terms") + 
           ylab("Count") + coord_flip() + scale_fill_hue(c = sample(hues, 1)) + guides(fill=FALSE)
       })
       
       #n-gram wordcloud
       output$plot2 <- renderWordcloud2(expr = {
         df2 <- getNGramDf2(myCorpus)
         validate(
           need(input$size2 <= nrow(df2), "Selected size greater than number of elements in data")
         )
         df2 <- df2[1:input$size2,]
         wordcloud2(df2, size = 0.27, rotateRatio = sample(c(1:100) / 100))})
       
       #wordcloud generation
       observeEvent(input$newCloud2, handlerExpr = {
         output$plot2 <- renderWordcloud2(expr = {
           df2 <- getNGramDf2(myCorpus)
           validate(
             need(input$size2 <= nrow(df2), "Selected size greater than number of elements in data")
           )
           df2 <- df2[1:input$size2,]
           wordcloud2(df2, size = 0.27, rotateRatio = sample(c(1:100) / 100))})
       })
     })
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

