### Dashboard Team 12 ###

## app.R ##
library(shiny)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Mars 2030"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "Home"),
      menuItem("Token Frequency", tabName = "frequency", icon = icon("chart-bar")),
      menuItem("Sentiment - Bar plots", tabName = "sentiment", icon = icon("chart-bar")),
      menuItem("Sentiment Analysis - Business Outcome", tabName = "yes_no_sent", icon = icon("chart-bar")),
      menuItem("n-grams", icon = icon("chart-bar"), startExpanded = TRUE,
               menuSubItem("Bigram", tabName = "bigram"),
               menuSubItem("Trigram", tabName = 'trigram'),
               menuSubItem("Quadrogram", tabName = 'quadrogram')),
      menuItem("TF-IDF", icon = icon("chart-bar"), startExpanded = TRUE,
               menuSubItem("Single token", tabName = "single_tfidf"),
               menuSubItem("Bigram", tabName = "bigram_tfidf")),
      menuItem("Naive Bayes", tabName = "naive_bayes", icon = icon("chart-bar"))),
    textOutput("res")),
  
  dashboardBody(
    tabItems(
      #Home/About page
      tabItem(tabName = "Home", h2("MsBA2 - Team 12"), 
              h3("Q1. What is the ideal weather condition you would like to live in and why?"), 
              h3("Q2. What is your opinion about electric vehicles?"), 
              h3("Q3. If you can have one item with you  when stranded on an island, what would it be and why?"), 
              h3("Q4. If an alien were to arrive to Earth, would you welcome it, tell it to go away, or take over their homeland/planet?"),
              h3("Q5. If you were a participant on The Survivor, how would you make it to the end?"), 
              h3("Q6. Will you travel with Elon Musk and move to Mars in 2030?")),
      # frequency histograms
      tabItem(tabName = "frequency", h2("Token Frequency in Responses"),
              fluidRow(
                box(
                  width = 6,
                  selectInput("freq_select","Select Question",selected = 1,choices = c(1,2,3,4,5,6))),
                box(
                  width = 12,
                  title = "Histogram plots of word frequency",
                  plotly::plotlyOutput("freq_hist")))),
      # sentiment analysis bar plots
      tabItem(tabName = "sentiment", h2("Sentiment Analysis"),
               fluidRow(
                 box(
                   width = 6,
                   selectInput("sent_select","Select Question",selected = 1,choices = c(1,2,3,4,5,6))),
                 box(
                   width = 12,
                   title = "Sentiment",
                   plotly::plotlyOutput("sentiment")))),
      # token significance by business outcome
      tabItem(tabName = "yes_no_sent", h2("Significant tokens"),
              fluidRow(
                box(
                  width = 6,
                  selectInput("positive_negative", "Select Business Outcome", c("Success", "Failure"))),
                box(
                  width = 12,
                  title = "Token sentiment",
                  plotly::plotlyOutput("token_sign")))),
      
      # bigram
      tabItem(tabName = "bigram", h2("Bigram"),
              fluidRow(
                box(
                  width = 6,
                  selectInput("pos_neg", "Select Business Outcome", c("Success", "Failure"))),
                box(
                  width = 12,
                  title = "Bigram",
                  plotly::plotlyOutput("bigram")),
                box(
                  width = 12,
                  title = "Bigrams Network",
                  plotOutput("bigram_network")))),
      
      # trigram
      tabItem(tabName = "trigram", h2("Trigram"),
              fluidRow(
                box(
                  width = 6,
                  selectInput("pos_neg_tri", "Select Business Outcome", c("Success", "Failure"))),
                box(
                  width = 12,
                  title = "Trigram",
                  plotly::plotlyOutput("trigram")))),
      
      # quadrogram
      tabItem(tabName = "quadrogram", h2("Quadrogram"),
              fluidRow(
                box(
                  width = 6,
                  selectInput("pos_neg_quad", "Select Business Outcome", c("Success", "Failure"))),
                box(
                  width = 12,
                  title = "Quadrogram",
                  plotly::plotlyOutput("quadrogram")))),
      
      # TF-IDF single token
      tabItem(tabName = "single_tfidf", h2("TF-IDF single token"),
              fluidRow(
                box(
                  width = 6,
                  selectInput("pos_neg_tfidf", "Select Business Outcome", c("Success", "Failure"))),
                box(
                  width = 12,
                  title = "TF-IDF",
                  tableOutput("tfidf")))),
      
      # TF-IDF bigrams
      tabItem(tabName = "bigram_tfidf", h2("TF-IDF bigrams"),
              fluidRow(
                box(
                  width = 6,
                  selectInput("pos_neg_tfidf_bigram", "Select Business outcome", c("Success", "Failure"))),
                box(
                  width = 12,
                  title = "TF-IDF",
                  tableOutput("tfidf_bigram")))),
      
      # Naive Bayes Analysis
      tabItem(tabName = "naive_bayes", h2("How well is our models?"),
              fluidRow(
                box(
                  width = 12,
                  title = "Accuracy of training and testing data",
                  verbatimTextOutput("naive_bayes_chi_squared")))))))

server <- function(input, output) {
  output$res <- renderText({ paste("You've selected:", input$tabs)})
  
  freq_hist <- function() {
    survey_tokens %>%
      filter(question == toString(input$freq_select)) %>%
      count(word, sort = TRUE) %>%
      top_n(10) %>%
      mutate(word=reorder(word, n)) %>%
      ggplot(aes(x=word, y=n, fill=10))+
      geom_col()+
      xlab(NULL)+
      coord_flip()
  }
  
  output$freq_hist <- plotly::renderPlotly({freq_hist()})

  sentiment <- function(){
    survey_tokens %>%
      filter(question == toString(input$sent_select)) %>%
      inner_join(nrc) %>% 
      count(word, sentiment, sort = TRUE) %>% 
      group_by(sentiment) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(vars(sentiment), scales = "free") +
      labs(y = "Question 1 Sentiments",
           x = NULL) +
      scale_fill_viridis_d() +
      coord_flip() +
      theme_minimal()
  }
  
  output$sentiment <- plotly::renderPlotly({sentiment()})
  
  token_significance <- function(){
    if (input$positive_negative == "Success"){
      succ_sentiment %>% #tfs=token-frequency-sentiment
        mutate(word=reorder(word, tokenfreqsentiment)) %>% #reorder words by frequency with mutate
        filter(n>1) %>%
        ggplot(aes(word, tokenfreqsentiment))+
        geom_col()+
        xlab(NULL)+
        coord_flip()
    } 
    else {
      fail_sentiment %>% #tfs=token-frequency-sentiment
        mutate(word=reorder(word, tokenfreqsentiment)) %>% #reorder words by frequency with mutate
        filter(n>1) %>%
        ggplot(aes(word, tokenfreqsentiment))+
        geom_col()+
        xlab(NULL)+
        coord_flip()
    }
  }
  
  output$token_sign <- plotly::renderPlotly({token_significance()})
  
  ### n-gram functions from Antonio's code ###
  
  # bigram network
  bigram_network <- function(){
    if (input$pos_neg == "Success"){
      ggraph(succ_big_net, layout = "fr") +
        geom_edge_link()+ #let us plot the arrows
        geom_node_point()+ #set us with these nodes of semantic structure
        geom_node_text(aes(label=name), vjust =1, hjust=1)
    }
    else {
      ggraph(fail_big_net, layout = "fr") +
        geom_edge_link()+ #let us plot the arrows
        geom_node_point()+ #set us with these nodes of semantic structure
        geom_node_text(aes(label=name), vjust =1, hjust=1)
    }
  }
  
  output$bigram_network <- renderPlot({bigram_network()})
  
  # bigram
  bigram <- function(){
    if (input$pos_neg == "Success"){
      count_succ_bigrams %>%
        mutate(word=reorder(word, n)) %>% #reorder words by frequency with mutate
        filter(n>1) %>%
        ggplot(aes(word, n))+
        geom_col()+
        xlab(NULL)+
        coord_flip()
    }
    else {
      count_fail_bigrams %>%
        mutate(word=reorder(word, n)) %>% #reorder words by frequency with mutate
        filter(n>1) %>%
        ggplot(aes(word, n))+
        geom_col()+
        xlab(NULL)+
        coord_flip()
    }
  }
  
  output$bigram <- plotly::renderPlotly({bigram()})
  
  # Trigram
  trigram <- function(){
    if (input$pos_neg_tri == "Success"){
      count_succ_trigram %>%
        mutate(word=reorder(word, n)) %>% #reorder words by frequency with mutate
        ggplot(aes(word, n))+
        geom_col()+
        xlab(NULL)+
        coord_flip()
    }
    else {
      count_fail_trigram %>%
        mutate(word=reorder(word, n)) %>% #reorder words by frequency with mutate
        ggplot(aes(word, n))+
        geom_col()+
        xlab(NULL)+
        coord_flip()
    }
  }
  
  output$trigram <- plotly::renderPlotly({trigram()})
  
  # Quadrogram
  quadrogram <- function(){
    if (input$pos_neg_quad == "Success"){
      count_succ_quadrogram %>%
        mutate(word=reorder(word, n)) %>% #reorder words by frequency with mutate
        ggplot(aes(word, n))+
        geom_col()+
        xlab(NULL)+
        coord_flip()
    }
    else {
      count_fail_quadrogram %>%
        mutate(word=reorder(word, n)) %>% #reorder words by frequency with mutate
        ggplot(aes(word, n))+
        geom_col()+
        xlab(NULL)+
        coord_flip()
    }
  }
  
  output$quadrogram <- plotly::renderPlotly({quadrogram()})
  
  # TF-IDF single token
  tfidf <- function(){
    if (input$pos_neg_tfidf == "Success"){
      success %>%
        count(question, word, sort=TRUE) %>%
        bind_tf_idf(question, word, n) %>%
        arrange(desc(tf_idf)) %>%
        head(10)
    }
    else {
      failure %>%
        count(question, word, sort=TRUE) %>%
        bind_tf_idf(question, word, n) %>%
        arrange(desc(tf_idf)) %>%
        head(10)
    }
  }
  
  output$tfidf <- renderTable({tfidf()})
  
  # TF-IDF bigrams
  tfidf_bigram <- function(){
    if (input$pos_neg_tfidf_bigram == "Success"){
      succ_united %>%
        count(question, bigram) %>%
        bind_tf_idf(bigram, question, n) %>%
        arrange(desc(tf_idf)) %>%
        head(10)
    }
    else {
      fail_united %>%
        count(question, bigram) %>%
        bind_tf_idf(bigram, question, n) %>%
        arrange(desc(tf_idf)) %>%
        head(10)
    }
  }
  
  output$tfidf_bigram <- renderTable({tfidf_bigram()})
  
  # Naive Bayes Table
  naive_bayes_table <- function(){
      CrossTable(pred, testing_data$final_question, 
               prop.chisq = FALSE, prop.t = FALSE, 
               dnn = c('predicted', 'actual'))
  }
  
  output$naive_bayes_chi_squared <- renderPrint({naive_bayes_table()})

}

shinyApp(ui, server)
                     
                     
                     
                     
                     