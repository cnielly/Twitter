library(shiny)

shinyServer(function(input, output) {
  #0.
  output$text0 <- renderPrint(
    cat("Please wait when you change the text, retrieving the data from Twitter may take a while.")
  )
  
  tweets <- reactive({
    getTweets(input$word, getOnlyText = FALSE)
  })
  
# 1. 
  output$twitterMapText <- renderPrint(
    cat("This map shows the connexions between the user and a sample of its followers.")
  )
  output$twitterMap <- renderPlot({
    twitterMap(input$userName)
  })
  
# 2. 
  output$wordCloudText <- renderPrint(
    cat("<h1>Word Cloud</h1><br/>This word cloud shows the words associated to the tweet.")
  )
  output$wordCloud <- renderPlot({
    drawWordCloud(input$word, tweets())
  })

  
# 3. 
  output$sentimentAnalysisText <- renderPrint(
    cat("Following are three graphs about sentiments associated to the tweet.")
  ) 
  res <- reactive({
    sentimentAnalysis(tweets())
  })
  
  output$sentimentAnalysis1 <- renderPlot({
    printEmotion(res()$s_df)
  })

  output$sentimentAnalysis2 <- renderPlot({
    printPolarity(res()$s_df)
  })
  output$sentimentAnalysis3 <- renderPlot({
    printEmotionWordCloud(res()$s_df, res()$txt, res()$emotion)
  })
  
# 4.
  output$heatMapText <- renderPrint(
    cat("This plot shows the number of tweets containing the word mentionned regarding the daily timeline. (Note: if the tweet is too famous, since the API can't give back more than a limited number of tweets, it is possible that there are only several dots on the graph)")
  )
  output$heatMap <- renderPlot({
    drawTweetFreq(tweets())
  })
  
})
