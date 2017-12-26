library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("cerulean"),
  
  # Application title
  titlePanel("Several Twitter Datas"),
  
  textInput(inputId = "userName", label = "Write your user name:", value = "France"),
  
  textInput(inputId = "word", label = "Write a word:", value = "RStudio"),
  
    
    # Show 4 plots about the tweet concerned
    
    mainPanel(
      
      textOutput("text0"),
      textOutput("wordCloudText"),
      plotOutput("wordCloud"),
      
      textOutput("heatMapText"),
      plotOutput("heatMap"),
      
      textOutput("sentimentAnalysisText"),
      plotOutput("sentimentAnalysis1"),
      plotOutput("sentimentAnalysis2"),
      plotOutput("sentimentAnalysis3"),
      
      textOutput("twitterMapText"),
      plotOutput("twitterMap")
      
    )
  )
)
