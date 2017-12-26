### The functions used by the other files

# generalFunctions file
library(twitteR)
library(stringr)
library(dplyr)
library(plyr)
library(tidyr)

# drawWordCloud
library(stringr)
library(tm)
library(wordcloud)
library(RColorBrewer)

#drawTwitterHeatMap
library(lubridate)
library(ggplot2)
library(dplyr)

# twitterMap
library(twitteR)
library(maps)
library(geosphere)
library(RColorBrewer)

# sentimentAnalysis
library(devtools)
if (!require('pacman')) install.packages('pacman&')
pacman::p_load(devtools)
# install.Rtools()
if (!require('pacman')) install.packages('pacman')
pacman::p_load(twitteR, sentiment, plyr, ggplot2, wordcloud, RColorBrewer, httpuv, RCurl, base64enc, stringr, dplyr, tidyr, stringr, tm, lubridate, maps, geosphere, shinythemes)
options(RCurlOptions = list(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl')))

# sourcing all the files
source("drawWordCloud.R")
source("drawTwitterHeatMap.R")
source("sentimentAnalysis.R")
source("twitterMap.R")

initializeStuff <- function(){
  consumer_key <- "mZVqZrNSR4yk54yTbpUlVYVda"
  consumer_secret <- "t3SBazLWwJ3NGqCnwdMTEfM3R67PyeAcdN0UefOqQFFlPuD7E2"
  access_token <- "920274707332640768-Jqm0YA2RQyYcYGBsYRJlXNdxElW3dEq"
  access_secret <- "5dEeDUZV1qcPN2tDOM1OgIjewNa8RbeqDyNCd7d581Nnu"
  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
  rm(access_secret)
  rm(access_token)
  rm(consumer_key)
  rm(consumer_secret)
}
initializeStuff()

#' To treat the tweets
#'
#' @param x a vector of string
#'
#' @return a string
#' @export
#'
#' @examples
#' \dontrun{
#' treatTweet("The new iPhone sucks")
#' }
treatTweet <- function(x) {tolower(str_replace_all(x,"[^[:graph:]]", " "))}

#' To retrieve the tweets
#'
#' @param searchWords the words to search
#' @param language fr, en
#'
#' @return a list of tweets
#' @export
#' @import twitter
#' @import stringr
#'
#' @examples
getTweets <- function(searchWords = c("RedStarFC"), language = "en", n = 1000, getOnlyText = TRUE){
  
  tweets <- c()
  
  # Retrieving the tweets
  for (w in searchWords){
    tweets = c(tweets, searchTwitter(w, n, lang=language))
  }
  
  # Treating the data
  if(getOnlyText) {tweets <- sapply(tweets, function(x) treatTweet(x$getText()))} # To treat the tweets
  else tweets <- twListToDF(tweets)
  
  return(tweets)
}

source("ui.R")
source("server.R")

runApp(".")