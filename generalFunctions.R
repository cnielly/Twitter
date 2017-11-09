### The functions used by the other files
library(twitteR)
library(stringr)
library(dplyr)
library(plyr)
library(tidyr)

initializeStuff <- function(){
  consumer_key <- "TKBdocjGFXRyX7G29oHEwPBD1"
  consumer_secret <- "jYNmDeCQeoOyVHm5fxtnLzXGnFZJmBHpc9vyz7sprKtXqmwYGC"
  access_token <- "920274707332640768-MzeAcdB2SiOsv6Ib9SLvRMdc3SdKTuQ"
  access_secret <- "sxiHhSCd0lnwt2biCdJ0Gdny5QZPaZTQ2OeFN05yZndin"
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

