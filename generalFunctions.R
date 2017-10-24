### The functions used by the other files
library(stringr)

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
getTweets <- function(searchWords = c("RedStarFC"), language = "fr", n = 1000){
  
  tweets <- c()
  
  # Retrieving the tweets
  for (w in searchWords){
    tweets = c(tweets, searchTwitter(w, n, lang=language))
  }
  
  # Treating the data
  tweets = sapply(tweets, function(x) treatTweet(x$getText())) # To treat the tweets
  
  return(tweets)
}
