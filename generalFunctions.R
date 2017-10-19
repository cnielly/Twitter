### The functions used by the other files


#' To treat the tweets
#'
#' @param x a string
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
#'
#' @examples
getTweets <- function(searchWords = c("RedStarFC"), language = "fr"){
  
  tweets <- c()
  
  # Retrieving the tweets
  for (w in words){
    tweets = c(tweets, searchTwitter(w, n=1000, lang=language))
  }
  
  # Treating the data
  tweets = sapply(tweets, function(x) treatTweets(x$getText())) # To treat the tweets
  
  return tweets
}