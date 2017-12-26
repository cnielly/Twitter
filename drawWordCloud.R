#' Draw a word cloud from tweets
#'
#' @param searchWords the words used to retrieve the tweets
#' @param tweets from getTweets
#' @param language fr or en
#'
#' @return nothing
#' @export
#' @import stringr tm wordcloud RColorBrewer generalFunctions.R
#' 
#' library(stringr)
#' library(tm)
#' library(wordcloud)
#' library(RColorBrewer)
#'
#' @examples
drawWordCloud <- function(searchWords, tweets){
  
  # Treating the data
  tweets_corpus = Corpus(VectorSource(tweets)) # For wordcloud
  tdm = TermDocumentMatrix(tweets_corpus, control = list(removePunctuation = TRUE,
                                                       stopwords = c("https", "nofollow", "com", "twitter", "Twitter", "the", "http", "href", "FALSE", "TRUE", stopwords("en"), sapply(searchWords, treatTweet)), removeNumbers = TRUE, tolower = FALSE)) # tolower bugs, included in treatTweets.
  m = as.matrix(tdm)
  
  word_freqs = sort(rowSums(m), decreasing=TRUE) # Calculating the frequences
  dm = data.frame(word=names(word_freqs), freq=word_freqs)
  
  # Plotting
  wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"), scale=c(2, .2)) # add scale=c(max, min) if too big

}
