#' Title Sentiment Analysis
#'
#' @param tweets return by the function searchTwitter
#'
#' @return nothing, plots a graph
#' @export
#'
#' @examples
sentimentAnalysis <- function(tweets){

  ### RETRIEVE THE DATA (only the text is interesting here)
  
  txt = tweets$text
  
  
  ### CLEAN UP THE TEXT
  
  # remove retweet entities
  txt = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', txt)
  # remove at people
  txt = gsub('@\\w+', '', txt)
  # remove punctuation
  txt = gsub('[[:punct:]]', '', txt)
  # remove numbers
  txt = gsub('[[:digit:]]', '', txt)
  # remove html links
  txt = gsub('http\\w+', '', txt)
  # remove unnecessary spaces
  txt = gsub('[ \t]{2,}', '', txt)
  txt = gsub('^\\s+|\\s+$', '', txt)
  
  # define 'tolower error handling' function
  try.error = function(x)
  {
    # create missing value
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, 'error'))
      y = tolower(x)
    # result
    return(y)
  }
  # lower case using try.error with sapply
  txt = sapply(txt, try.error)
  
  # remove NAs in txt
  txt = txt[!is.na(txt)]
  names(txt) = NULL
  
  
  
  
  
  
  
  ### ANALYSIS
  
  
  # Code found on the internet to anlyse the sentiments behind the tweets
  # Perform Sentiment Analysis
  # classify emotion
  class_emo = classify_emotion(txt, algorithm='bayes', prior=1.0)
  # get emotion best fit
  emotion = class_emo[,7]
  # substitute NA's by 'unknown'
  emotion[is.na(emotion)] = 'unknown'
  
  # classify polarity
  class_pol = classify_polarity(txt, algorithm='bayes')
  # get polarity best fit
  polarity = class_pol[,4]
  # Create data frame with the results and obtain some general statistics
  # data frame with results
  sent_df = data.frame(text=txt, emotion=emotion,
                       polarity=polarity, stringsAsFactors=FALSE)
  
  # sort data frame
  sent_df = within(sent_df, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
  
  return(list("s_df" = sent_df, "txt" = txt, "emotion" = emotion))
}

printEmotion <- function(sent_df){
  
  # plot distribution of emotions
  ggplot(sent_df, aes(x=emotion)) +
    geom_bar(aes(y=..count.., fill=emotion)) +
    scale_fill_brewer(palette='Dark2') +
    labs(x='emotion categories', y='number of tweets') +
    ggtitle('Sentiment Analysis of Tweets about the word entered\n(classification by emotion)') +
    theme(plot.title = element_text(size=12, face='bold'))
  

}

printPolarity <- function(sent_df){
  
  # plot distribution of polarity
  ggplot(sent_df, aes(x=polarity)) +
    geom_bar(aes(y=..count.., fill=polarity)) +
    scale_fill_brewer(palette='RdGy') +
    labs(x='polarity categories', y='number of tweets') +
    ggtitle('Sentiment Analysis of Tweets about the word entered\n(classification by polarity)') +
    theme(plot.title = element_text(size=12, face='bold'))
}

printEmotionWordCloud <- function(sent_df, txt, emotion){
  
  # Separate the text by emotions and visualize the words with a comparison cloud
  # separating text by emotion
  sent_df = as.data.frame(sent_df)
  emos = levels(factor(sent_df$emotion))
  nemo = length(emos)
  emo.docs = rep('', nemo)
  for (i in 1:nemo)
  {
    tmp = txt[emotion == emos[i]]
    emo.docs[i] = paste(tmp, collapse=' ')
  }
  
  # remove stopwords
  emo.docs = removeWords(emo.docs, stopwords('english'))
  # create corpus
  corpus = Corpus(VectorSource(emo.docs))
  tdm = TermDocumentMatrix(corpus)
  tdm = as.matrix(tdm)
  colnames(tdm) = emos
  
  # comparison word cloud
  comparison.cloud(tdm, colors = brewer.pal(nemo, 'Dark2'), random.order = FALSE, title.size = 1.5)
  
}