# https://colinpriest.com/2015/07/04/tutorial-using-r-and-twitter-to-analyse-consumer-sentiment/



##### to explore in french: http://www.bnosac.be/index.php/blog/54-sentiment-analysis-and-parts-of-speech-tagging-in-dutch-french-english-german-spanish-italian




##### TO RUN AT THE BEGINNING, SORT WHAT IS USEFUL AND WHAT IS NOT



# if (!require('pacman')) install.packages('pacman&')
# pacman::p_load(devtools, installr)
# install.Rtools()
# install_url('http://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz')
# install_url('http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz')
# 
# 
# if (!require('pacman')) install.packages('pacman')
# pacman::p_load(twitteR, sentiment, plyr, ggplot2, wordcloud, RColorBrewer, httpuv, RCurl, base64enc)
# 
# options(RCurlOptions = list(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl')))






### RETRIEVE THE DATA

# harvest some tweets
some_tweets = searchTwitter('RedStarFC', n=10000, lang='fr')

# get the text
some_txt = sapply(some_tweets, function(x) x$getText())






### CLEAN UP THE TEXT

# remove retweet entities
some_txt = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', some_txt)
# remove at people
some_txt = gsub('@\\w+', '', some_txt)
# remove punctuation
some_txt = gsub('[[:punct:]]', '', some_txt)
# remove numbers
some_txt = gsub('[[:digit:]]', '', some_txt)
# remove html links
some_txt = gsub('http\\w+', '', some_txt)
# remove unnecessary spaces
some_txt = gsub('[ \t]{2,}', '', some_txt)
some_txt = gsub('^\\s+|\\s+$', '', some_txt)

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
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL







### ANALYSIS



# Perform Sentiment Analysis
# classify emotion
class_emo = classify_emotion(some_txt, algorithm='bayes', prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by 'unknown'
emotion[is.na(emotion)] = 'unknown'

# classify polarity
class_pol = classify_polarity(some_txt, algorithm='bayes')
# get polarity best fit
polarity = class_pol[,4]
# Create data frame with the results and obtain some general statistics
# data frame with results
sent_df = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))











### PLOTS 

# Let’s do some plots of the obtained results
# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette='Dark2') +
  labs(x='emotion categories', y='number of tweets') +
  ggtitle('Sentiment Analysis of Tweets about RedStarFC\n(classification by emotion)') +
  theme(plot.title = element_text(size=12, face='bold'))


# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette='RdGy') +
  labs(x='polarity categories', y='number of tweets') +
  ggtitle('Sentiment Analysis of Tweets about RedStarFC\n(classification by polarity)') +
  theme(plot.title = element_text(size=12, face='bold'))







### WORDCLOUD

# Separate the text by emotions and visualize the words with a comparison cloud
# separating text by emotion
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep('', nemo)
for (i in 1:nemo)
{
  tmp = some_txt[emotion == emos[i]]
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
comparison.cloud(tdm, colors = brewer.pal(nemo, 'Dark2'),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)





