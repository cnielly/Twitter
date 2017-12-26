# generalFunctions file
if (!require("twitteR")) install.packages("twitteR")
if (!require("stringr")) install.packages("stringr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("plyr")) install.packages("plyr")
if (!require("tidyr")) install.packages("tidyr")

# drawWordCloud
if (!require("stringr")) install.packages("stringr")
if (!require("tm")) install.packages("tm")
if (!require("wordcloud")) install.packages("wordcloud")
if (!require("RColorBrewer")) install.packages("RColorBrewer")

#drawTwitterHeatMap
if (!require("lubridate")) install.packages("lubridate")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")

# drawTwitterMap
if (!require("twitteR")) install.packages("twitteR")
if (!require("maps")) install.packages("maps")
if (!require("geosphere")) install.packages("geosphere")
if (!require("RColorBrewer")) install.packages("RColorBrewer")

# sentimentAnalysis
library(devtools)
install_url('http://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz')
install_url('http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz')

source("app.R")
