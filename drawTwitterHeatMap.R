### http://www.r-graph-gallery.com/79-levelplot-with-ggplot2/
### http://www.r-graph-gallery.com/portfolio/heatmap/

#source("generalFunctions.R")
library(lubridate)
library(ggplot2)
library(dplyr)

### Example of grid
# xa <- seq(1,10, length.out=20)
# ya <- seq(1,10, length.out=20)
# dataa <- expand.grid(X=x, Y=y)
# 
# dataa$Z <- runif(400, 0, 5)
# dataa$Z[1][1] = 100
# drawHeatMap(xa, ya, dataa)


# https://stackoverflow.com/questions/21376176/counts-for-time-intervals
#tweets <- getTweets(getOnlyText = FALSE)
t <- tweets %>% select(created)
# table(format(chron::as.chron(t$created), "%d %"))
# table(format(as.Date(t$created), "%d %b"))
halfHours <- ifelse(minute(t$created) < 30, 0, 30)
t$datesHH <- t$created
minute(t$datesHH) <- halfHours; second(t$datesHH) <- 0
sortedHH <- data.frame(table(t[ ,2]))
begin <- sortedHH$Var1[1]
# end <- test$Var1[length(test$Var1)]
#Création de nouvelles colonnes
sortedHH <- sortedHH %>% mutate(date_time = ymd_hms(Var1))
sortedHH <- sortedHH %>% mutate(day = as.Date(date_time))
#sortedHH <- sortedHH %>% mutate(hour = strsplit(as.character(date_time), " "))
sortedHH <- sortedHH %>% mutate(hourGoodFormat = format(date_time, "%H:%M"))
# sortedHH <- sortedHH %>% mutate(hour = tail(hour[[]][2]))
#for (i in seq(1,length(sortedHH$hour)))
 # sortedHH$hour[[i]] <- sortedHH$hour[[i]][2]

#Dessin Gaph
a = sortedHH[["hourGoodFormat"]]
b = sortedHH[["day"]]
data2 = expand.grid(X=a, Y=b)
#data2$Z <- matrix(sortedHH$Freq, nrow = 181, ncol = 181)

#time <- seq(chron::as.chron(as.character(begin)), chron::as.chron(as.character(end)), by = 0.02083333333333333333) # unit is a day
# values <- integer(length(time))


# drawHeatMap <- function(data){
#   ggplot(data, aes(X, Y, Z)) + geom_tile(aes(fill = Z)) + 
#     theme_bw() + 
#     scale_fill_gradient(low="white", high="blue") 
# }


p <- ggplot(sortedHH, aes(x = hourGoodFormat, y = Freq)) +
  geom_point() +
  theme(axis.text.x=element_text(angle=90)) +
  scale_x_discrete(breaks = c("00:00","02:00", "04:00", "06:00","08:00","10:00","12:00","14:00","16:00","18:00","20:00","22:00"))
p
