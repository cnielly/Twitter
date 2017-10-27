### http://www.r-graph-gallery.com/79-levelplot-with-ggplot2/
### http://www.r-graph-gallery.com/portfolio/heatmap/

#source("generalFunctions.R")
library(lubridate)

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
sortedHH <- sortedHH %>% mutate(hour = strsplit(as.character(date_time), " "))
# sortedHH <- sortedHH %>% mutate(hour = tail(hour[[]][2]))
for (i in seq(1,length(sortedHH$hour)))
  sortedHH$hour[[i]] <- sortedHH$hour[[i]][2]

#Dessin Gaph
a = sortedHH[["hour"]]
b = sortedHH[["day"]]
data2 = expand.grid(X=a, Y=b)
data$Z <- matrix(sortedHH$Freq, nrow = 181, ncol = 181)

#time <- seq(chron::as.chron(as.character(begin)), chron::as.chron(as.character(end)), by = 0.02083333333333333333) # unit is a day
# values <- integer(length(time))

### todo : code the x, y and z for heatmap with values on sortedHH


drawHeatMap <- function(data){
  ggplot(data, aes(X, Y, Z)) + geom_tile(aes(fill = Z)) + 
    theme_bw() + 
    scale_fill_gradient(low="white", high="blue") 
}

# d1 <- "2017-06-03"
# d2 <- "2017-06-04"
# d3 <- "2017-06-05"
# x = c(as.Date(d1), as.Date(d2), as.Date(d3))
# x1 = c(d1,d2,d3)
# h1 <- "08:00:00"
# h2 <- "08:30:00"
# y = c(as.character(hms(h1)), as.character(hms(h2)))
# y2 = c(h1,h2)
# 
# data = expand.grid(X=x, Y=y)
# data$Z <- matrix(c(1,2,3,4,5,6), nrow = 6, ncol = 1)
# 
# drawHeatMap(data)

p <- ggplot(sortedHH, aes(x = unlist(hour), y = Freq)) + geom_point() + scale_x_continuous(name, breaks, labels, limits, trans)
p 
