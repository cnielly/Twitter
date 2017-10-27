### http://www.r-graph-gallery.com/79-levelplot-with-ggplot2/
### http://www.r-graph-gallery.com/portfolio/heatmap/

source("generalFunctions.R")

### Example of grid
# x <- seq(1,10, length.out=20)
# y <- seq(1,10, length.out=20)
# data <- expand.grid(X=x, Y=y)
# data$Z <- runif(400, 0, 5)



# https://stackoverflow.com/questions/21376176/counts-for-time-intervals
tweets <- getTweets(getOnlyText = FALSE)
t <- tweets %>% select(created)
# table(format(chron::as.chron(t$created), "%d %"))
# table(format(as.Date(t$created), "%d %b"))
halfHours <- ifelse(minute(t$created) < 30, 0, 30)
t$datesHH <- t$created
minute(t$datesHH) <- halfHours; second(t$datesHH) <- 0
sortedHH <- data.frame(table(t[ ,2]))
begin <- test$Var1[1]
end <- test$Var1[length(test$Var1)]

time <- seq(chron::as.chron(as.character(begin)), chron::as.chron(as.character(end)), by = 0.02083333333333333333) # unit is a day
values <- integer(length(time))

### todo : code the x, y and z for heatmap with values on sortedHH
for(i in 1:length(values)){
  if((sortedHH %>% filter(Var1 == time[i])  )
}




drawHeatMap <- function(x, y, z){
  ggplot(data, aes(X, Y, z= Z)) + geom_tile(aes(fill = Z)) + 
    theme_bw() + 
    scale_fill_gradient(low="white", high="blue") 
}


