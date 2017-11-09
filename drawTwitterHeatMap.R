#' Plot the number of tweets during the day
#'
#' @param tweets 
#'
#' @return nothing
#' @export
#'
#' @examples
#' 
drawTweetFreq <- function(tweets){

  t <- tweets %>% select(created)
  #We are going to work on one specific column of the tweets dataframe: when the tweets were released ("created")
  halfHours <- ifelse(minute(t$created) < 30, 0, 30)
  minute(t$created) <- halfHours
  second(t$created) <- 0
  #Sort the created values in groups of 30 minutes
  sortedHH <- data.frame(table(t[1]))
  
  sortedHH <- sortedHH %>% mutate(date_time = ymd_hms(Var1))
  sortedHH <- sortedHH %>% mutate(day = as.Date(date_time))
  sortedHH <- sortedHH %>% mutate(hour = format(date_time, "%H:%M"))
  #Creation of two new columns for the plot : "date" and "hour"
  
  p <- ggplot(sortedHH, aes(x = hour, y = Freq)) +
    geom_point() +
    xlab("Hour of the day") +
    ylab("Nb of tweets") +
    ggtitle("Number of tweets during a day") +
    theme(axis.text.x=element_text(angle=90)) +
    scale_x_discrete(breaks = c("00:00","02:00", "04:00", "06:00","08:00","10:00","12:00","14:00","16:00","18:00","20:00","22:00"))
  
  p
  #Plotting part
}