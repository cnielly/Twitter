#source("generalFunctions.R")
library(lubridate)
library(ggplot2)
library(dplyr)


#tweets <- getTweets(getOnlyText = FALSE)
halfHours <- ifelse(minute(t$created) < 30, 0, 30)
t$datesHH <- t$created
minute(t$datesHH) <- halfHours; second(t$datesHH) <- 0
sortedHH <- data.frame(table(t[ ,2]))

#Création de nouvelles colonnes
sortedHH <- sortedHH %>% mutate(date_time = ymd_hms(Var1))
sortedHH <- sortedHH %>% mutate(day = as.Date(date_time))
#sortedHH <- sortedHH %>% mutate(hour = strsplit(as.character(date_time), " "))
sortedHH <- sortedHH %>% mutate(hourGoodFormat = format(date_time, "%H:%M"))
# sortedHH <- sortedHH %>% mutate(hour = tail(hour[[]][2]))
#for (i in seq(1,length(sortedHH$hour)))
 # sortedHH$hour[[i]] <- sortedHH$hour[[i]][2]

#Dessin Gaph


p <- ggplot(sortedHH, aes(x = hourGoodFormat, y = Freq)) +
  geom_point() +
  theme(axis.text.x=element_text(angle=90)) +
  scale_x_discrete(breaks = c("00:00","02:00", "04:00", "06:00","08:00","10:00","12:00","14:00","16:00","18:00","20:00","22:00"))
p
