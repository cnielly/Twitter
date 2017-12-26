#' Draws the map of the followers
#'
#' @param userName name of the user you want to map
#' @param userLocation if it is not filled on twitter
#' @param nMax number max of followers
#' @param plotType followers or following or both
#'
#' @return nothing
#' @export
#' @import twitteR maps geosphere RColorBrewer
#' 
#' library(twitteR)
#' library(maps)
#' library(geosphere)
#' library(RColorBrewer)
#'
#' @examples
twitterMap <- function(userName,userLocation=NULL,nMax = 5000,plotType=c("followers","both","following")){
  
  # Get location data
  cat("Getting data from Twitter, this may take a moment.\n")
  tmp = getUser(userName)
  if(is.null(userLocation)){
    userLocation = as.data.frame(tmp)$location
    userLocation = trim(userLocation)
    if(nchar(userLocation) < 2){stop("We can not find your location from Twitter")}
  }
  
  followers=tmp$getFollowers(n=nMax)
  followersLocation = sapply(followers,function(x){as.data.frame(x)$location})
  following = tmp$getFriends(n=nMax)
  followingLocation = sapply(following,function(x){as.data.frame(x)$location})
  
  
  # Load the geographic data
  data(world.cities)
  data(us.cities)
  data(canada.cities)
  
  # Find the latitude and longitude of the user
  cat("Getting geographic (latitude/longitude) of Twitter users.\n")
  userLL <- findLatLon(userLocation)$latlon
  if(any(is.na(userLL))){stop("We can't find the latitude and longitude of your location from Twitter")}
  
  
  # Find the latitude and longitude of each of the followers/following
  # and calcualte the distance to the user
  
  followersLL = matrix(NA,nrow=length(followers),ncol=4)
  followingLL = matrix(NA,nrow=length(following),ncol=4)
  
  for(i in 1:length(followers)){
    if(length(followersLocation[[i]]) > 0){
      tmpLL = findLatLon(trim(followersLocation[[i]]))
      if(any(!is.na(tmpLL$latlon))){
        followersLL[i,] = c(unlist(tmpLL$latlon),distCosine(userLL,tmpLL$latlon),unlist(tmpLL$cont))
      }
    }
  }
  
  for(i in 1:length(following)){
    if(length(followingLocation[[i]]) > 0){
      tmpLL = findLatLon(trim(followingLocation[[i]]))
      if(any(!is.na(tmpLL$latlon))){
        followingLL[i,] =  c(unlist(tmpLL$latlon),distCosine(userLL,tmpLL$latlon),unlist(tmpLL$cont))
      }
    }
  }
  
  followingLL = followingLL[order(-followingLL[,3]),]
  followersLL = followersLL[order(-followersLL[,3]),]
  
  followingLL = followingLL[!is.na(followingLL[,1]),]
  followersLL = followersLL[!is.na(followersLL[,1]),]
  
  
  cat("Plotting results.\n")
  
  ## Just followers
  if(plotType=="followers"){
    plotMap(userName, paste(userName, "Followers.pdf", sep = "_"), followersLL, userLL)
  }
  
  ## Just following
  if(plotType=="following"){
    plotMap(userName, paste(userName, "Following.pdf", sep = "_"), followingLL, userLL)
  }
  
}


findLatLon <- function(loc){
  latlon = NA
  cont = NA
  
  # Asia = 1, Africa = 2, North America = 3, South America = 4, Australia/New Zealand = 5, Europe = 6
  continents = matrix(NA,nrow=length(unique(world.cities[,2])),ncol=2)
  continents[,1] = unique(world.cities[,2])
  continents[1:10,2] = c(1,1,1,2,1,1,1,1,1,1)
  continents[11:20,2]= c(1,1,2,1,1,2,1,2,2,2)
  continents[21:30,2] = c(2,1,6,6,6,6,6,6,6,6)
  continents[31:40,2] = c(6,6,6,6,2,4,4,1,2,1)
  continents[41:50,2] = c(4,6,1,4,6,1,3,1,6,6)
  continents[51:60,2] = c(3,2,4,2,6,1,6,1,3,2)
  continents[61:70,2] = c(1,2,2,2,3,6,3,3,6,6)
  continents[71:80,2] = c(1,1,2,6,3,4,3,4,6,1)
  continents[81:90,2] = c(3,3,3,2,2,6,6,6,6,4)
  continents[91:100,2] = c(2,5,2,2,3,1,1,1,1,1)
  continents[101:110,2] = c(1,2,1,1,1,3,2,5,1,6)
  continents[111:120,2] = c(1,6,1,1,2,6,1,1,6,2)
  continents[121:130,2] = c(6,6,6,1,1,3,4,3,4,2)
  continents[131:140,2] = c(6,6,2,2,1,1,1,4,1,1)
  continents[141:150,2] = c(1,2,2,1,1,1,4,6,6,2)
  continents[151:160,2] = c(4,1,1,1,1,2,4,6,2,2)
  continents[161:170,2] = c(1,2,2,1,6,2,1,1,6,1)
  continents[171:180,2] = c(1,1,1,2,6,2,2,6,1,1)
  continents[181:190,2] = c(2,6,2,1,6,6,3,3,3,3)
  continents[191:200,2] = c(2,2,2,2,3,2,3,2,3,1)
  continents[201:210,2] = c(3,2,2,2,2,2,2,1,6,2)
  continents[211:220,2] = c(1,3,1,6,2,4,3,6,3,4)
  continents[221:230,2] = c(1,1,1,3,2,3,3,6,1,6)
  continents[231:232,2] = c(2,1)
  
  
  # Get the first element of the location
  # firstElement = strsplit(loc,"[^[:alnum:]]")[[1]][1]
  firstElement = strsplit(loc,",")[[1]][1]
  if(is.na(firstElement)){firstElement="zzzzzzzzz"}
  
  # See if it is a city
  tmp = grep(firstElement,world.cities[,1],fixed=TRUE)
  tmp2 = grep(firstElement,state.name,fixed=TRUE)
  tmp3 = grep(firstElement,world.cities[,2],fixed=TRUE)
  
  if(length(tmp) == 1){
    latlon = world.cities[tmp,c(5,4)]
    cont = continents[which(world.cities[tmp,2]==continents[,1]),2]
  }else if(length(tmp) > 1){
    tmpCities = world.cities[tmp,]
    latlon = tmpCities[which.max(tmpCities$pop),c(5,4)]
    cont = continents[which(tmpCities[which.max(tmpCities$pop),2]==continents[,1]),2]
  }else if(length(tmp2) == 1){
    latlon = c(state.center$x[tmp2],state.center$y[tmp2])
    cont = 3
  }else if(length(tmp3) > 0){
    tmpCities = world.cities[tmp3,]
    latlon = tmpCities[which.max(tmpCities$pop),c(5,4)]
    cont = continents[which(tmpCities[which.max(tmpCities$pop),2]==continents[,1]),2]
  }
  
  return(list(latlon=latlon,cont=as.numeric(cont)))
  
}


getGreatCircle = function(userLL,relationLL){
  tmpCircle = greatCircle(userLL,relationLL)
  start = which.min(abs(tmpCircle[,1] - userLL[1,1]))
  end = which.min(abs(tmpCircle[,1] - relationLL[1]))
  greatC = tmpCircle[start:end,]
  return(greatC)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

plotMap <- function (userName, fileName, toPrintLL, userLL){
  #colors
  cols = brewer.pal(7,"Set2")
  bckg = "black"
  continents = "gray"
  
  # pdf(fileName,height=6,width=10)
  data(worldMapEnv)
  map('world',col=continents,bg=bckg,fill=T,mar=rep(0,4),border=0)
  
  #title
  mtext(paste("@",userName," Follower Map",sep=""),col="white")
  
  #links
  nFollowers = dim(toPrintLL)[1]
  for(i in 1:nFollowers){
    greatC = getGreatCircle(userLL,toPrintLL[i,1:2])
    lines(greatC,col=cols[toPrintLL[i,4]],lwd=0.8)
  }
  
  legend(-180,0,legend = c(paste("Asia",sum(toPrintLL[,4]==1)),paste("Africa",sum(toPrintLL[,4]==2)),paste("N. America",sum(toPrintLL[,4]==3)),paste("S. America",sum(toPrintLL[,4]==4)),paste("Australia/N.Z.",sum(toPrintLL[,4]==5)),paste("Europe",sum(toPrintLL[,4]==6))),text.col=cols[1:6],bg=bckg,cex=0.75)
  # dev.off()
}
