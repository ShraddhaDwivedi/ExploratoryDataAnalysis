#################################################################################
#                           sdwivedi@buffalo.edu                                #
#                           yshikhar@buffalo.edu                                #
#################################################################################

## change names of file for different plots or use shiny app

## install devtools package if it's not already
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
## install mapproj package if it's not already
if (!requireNamespace("mapproj", quietly = TRUE)) {
install.packages('mapproj')
}

## install dev version of rtweet from github
devtools::install_github("mkearney/rtweet")

## Function to remove rows based on NA value in particular column
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
## load rtweet package and other package
library(rtweet)
library(mapproj)

## twitter credentials
create_token(
  app = "my_twitter_research_app",
  access_token = '158904066-8UrgRY8p66DicZ5leg64B3cbNGhlopbSDnX8In3B',
  access_secret = 'SYNq5Ls98oD4VnGYZ39nqFm8YZjYvNrUptHNLVSM9PLqv',
  consumer_key = 'CS6hB0QXIphgzC55W86QqP4EO',
  consumer_secret = 'YkWolqp6TK8umMlkg980yVYXDePm8LTt8MYhmDaRexfmDJd9rB')

## twitter search
terms <- "influenza"
rt <- search_tweets(terms, geocode = lookup_coords("usa"), n = 30000)

## storing result as CSV
rf <- apply(rt,2,as.character)
write.table(rf, file = "rt.csv", append= T, row.names=FALSE, col.names = FALSE)

## create lat/lng variables using all available tweet and profile geo-location data
rt<- data.frame(rt)
rt <- lat_lng(rt)


pt <- completeFun(rt, "lat")
pf <- apply(pt,2,as.character)

## Storing data with locations only
write.table(pf, file = "influenza.csv", append= T, row.names=FALSE, col.names = FALSE)

my_data <- read.table("influenza.csv")

## filtering based on duplicate tweet and retweet
filter<-my_data[!duplicated(my_data$V2), ]
filter<-filter[!filter$V11,]

## plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)

## plot lat and lng points onto state map
with(filter, points(V90, V89, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))

## load packages
library(urbnmapr)
library(usmap)
library(maptools)
library(openintro)
library(stringr)

## turning lat,long into states
testPoints <- data.frame(x=as.numeric(as.character(filter$V90)), y=as.numeric(as.character(filter$V89)))
register_google(key = "AIzaSyAEjFBLbaOwtPHi1hXzdoqrwoq5oeNqYF4")
result <- do.call(rbind,
                       lapply(1:nrow(testPoints),
                              function(i) 
                                revgeocode(as.numeric(
                                  testPoints[i,1:2]), output = "address")))
write.csv(result, file = "geo_influenza.csv")

## storing ststes information
result<-read.csv("geo.csv")
result<-data.frame(result)
result$abr<-str_sub(result$V1, -13, -12)
result$state<-abbr2state(result$abr)
st_c <- as.data.frame(table(unlist(result$state)))

library(ggplot2)
library(maps)

map.df <- merge(statedata,st_c, by.x="state_name",by.y='Var1', all.x=T)
map.df$fips <- map.df$state_fips
plot_usmap(data = map.df, values = "Freq", lines = "black") + 
  scale_fill_gradientn(colours=c('green3','yellow','darkorange1','red','red3', 'darkred'),na.value="green3",name = "Activity_Level", label = scales::comma)+
  theme(legend.position = "right")

