library(ggplot2)
library(usmap)
library(tidyr)
library(ggmap)
library(data.table)
library(maps)
library(maptools)
library(plyr)
library(dplyr)

fluTweet <- read.csv(file="combine.csv", header=TRUE, sep=",")
nrow(fluTweet)

fluTweet <- fluTweet[!(is.na(fluTweet$lat) | fluTweet$lat==""), ]
nrow(fluTweet)

fluTweet <- unique(fluTweet, by = "status_id")
nrow(fluTweet)
# fluTweet

testPoints <- data.frame(x = fluTweet$lng, y = fluTweet$lat)

register_google(key = 'AIzaSyA4M9jHL1IFbiTzOulWSExn4FbA5vOKMTo')

result <- do.call(rbind,
                  lapply(1:nrow(testPoints),
                         function(i)revgeocode(as.numeric(testPoints[i,1:2]))))

result <- sapply(strsplit(as.character(result), ","), tail, 2, simplify=FALSE)
state <- sapply(strsplit(as.character(result), "\\s+"), `[`, 2, simplify=FALSE)
stateCode <- unlist(state, use.names = FALSE)
fluTweet <- cbind(fluTweet, stateCode)

fluTweet <- read.csv(file="data/tweets.csv", header=TRUE, sep=",")
print(fluTweet)

# Get state list
state_off <- data.frame(state.abb, state.name)
#print(state_off)
fluTweet_j <- inner_join(fluTweet, state_off, by = c("stateCode" = "state.abb"))
nrow(fluTweet_j)




write.csv(fluTweet_j, file = "final_tweets.csv")