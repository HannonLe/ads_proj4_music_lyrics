##### explore #####

library(dplyr)

setwd("D:/Yinxiang Gao/2015 M.A.STAT Courses/STATGR5243_Applied Data Science/ads_proj4_music_lyrics/data")
# setwd("~/Projects/ads_proj4_music_lyrics/data")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# lyrics data. caculated from "mxm_dataset_train.txt"
load("lyr.RData")
id <- lyr[,1]
n <- length(id)
cate_dirs <- paste0(substr(id,3,3), rep("/",n), substr(id,4,4), rep("/",n), substr(id,5,5), rep("/",n), id, rep(".h5"))

### high frequency words
sort(colSums(lyr[,-1]), decreasing = T) # total count
cumsum(sort(colSums(lyr[,-1]), decreasing = T)) / sum(sort(colSums(lyr[,-1]), decreasing = T)) # first 1000 words account for 89% of occurance
sort(colSums(lyr[2,-1]), decreasing = T)[1:100]

### lyrics
words_appeared <- apply(lyr[,-1],1,function(row) colnames(lyr[-1])[row!=0])

### topic modeling
library(topicmodels)

system.time(lda_lyrics <- LDA(x=lyr[,-1], k=20))
# save(lda_lyrics, file="D:/Yinxiang Gao/2015 M.A.STAT Courses/STATGR5243_Applied Data Science/ads_proj4_music_lyrics/output/lda.RData")

topics(lda_lyrics, 1)
lda_inf <- lda_lyrics@loglikelihood
write.csv(terms(lda_lyrics, 100), file="D:/Yinxiang Gao/2015 M.A.STAT Courses/STATGR5243_Applied Data Science/ads_proj4_music_lyrics/output/lda.csv")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# load all features data
load("features_full.RData")
load("features.RData")


# example of song length/tempo/beats analysis
v.durations <- sapply(1:n, function(i) features_full[[i]]$sound$songs$duration)
hist(v.durations)
v.wordsum <- rowSums(lyr[,-1])
hist(v.wordsum, breaks = seq(0,1500,50))
v.wordcount <- colSums(lyr[,-1])
sort(v.wordcount, decreasing = T)


### about the meter
v.bars_len <- sapply(1:n, function(i) median(diff(features[[i]]$bars_start)))
hist(v.bars_len)
v.beats_len <- sapply(1:n, function(i) median(diff(features[[i]]$beats_start)))
hist(v.beats_len)
v.segments_len <- sapply(1:n, function(i) median(diff(features[[i]]$segments_start)))
hist(v.segments_len)

v.time_signature <- sapply(1:n, function(i) features_full[[i]]$sound$songs$time_signature)
table(v.time_signature)

plot( round(v.bars_len / v.beats_len,0), v.time_signature) # exactly the same, perfect!
v.time_signature <- round(v.bars_len / v.beats_len,0)

### tempo = number of beats per minute
v.tempo <- sapply(1:n, function(i) features_full[[i]]$sound$songs$tempo)
v.beats_num <- sapply(1:n, function(i) length(features_full[[i]]$sound$beats_start))
plot(v.beats_num/v.durations, v.tempo)
v.tempo <- v.beats_num/v.durations


### about pitch: chroma feature

lapply(v.segments_pitches, dim) # 12 dims for each segment

# melody
pitches <- c("C","C#","D","D#","E","F","F#","G","G#","A","A#","B")
v.segments_pitches <- sapply(1:n, function(i) features[[i]]$segments_pitches)
v.year <- sapply(1:n, function(i) features_full[[i]]$musicbrainz$songs$year)
which(v.year>2005)

features_full[[1477]]$meta$songs$title
features_full[[1477]]$meta$songs$artist_name

melody <- unlist(apply(v.segments_pitches[[1477]], 2, function(col) which(col==1)))
factor(melody, levels=1:12, labels=pitches)

# two-bar feature
v.bars_num <- sapply(1:n, function(i) length(features_full[[i]]$sound$bars_start)) # how many bars in each song
sum(v.bars_num)
v.segments_num <- sapply(1:n, function(i) length(features_full[[i]]$sound$segments_start))
hist(v.segments_num)

some_start <- features[[2]]$segments_start
bars_start <- features[[2]]$bars_start

in.twobars <- function(i, some_start, bars_start){
  if(length(bars_start) < 2) return(NULL)
  n.bars <- length(bars_start)
  twobars_interval <- cbind(bars_start[-c(n.bars,n.bars-1)],bars_start[-(1:2)])
  ind <- lapply(1:nrow(twobars_interval), function(i) which(some_start >= twobars_interval[i,1] & some_start < twobars_interval[i,2]))
  return(ind)
}

v.twobars_beats_num <- sapply(1:n, function(i){
  ind <- in.twobars(i,features[[i]]$beats_start, features[[i]]$bars_start)
  return(sapply(ind, length))
})

v.twobars_segments_num <- sapply(1:n, function(i){
  ind <- in.twobars(i,features[[i]]$segments_start, features[[i]]$bars_start)
  return(sapply(ind, length))
})

features_full[[994]]$meta$songs$title
features_full[[994]]$meta$songs$artist_name
v.twobars_segments_num[[994]]



### about timbre: MFCC feature
v.segments_timbre <- sapply(1:n, function(i) features[[i]]$segments_timbre)
dim(v.segments_timbre[[1]])

# mds visualization
v.artist_terms <- sapply(1:n, function(i) features_full[[i]]$meta$artist_terms)
ind_metal <- which(sapply(v.artist_terms, function(tags) "metal" %in% tags[1:5]) == TRUE)
table(v.time_signature[ind_metal])
mean(v.tempo[ind_metal])
ind_jazz <- which(sapply(v.artist_terms, function(tags) "jazz" %in% tags[1:5]) == TRUE)
table(v.time_signature[ind_jazz])
mean(v.tempo[ind_jazz])
ind_blues <- which(sapply(v.artist_terms, function(tags) "blues" %in% tags[1:5]) == TRUE)
table(v.time_signature[ind_blues])
mean(v.tempo[ind_blues])
ind_guitar <- which(sapply(v.artist_terms, function(tags) "guitar" %in% tags[1:5]) == TRUE)
table(v.time_signature[ind_guitar])
mean(v.tempo[ind_guitar])
ind_soul <- which(sapply(v.artist_terms, function(tags) "soul" %in% tags[1:5]) == TRUE)
table(v.time_signature[ind_soul])
mean(v.tempo[ind_soul])
ind_rap <- which(sapply(v.artist_terms, function(tags) "rap" %in% tags[1:5]) == TRUE)
table(v.time_signature[ind_rap])
mean(v.tempo[ind_rap])
ind_black <- which(sapply(v.artist_terms, function(tags) "black" %in% tags[1:5]) == TRUE)
table(v.time_signature[ind_black])
mean(v.tempo[ind_black])

#### 34,35 jazz, 63,64 metal
index <- c(43,49,102,107)
temp <- lapply(index, function(i) t(v.segments_timbre[[i]]))
temp <- do.call(rbind, temp)
dim(temp)

cmd <- cmdscale(dist(temp), k=2)
cmd
plot(cmd[,1], cmd[,2], pch=16, cex=0.5, col=rep(c(1,1,2,2),sapply(index, function(i) ncol(v.segments_timbre[[i]]))),
     main="Mutidimensional Scaling (2D)", xlab="D1", ylab="D2")


