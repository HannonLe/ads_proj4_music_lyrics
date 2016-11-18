##### analysis #####

library(dplyr)


setwd("D:/Yinxiang Gao/2015 M.A.STAT Courses/STATGR5243_Applied Data Science/ads_proj4_music_lyrics")
# setwd("~/Projects/ads_proj4_music_lyrics/data")

# load all features data
load("output/features.RData")
load("data/lyr.RData") # lyrics data. caculated from "mxm_dataset_train.txt"
id <- lyr[,1]
n <- length(id)
cate_dirs <- paste0(substr(id,3,3), rep("/",n), substr(id,4,4), rep("/",n), substr(id,5,5), rep("/",n), id, rep(".h5"))


# construct two bars features
in.twobars <- function(i, some_start, bars_start){
  if(length(bars_start) < 2) return(NULL)
  n.bars <- length(bars_start)
  twobars_interval <- cbind(bars_start[-c(n.bars,n.bars-1)],bars_start[-(1:2)])
  ind <- lapply(1:nrow(twobars_interval), function(i) which(some_start >= twobars_interval[i,1] & some_start < twobars_interval[i,2]))
  return(ind)
}

v.bars_num <- sapply(1:n, function(i) length(features[[i]]$bars_start)) # how many bars in each song
v.segments_num <- sapply(1:n, function(i) length(features[[i]]$segments_start)) # how many segments in each song
v.bars_len <- sapply(1:n, function(i) median(diff(features[[i]]$bars_start)))
v.beats_len <- sapply(1:n, function(i) median(diff(features[[i]]$beats_start)))
v.segments_len <- sapply(1:n, function(i) median(diff(features[[i]]$segments_start)))
v.time_signature <- round(v.bars_len / v.beats_len,0)
v.segments_pitches <- sapply(1:n, function(i) features[[i]]$segments_pitches)

bad_songs <- which(v.bars_num < 5)

get.twobars_features <- function(features){
  n <- length(features)
  twobars_features <- list()
  
  for(i in (1:n)[-bad_songs]){
    # time signature of two bars
    twobars_time_signature <- v.time_signature[i]
    # total number of segments in two bars
    ind_segments <- in.twobars(i,features[[i]]$segments_start, features[[i]]$bars_start)
    twobars_segments_num <- sapply(ind_segments, length)
    
    good_segs <- which(twobars_segments_num >= 2) # keep two bars that has at least 2 segments, cuz bars with too few segments are not actually informative (doubtful though)
    ind_segments <- ind_segments[good_segs]
    twobars_segments_num <- twobars_segments_num[good_segs]
    
    # total number of beats in two bars
    ind_beats <- in.twobars(i,features[[i]]$beats_start, features[[i]]$bars_start)
    twobars_beats_num <- sapply(ind_beats, length)[good_segs]
    # length (in second) of two bars
    bars_len <- diff(features[[i]]$bars_start)
    twobars_len <- bars_len[-length(bars_len)] + bars_len[-1]
    twobars_len <- twobars_len[good_segs]
    # tempo of two bars
    twobars_tempo <- twobars_beats_num / twobars_len * 60
    # mean timbre of two bars (dim=12)
    twobars_segments_timbre_mean <- t( sapply(1:length(ind_segments), function(j) rowMeans(features[[i]]$segments_timbre[ , ind_segments[[j]] ])) )
    colnames(twobars_segments_timbre_mean) <- paste0("twobars_segments_timbre_mean_dim",1:12)
    # mean, var, max, min of loudness for all segments in two bars
    twobars_loudness_max_peak <- sapply(1:length(ind_segments), function(j) max(features[[i]]$segments_loudness_max[ind_segments[[j]]]))
    twobars_loudness_max_valley <- sapply(1:length(ind_segments), function(j) min(features[[i]]$segments_loudness_max[ind_segments[[j]]]))
    twobars_loudness_max_mean <- sapply(1:length(ind_segments), function(j) mean(features[[i]]$segments_loudness_max[ind_segments[[j]]]))
    twobars_loudness_max_var <- sapply(1:length(ind_segments), function(j) var(features[[i]]$segments_loudness_max[ind_segments[[j]]]))
    # variation of pitches, pitch repeats, pitch rises in two bars
    segments_pitches_1st <- apply(v.segments_pitches[[i]], 2, function(col) which.max(col))
    twobars_segments_pitches_1st <- sapply(1:length(ind_segments), function(j) segments_pitches_1st[ind_segments[[j]]])
    twobars_segments_pitches_1st_var <- sapply(twobars_segments_pitches_1st, var)
    twobars_segments_pitches_1st_pct_same.as.last <-  sapply(twobars_segments_pitches_1st, function(pitches) mean(diff(pitches) == 0))
    twobars_segments_pitches_1st_pct_higher.than.last <-  sapply(twobars_segments_pitches_1st, function(pitches) mean(diff(pitches) > 0))
    # mean, variance, percentage of long segments of segments length within two bars (normalized by tempo)
    segments_len <- diff(features[[i]]$segments_start)
    twobars_segments_rhythm <- sapply(1:length(ind_segments), function(j) segments_len[ind_segments[[j]]] * twobars_tempo[[j]] / 60)
    twobars_segments_rhythm_mean <- sapply(twobars_segments_rhythm, mean)
    twobars_segments_rhythm_var <- sapply(twobars_segments_rhythm, var)
    twobars_segments_rhythm_pct_long <- sapply(twobars_segments_rhythm, function(beats) mean(beats > 0.95)) # percentage of segments that is longer than 1 beats
    
    twobars_features[[i]] <- data.frame(i,
                                        twobars_time_signature,
                                        twobars_segments_num,
                                        twobars_beats_num,
                                        twobars_len,
                                        twobars_tempo,
                                        twobars_segments_timbre_mean,
                                        twobars_loudness_max_peak,
                                        twobars_loudness_max_valley,
                                        twobars_loudness_max_mean,
                                        twobars_loudness_max_var,
                                        twobars_segments_pitches_1st_var,
                                        twobars_segments_pitches_1st_pct_same.as.last,
                                        twobars_segments_pitches_1st_pct_higher.than.last,
                                        twobars_segments_rhythm_mean,
                                        twobars_segments_rhythm_var,
                                        twobars_segments_rhythm_pct_long
                                        )
    print(i)
  }
  return(twobars_features)
}

twobars_features <- get.twobars_features(features)
n.twobars <- unlist(sapply(twobars_features, nrow))
names(n.twobars) <- (1:n)[-bad_songs]
length(n.twobars)

str(twobars_features[[1]])
View(twobars_features[[1]])

twobars_features_df <- do.call(rbind, twobars_features)
dim(twobars_features_df)
sum(is.na(twobars_features_df))
dim(na.omit(twobars_features_df))

save(twobars_features, n.twobars, twobars_features_df, file="output/twobars.RData")
load("output/twobars.RData")


# topic model
library(topicmodels)

system.time(lda_lyrics <- LDA(x=lyr[1:2000,-1], k=20))
lda_inf <- posterior(lda_lyrics, lyr[1:2000,-1])
song_topics <- paste0("topic",apply(lda_inf$topics,1,which.max))
rowSums(lda_inf$topics) # all 1
# save(lda_lyrics, lda_inf, file="output/lda.RData")
load("output/lda.RData")
topics(lda_lyrics, 1)
# write.csv(terms(lda_lyrics, 100), file="output/lda.csv")


# get train X, train Y and test X

trainX <- na.omit(twobars_features_df[twobars_features_df$i <= 2000,])
trainY <- song_topics[trainX$i]
testX <- na.omit(twobars_features_df[twobars_features_df$i > 2000,])
testY <- song_topics[testX$i]

trainX <- trainX[,-1]
testX <- testX[,-1]

dim(trainX)
dim(testX)
length(trainY)

# gbm?
library(gbm)
rd <- sample(1:nrow(trainX), 20000)
gb <- gbm.fit(trainX[rd,], trainY[rd], distribution="multinomial")
gb$n.trees
gb$valid.error
summary(gb)
pred <- apply(predict(gb, testX, n.trees=100),1,which.max)
mean(pred == testY)
table(pred)
