##### analysis #####

library(dplyr)


setwd("D:/Yinxiang Gao/2015 M.A.STAT Courses/STATGR5243_Applied Data Science/ads_proj4_music_lyrics")

# load all features data
load("output/features.RData")
load("output/features_full.RData")
load("data/lyr.RData") # lyrics data. caculated from "mxm_dataset_train.txt"
id <- lyr[,1]
n <- length(id)
cate_dirs <- paste0(substr(id,3,3), rep("/",n), substr(id,4,4), rep("/",n), substr(id,5,5), rep("/",n), id, rep(".h5"))


# construct song features
## number of segments
v.bars_num <- sapply(1:n, function(i) length(features[[i]]$bars_start)) # how many bars in each song
v.beats_num <- sapply(1:n, function(i) length(features[[i]]$beats_start)) # how many segments in each song
v.segments_num <- sapply(1:n, function(i) length(features[[i]]$segments_start)) # how many segments in each song
v.segments_per_beat <- v.segments_num / v.beats_num
hist(v.segments_per_beat)
## segment length
v.bars_len <- sapply(1:n, function(i) diff(features[[i]]$bars_start))
v.bars_len_median <- sapply(v.bars_len, median)
v.beats_len <- sapply(1:n, function(i) diff(features[[i]]$beats_start))
v.beats_len_median <- sapply(v.beats_len, median)
v.segments_len <- sapply(1:n, function(i) diff(features[[i]]$segments_start))
v.segments_len_mean <- sapply(v.segments_len, mean)
## time signature
v.time_signature <- round(v.bars_len_median / v.beats_len_median,0)
sum(is.na(v.time_signature))
## tempo
v.segments_last <- sapply(1:n, function(i) features[[i]]$segments_start[length(features[[i]]$segments_start)])
v.tempo <- v.beats_num / v.segments_last
## loudness
v.segments_loudness_max <- sapply(1:n, function(i) features[[i]]$segments_loudness_max)
v.segments_loudness_max_peak <- sapply(v.segments_loudness_max, max)
v.segments_loudness_max_valley <- sapply(v.segments_loudness_max, min)
v.segments_loudness_max_mean <- sapply(v.segments_loudness_max, mean)
v.segments_loudness_max_sd <- sapply(v.segments_loudness_max, sd)
## timbre
v.segments_timbre <- sapply(1:n, function(i) t(features[[i]]$segments_timbre))
v.segments_timbre_mean <- do.call(rbind,lapply(v.segments_timbre, colMeans))
colnames(v.segments_timbre_mean) <- paste0("v.segments_timbre_mean_dim",1:12)
v.segments_timbre_sd <- do.call(rbind,lapply(v.segments_timbre, function(mat) apply(mat, 2, sd)))
colnames(v.segments_timbre_sd) <- paste0("v.segments_timbre_sd_dim",1:12)
## pitches
v.segments_pitches <- sapply(1:n, function(i) features[[i]]$segments_pitches)
v.segments_pitches_1st <- lapply(v.segments_pitches, function(pitches) apply(pitches, 2, which.max))
v.segments_pitches_1st_sd <- sapply(v.segments_pitches_1st, sd)
v.segments_pitches_1st_repeat4 <- sapply(v.segments_pitches_1st, function(pitches){
  lag3 <- diff(pitches, lag=3)
  lag2 <- diff(pitches, lag=2)
  lag1 <- diff(pitches, lag=1)
  return(mean((lag3 == 0) & (lag2[1:length(lag3)] == 0) & (lag1[1:length(lag3)] == 0)))
})
v.segments_pitches_1st_repeat3 <- sapply(v.segments_pitches_1st, function(pitches){
  lag2 <- diff(pitches, lag=2)
  lag1 <- diff(pitches, lag=1)
  return(mean((lag2 == 0) & (lag1[1:length(lag2)] == 0)))
})

## rhythm
v.segments_rhythm <- sapply(1:n, function(i) v.segments_len[[i]] * v.tempo[i])
v.segments_rhythm_mean <- sapply(v.segments_rhythm, mean)
v.segments_rhythm_sd <- sapply(v.segments_rhythm, sd)
v.segments_rhythm_pct_long <- sapply(v.segments_rhythm, function(beats) mean(beats > 0.95)) # percentage of segments that is longer than 1 beats

song_features <- data.frame(as.factor(v.time_signature),
                            v.beats_len_median,
                            v.segments_len_mean,
                            v.tempo,
                            v.segments_loudness_max_peak,
                            v.segments_loudness_max_valley,
                            v.segments_loudness_max_mean,
                            v.segments_loudness_max_sd,
                            v.segments_timbre_mean,
                            v.segments_timbre_sd,
                            v.segments_pitches_1st_sd,
                            v.segments_pitches_1st_repeat4,
                            v.segments_pitches_1st_repeat3,
                            v.segments_rhythm_mean,
                            v.segments_rhythm_sd,
                            v.segments_rhythm_pct_long
                            )
dim(song_features)




# topic model
library(topicmodels)

system.time(lda_lyrics <- LDA(x=lyr[1:2000,-1], k=20))
lda_inf <- posterior (lda_lyrics, lyr[,-1])
dim(lda_inf$topics)
topics(lda_lyrics, 1)
rowSums(lda_inf$topics) # all 1
save(lda_lyrics, lda_inf, file="output/lda.RData")
# load("output/lda.RData")
song_topics <- apply(lda_inf$topics,1,which.max)

# write.csv(terms(lda_lyrics, 100), file="output/lda.csv")


# get train X, train Y and test X
trainX <- na.omit(song_features[1:2000,])

trainY <- lda_inf$topics[as.numeric(rownames(trainX)),]
colnames(trainY) <- paste0("topic", 1:ncol(lda_inf$topics))

testX <- na.omit(song_features[2001:2350,])
testY <- lda_inf$topics[as.numeric(rownames(testX)),]

dim(trainX)
dim(testX)
dim(trainY)
dim(testY)

save(trainX,testX,trainY,appearance_mat,file="output/model.RData")
load("output/model.RData")

# high frequency words
freq.words <- colSums(lyr[,-1]) > 100

# nn
library(nnet)

m <- nnet(x=trainX, y=trainY, size=30, rang=0.1, decay = 5e-4, maxit = 400, MaxNWts = 10000)
colSums(m$fitted.values)
pred <- predict(m, testX, type="raw")
mean(apply(pred, 1, which.max) == song_topics[2001:2350])
confusionMatrix(apply(pred, 1, which.max), song_topics[2001:2350])


# predictive rank sum
occur <- apply(lyr[,-1], 1, function(row) which(row > 0))
ref <- apply(terms(lda_lyrics,5000), 2, function(Topic){
  r <- 1:5000
  names(r) <- Topic
  return(r[colnames(lyr[,-1])])
})
colSums(ref)
pred_rank_sum <- list()
for(i in 2001:2350){
  pred_rank <- rank(colSums(pred[i-2000,] * t(ref)))
  pred_rank_sum[[i]] <- sum(pred_rank[occur[[i]]]) / mean(1:5000) / length(occur[[i]])
}
pred_rank_sum <- unlist(pred_rank_sum)
mean(pred_rank_sum) * mean(1:5000) # about 820

# baseline
pred_rank_sum <- list()
pred_rank <- rank(-colSums(lyr[,-1]))
for(i in 2001:2350){
  pred_rank_sum[[i]] <- sum(pred_rank[occur[[i]]]) / mean(1:5000) / length(occur[[i]])
}
pred_rank_sum <- unlist(pred_rank_sum)
mean(pred_rank_sum) * mean(1:5000) # 600
