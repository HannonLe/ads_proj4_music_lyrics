##### analysis final ####

library(dplyr)
library(arules)
library(nnet)
library(topicmodels)


setwd("D:/Yinxiang Gao/2015 M.A.STAT Courses/STATGR5243_Applied Data Science/ads_proj4_music_lyrics")

# load all features data
load("output/features.RData")
load("output/features_test.RData")
load("data/lyr.RData") # lyrics data. caculated from "mxm_dataset_train.txt"
id <- lyr[,1]
n <- length(id)

get.features <- function(features){
  n <- length(features)
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
  return(song_features)
}

song_features <- get.features(features)
song_features_test <- get.features(features_test)
dim(song_features)
dim(song_features_test)

# topic model
library(topicmodels)

system.time(lda_lyrics <- LDA(x=lyr[1:2000,-1], k=20))
lda_inf <- posterior(lda_lyrics, lyr[,-1])
dim(lda_inf$topics)
song_topics <- topics(lda_lyrics, 1)
rowSums(lda_inf$topics) # all 1
save(lda_lyrics, lda_inf, song_topics, file="output/lda.RData")
load("output/lda.RData")

# delete non-english songs (belongs to topics 6,8,14)
write.csv(terms(lda_lyrics, 100), file="output/lda.csv") # inspect csv file to determine non-english topics
nonEng <- which(song_topics == 6 | song_topics == 8 | song_topics == 14)
length(nonEng)
ind <- (1:2350)[-nonEng]

# pca model
occur_mat <- lyr[,-1] > 0
mode(occur_mat) <- "numeric"

pca <- prcomp(lyr[,-1][1:2000,])
dim(pca$x)
dim(pca$rotation)
plot(cumsum(pca$sdev^2)/sum(pca$sdev^2))
# p <- t(t(occur_mat[1:2000,] - pca$x %*% t(pca$rotation)) - pca$center)
# View(p)

# get train X, train Y and test X
trainX <- na.omit(song_features[ind[ind<=2000],])

trainY <- pca$x[as.numeric(rownames(trainX)),1:100]
colnames(trainY) <- paste0("PC", 1:100)

testX <- na.omit(song_features[ind[ind>2000],])

dim(trainX)
dim(testX)
dim(trainY)

save(trainX,testX,trainY,occur_mat, file="output/model.RData")
load("output/model.RData")


# nn
library(nnet)

m <- nnet(x=trainX, y=trainY, size=50, rang=1, decay = 5e-4, maxit = 100, MaxNWts = 30000)
colSums(m$fitted.values)
pred <- predict(m, testX, type="raw") 
pred <- t(t(pred %*% t(pca$rotation[,1:100]))+pca$center)
pred <- t(apply(-pred, 1, rank))


# predictive rank sum
occur <- apply(lyr[,-1], 1, function(row) which(row > 0))
pred_rank_sum <- list()
test_ind <- ind[ind>2000]
for(i in 1:length(test_ind)){
  pred_rank_sum[[i]] <- sum(pred[i,][occur[[test_ind[i]]]]) / mean(1:5000) / length(occur[[test_ind[i]]])
}
pred_rank_sum <- unlist(pred_rank_sum)
mean(pred_rank_sum) * mean(1:5000)


# baseline
pred_rank_sum <- list()
pred_rank <- rank(-colSums(lyr[,-1]))
for(i in 1:length(test_ind)){
  pred_rank_sum[[i]] <- sum(pred_rank[occur[[test_ind[i]]]]) / mean(1:5000) / length(occur[[test_ind[i]]])
}
pred_rank_sum <- unlist(pred_rank_sum)
mean(pred_rank_sum) * mean(1:5000) # 600

# true test
testX <- song_features_test
dim(testX)
pred <- predict(m, testX, type="raw")
pred <- t(t(pred %*% t(pca$rotation[,1:100]))+pca$center)
colnames(lyr)[c(2,3,6:30)]
pred[,c(2,3,6:30)-1] <- -Inf
pred <- t(apply(-pred, 1, rank))
dim(pred)

write.csv(pred, file="test.csv")
