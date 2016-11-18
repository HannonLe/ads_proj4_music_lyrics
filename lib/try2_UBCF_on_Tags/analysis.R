##### analysis #####

library(dplyr)


setwd("D:/Yinxiang Gao/2015 M.A.STAT Courses/STATGR5243_Applied Data Science/ads_proj4_music_lyrics/data")
# setwd("~/Projects/ads_proj4_music_lyrics/data")

# load all features data
load("features.RData")
# lyrics data. caculated from "mxm_dataset_train.txt"
load("lyr.RData")
id <- lyr[,1]
n <- length(id)
cate_dirs <- paste0(substr(id,3,3), rep("/",n), substr(id,4,4), rep("/",n), substr(id,5,5), rep("/",n), id, rep(".h5"))


str(features[[1]])
View(features[[1]]$segments_pitches)



#### User-based Collaborative Filtering ####

# 1. construct all segments matrix
all_segments_mat_list <- list()
for(i in 1:n){
  segments_len <- with(features[[i]], diff(segments_start, lag = 1))
  segments_len <- c(segments_len, segments_len[length(segments_len)])
  segments_mat <- with(features[[i]], cbind(segments_len, # segments length
                                            segments_loudness_max,
                                            segments_loudness_max_time,
                                            segments_loudness_start,
                                            t(segments_pitches), # 12 dim pitches features
                                            t(segments_timbre)) # 12 dim timbre features
  )
  # dim(segment_mat) = length(segments_start) by 28
  all_segments_mat_list[[i]] <- segments_mat
  print(i)
}
all_segments_mat <- do.call(rbind, all_segments_mat_list)
dim(all_segments_mat)
sum(!is.numeric(all_segments_mat)) # all numeric

all_segments_mat_train <- do.call(rbind, all_segments_mat_list[1:2000])


# 2. clustering

### normalization
norm_sd <- apply(all_segments_mat_train, 2, sd) # normalize factor
norm_mean <- colMeans(all_segments_mat_train) # normalize factor
norm_multiplier <- c(sqrt(1/4), rep(sqrt(1/4 / 3), 3), rep(sqrt(1/4 / 12), 12), rep(sqrt(1/4 / 12), 12)) # IMPORTANT parameters
sum(norm_multiplier^2) # check

NORM <- function(segments_mat_train) t((t(segments_mat_train)-norm_mean)/norm_sd*norm_multiplier)

all_segments_mat_train_norm <- NORM(all_segments_mat_train) # normalized
colMeans(all_segments_mat_train_norm) # check
apply(all_segments_mat_train_norm, 2, sd) # check

### clustering
set.seed(666)
rand_segments <- sample(1:nrow(all_segments_mat_norm), 100000)
system.time({
  cl <- kmeans(all_segments_mat_norm[rand_segments,], centers=1000, iter.max=20)
})
dim(cl$centers)

# 3. Construct bag of 1000 sound features
library(caret)
knn_model <- knn3(x=cl$centers, y=as.factor(1:1000), k=1)
BOW <- list()
for(i in 1:n){
  BOW[[i]] <- table(predict(knn_model, NORM(all_segments_mat_list[[i]]), type="class"))
  print(i)
}
BOW <- do.call(rbind, BOW) # bag of words constructed

# 4. combine with lyrics
dim(lyr[,-1])
dim(BOW)

m <- as.matrix(cbind(BOW, lyr[,-1]))
dim(m)

# 5. user-based collaborative filtering
library(recommenderlab)

### prepare real rating matrix
m1 <- m
m1[2001:2350,1001:6000] <- NA # test songs 2001 to 2350
dimnames(m1) <- list(song=paste0("s",1:n), item=c(paste0("cluster",1:1000),colnames(lyr[,-1])))
colnames(m1)
r <- as(m1, "realRatingMatrix")
getRatingMatrix(r)
identical(as(r, "matrix"),m1)

r_m <- normalize(r)
getRatingMatrix(r_m)

hist(getRatings(r), breaks=100)

hist(getRatings(normalize(r)), breaks=100)
hist(rowCounts(r), breaks=50)

### recommender
recommenderRegistry$get_entries(dataType = "realRatingMatrix")
my_recommender <- Recommender(r[1:2000,], method="UBCF")
names(getModel(my_recommender))


### prediction of my recommender
pred.rank.sum <- function(pred) 1/length(pred) / mean(1:5000) * sum(pred)

system.time({
  recom <- predict(my_recommender, r[2001:2350,], n=5000)
})


pred_rank_sum <- c()
pred_wordrank <- as(recom, "list")
for(i in 1:350){
  pred <- 1:5000 # ranks
  system.time({
    names(pred) <- pred_wordrank[[i]] # words
  })
  names(pred) <- pred_wordrank[[i]] # words
  pred <- pred[colnames(lyr[,-1])] # reorder
  pred <- pred[which(lyr[2000 + i, -1] != 0)] # keep non-zero appearance words
  pred_rank_sum[i] <- pred.rank.sum(pred) # calculate predictive rank sum # less than one is good
  print(i)
}
mean(pred_rank_sum) # should be less than 1
hist(pred_rank_sum)

### baseline: random guess. 
pred_rank_sum_baseline_random <- c()
for(i in 1:350){
  pred <- sample(1:5000, size=5000, replace=F) # ranks
  names(pred) <- as(recom, "list")[[i]] # words
  pred <- pred[colnames(lyr[,-1])] # reorder
  pred <- pred[which(lyr[2000 + i, -1] != 0)] # keep non-zero appearance words
  pred_rank_sum_baseline_random[i] <- 1/length(pred) / mean(1:5000) * sum(pred) # calculate predictive rank sum # less than one is good
}
mean(pred_rank_sum_baseline_random) # Should be around 1.
hist(pred_rank_sum_baseline_random)


### baseline: use word frequency.
pred_rank_sum_baseline_wf <- c()
for(i in 1:350){
  pred <- rank(-colSums(lyr[,-1]))
  pred <- pred[which(lyr[2000 + i, -1] != 0)] # keep non-zero appearance words
  pred_rank_sum_baseline_wf[i] <- 1/length(pred) / mean(1:5000) * sum(pred) # calculate predictive rank sum # less than one is good
}
mean(pred_rank_sum_baseline_wf)
hist(pred_rank_sum_baseline_wf)
