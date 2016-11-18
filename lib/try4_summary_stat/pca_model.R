pca <- prcomp(lyr[,-1])
dim(pca$x)
dim(pca$rotation)
View(lyr[,-1] - pca$x %*% t(pca$rotation))
plot(cumsum(pca$sdev^2)/sum(pca$sdev^2))
p <- t(t(lyr[,-1] - pca$x %*% t(pca$rotation)) - pca$center)


# get train X, train Y and test X
trainX <- na.omit(song_features[1:2000,])

trainY <- pca$x[as.numeric(rownames(trainX)),1:50]
colnames(trainY) <- paste0("PC", 1:50)

testX <- na.omit(song_features[2001:2350,])
testY <- pca$x[as.numeric(rownames(testX)),1:50]

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

m <- nnet(x=trainX, y=trainY, size=20, rang=0.01, decay = 5e-3, maxit = 500, MaxNWts = 10000)
colSums(m$fitted.values)
pred <- predict(m, testX, type="raw")
pred <- t(t(pred%*% t(pca$rotation[,1:50]))+pca$center)
pred <- t(apply(-pred, 1, rank))


# predictive rank sum
occur <- apply(lyr[,-1], 1, function(row) which(row > 0))
pred_rank_sum <- list()
for(i in 2001:2350){
  pred_rank_sum[[i]] <- sum(pred[i-2000,][occur[[i]]]) / mean(1:5000) / length(occur[[i]])
}
pred_rank_sum <- unlist(pred_rank_sum)
mean(pred_rank_sum) * mean(1:5000)

