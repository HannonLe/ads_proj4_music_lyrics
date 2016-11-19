occur_mat <- lyr[,-1] > 0
mode(occur_mat) <- "numeric"

pca <- prcomp(occur_mat[1:2000,])
dim(pca$x)
dim(pca$rotation)
plot(cumsum(pca$sdev^2)/sum(pca$sdev^2))
p <- t(t(occur_mat[1:2000,] - pca$x %*% t(pca$rotation)) - pca$center)
View(p)

# get train X, train Y and test X
trainX <- na.omit(song_features[1:2000,])

trainY <- pca$x[as.numeric(rownames(trainX)),1:150]
colnames(trainY) <- paste0("PC", 1:150)

testX <- na.omit(song_features[2001:2350,])

dim(trainX)
dim(testX)
dim(trainY)

save(trainX,testX,trainY,occur_mat, file="output/model.RData")
load("output/model.RData")


# high frequency words
freq.words <- colSums(lyr[,-1]) > 100

# nn
library(nnet)

m <- nnet(x=trainX, y=trainY, size=50, rang=1, decay = 5e-4, maxit = 150, MaxNWts = 30000)
colSums(m$fitted.values)
pred <- predict(m, testX, type="raw") 
pred <- t(t(pred %*% t(pca$rotation[,1:150]))+pca$center)
pred <- t(apply(-pred, 1, rank))


# predictive rank sum
occur <- apply(lyr[,-1], 1, function(row) which(row > 0))
pred_rank_sum <- list()
for(i in 2001:2350){
  pred_rank_sum[[i]] <- sum(pred[i-2000,][occur[[i]]]) / mean(1:5000) / length(occur[[i]])
}
pred_rank_sum <- unlist(pred_rank_sum)
mean(pred_rank_sum) * mean(1:5000)

