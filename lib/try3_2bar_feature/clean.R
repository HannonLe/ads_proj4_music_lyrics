##### MSD Datset #####

library(dplyr)

# HDF5 data

source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")

library(rhdf5)

setwd("D:/Yinxiang Gao/2015 M.A.STAT Courses/STATGR5243_Applied Data Science/ads_proj4_music_lyrics")


sound <- h5read("data/A/I/D/TRAIDIU128F92F11C7.h5", "/analysis")


str(sound)
length(sound$bars_confidence)
sound$bars_start
sound$bars_confidence
sound$bars_confidence
sound$beats_start
sound$sections_start
sound$tatums_start
sound$segments_pitches
sound$segments_start
sound$songs

# lyrics
load("data/lyr.RData")
a <- colnames(lyr)
a <- iconv(a, "", "utf-8")
a[1:10]

str(lyr)
id <- lyr[,1]
sum(id != sort(id)) # the original id is in alphabet order
as.character(colnames(lyr)[1000])
hist(rowSums(lyr[,-1])) # number of words in songs

### parse id
id[1]
n <- length(id)
cate_dirs <- paste0("data/",substr(id,3,3), rep("/",n), substr(id,4,4), rep("/",n), substr(id,5,5), rep("/",n), id, rep(".h5"))
### combine data
features <- list()
for(i in 1:n){
  features[[id[i]]] <- h5read(cate_dirs[i], "/analysis")
  print(i)
}
length(features)

H5close()
save(features, file="output/features.RData")

