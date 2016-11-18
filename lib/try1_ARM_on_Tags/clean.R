##### MSD Datset #####

library(dplyr)

# HDF5 data

source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")

library(rhdf5)

setwd("D:/Yinxiang Gao/2015 M.A.STAT Courses/STATGR5243_Applied Data Science/ads_proj4_music_lyrics/data")


sound <- h5read("A/I/D/TRAIDIU128F92F11C7.h5", "/analysis")
meta <- h5read("A/I/D/TRAIDIU128F92F11C7.h5", "/metadata")
musicbrainz <- h5read("A/I/D/TRAIDIU128F92F11C7.h5", "/musicbrainz")

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

str(meta)
meta$artist_terms
meta$artist_terms_freq
meta$artist_terms_weight
meta$songs

str(musicbrainz)
musicbrainz$artist_mbtags
musicbrainz$artist_mbtags_count
musicbrainz$songs

# lyrics
load("lyr.RData")
a <- colnames(lyr)
a <- iconv(a, "", "utf-8")
a[1:10]
charToRaw(a[2])

str(lyr)
id <- lyr[,1]
sum(id != sort(id)) # the original id is in alphabet order
as.character(colnames(lyr)[1000])
hist(rowSums(lyr[,-1])) # number of words in songs

### parse id
id[1]
n <- length(id)
cate_dirs <- paste0(substr(id,3,3), rep("/",n), substr(id,4,4), rep("/",n), substr(id,5,5), rep("/",n), id, rep(".h5"))
### combine data
features <- list()
for(i in 1:n){
  sound <- h5read(cate_dirs[i], "/analysis")
  meta <- h5read(cate_dirs[i], "/metadata")
  musicbrainz <- h5read(cate_dirs[i], "/musicbrainz")
  features[[id[i]]] <- list("sound"=sound, "meta"=meta, "musicbrainz"=musicbrainz)
  print(i)
}
length(features)

H5close()
features_full <- features
save(features_full, file="features_full.RData")

