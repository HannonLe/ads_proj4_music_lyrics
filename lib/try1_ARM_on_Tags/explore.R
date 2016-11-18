##### explore #####

library(dplyr)


setwd("D:/Yinxiang Gao/2015 M.A.STAT Courses/STATGR5243_Applied Data Science/ads_proj4_music_lyrics/data")
# setwd("~/Projects/ads_proj4_music_lyrics/data")

# load all features data
load("features_full.RData")
# lyrics data
load("lyr.RData")
id <- lyr[,1]
n <- length(id)
cate_dirs <- paste0(substr(id,3,3), rep("/",n), substr(id,4,4), rep("/",n), substr(id,5,5), rep("/",n), id, rep(".h5"))

# extract simple features to form a vector
v.durations <- sapply(1:n, function(i) features[[i]]$sound$songs$duration)
v.tempo <- sapply(1:n, function(i) features[[i]]$sound$songs$tempo) # tempo = beats per minute
v.bars_len <- sapply(1:n, function(i) mean(diff(features[[i]]$sound$bars_start, lag=1)))
v.beats_len <- sapply(1:n, function(i) mean(diff(features[[i]]$sound$beats_start, lag=1)))
v.beats_sum <- sapply(1:n, function(i) length(features[[i]]$sound$beats_start))
v.time_signature <- sapply(1:n, function(i) features[[i]]$sound$songs$time_signature)
v.time_signature_confidence <- sapply(1:n, function(i) features[[i]]$sound$songs$time_signature_confidence)
v.title <- sapply(1:n, function(i) features[[i]]$meta$songs$title)
v.song_hotttness <- sapply(1:n, function(i) features[[i]]$meta$songs$song_hotttness)
v.artist_name <- sapply(1:n, function(i) features[[i]]$meta$songs$artist_name)
v.year <- sapply(1:n, function(i) features[[i]]$musicbrainz$songs$year)
v.artist_mbtags <- sapply(1:n, function(i) features[[i]]$musicbrainz$artist_mbtags)
v.artist_mbtags_count <- sapply(1:n, function(i) features[[i]]$musicbrainz$artist_mbtags_count)
v.artist_terms <- sapply(1:n, function(i) features[[i]]$meta$artist_terms)
v.artist_terms_freq <- sapply(1:n, function(i) features[[i]]$meta$artist_terms_freq)
v.artist_terms_weight <- sapply(1:n, function(i) features[[i]]$meta$artist_terms_weight)


# example of song length/tempo/beats analysis
hist(v.durations)
v.wordsum <- rowSums(lyr[,-1])
hist(v.wordsum, breaks = seq(0,1500,50))
v.wordcount <- colSums(lyr[,-1])
sort(v.wordcount, decreasing = T)
plot(v.bars_len, v.wordsum, pch=16, cex=0.5, xlab="song duration/tempo", ylab="words sum")
plot(v.durations*v.tempo, v.beats_sum)
plot(v.beats_sum, v.wordsum)

### tempo to factor
# Larghissimo - very, very slow; extremely slow (24 bpm (beats per minute in a 4
#                                                        4 time) and under)
# Grave - very slow (25-45 bpm)
# Largo - broadly (40-60 bpm)
# Lento - slowly (45-60 bpm)
# Larghetto - rather broadly (60-66 bpm)
# Adagio - slow and stately (literally, "at ease") (66-76 bpm)
# Adagietto - slower than andante (72-76 bpm)
# Andante - at a walking pace (76-108 bpm)
# Andantino - slightly faster than Andante (although in some cases it can be taken to mean slightly slower than andante) (80-108 bpm)
# Marcia moderato - moderately, in the manner of a march[7][8] (83-85 bpm)
# Andante moderato - between andante and moderato (thus the name andante moderato) (92-112 bpm)
# Moderato - moderately (108-120 bpm)
# Allegretto - moderately fast (112-120 bpm)
# Allegro moderato - close to but not quite allegro (116-120 bpm)
# Allegro - fast, quickly, and bright (120-168 bpm) (molto allegro is slightly faster than allegro, but always in its range)
# Vivace - lively and fast (168-176 bpm)
# Vivacissimo - very fast and lively (172-176 bpm)
# Allegrissimo or Allegro vivace - very fast (172-176 bpm)
# Presto - very, very fast; extremely fast (168-200 bpm)
# Prestissimo - very, very, very fast; breakneck (200 bpm and over)
hist(v.tempo)
v.tempo_cut <- ordered(cut(v.tempo,
                           breaks=c(0,25,45,60,66,76,108,120,168,176,200,Inf),
                           labels=c("Larghissimo","Grave","Lento","Larghetto","Adagio","Andante","Moderato","Allegro","Vivace","Presto","Prestissimo")))
table(v.tempo_cut)

### about the meter
plot(v.bars_len / v.beats_len, v.time_signature)

sum(na.omit(abs(v.bars_len / v.beats_len - 4)) < 0.1)
sum(na.omit(abs(v.bars_len / v.beats_len - 3)) < 0.1)
sum(na.omit(abs(v.bars_len / v.beats_len - 1)) < 0.1)
sum(na.omit(abs(v.bars_len / v.beats_len - 5)) < 0.1)
sum(na.omit(abs(v.bars_len / v.beats_len - 7)) < 0.1)

plot(v.beats_len, v.bars_len, xlim=c(0,2), ylim=c(0,12))
abline(v=1)
abline(a=0,b=1,lty=2) # 
abline(a=0,b=3,lty=2) # typical triple: dance styles, for example the waltz, the minuet and the mazurka, and thus also in classical dance music.
abline(a=0,b=4,lty=2) # quadruple: rock, blues, country, funk, and pop
abline(a=0,b=5,lty=2) # quintuple: jazz
abline(a=0,b=7,lty=2) # Septuple meter: European folk music

### about time signature
features[[78]]$sound$songs$time_signature
a <- diff(features[[78]]$sound$beats_start, lag=1)
mean(a) # average length between beats
plot(a, ylim=c(0,max(a)))
v.title[which(v.time_signature==4)]
v.time_signature_confidence[which(v.time_signature==1)]
table(v.time_signature)

### about hotness
hist(v.song_hotttness)
sum(is.na(v.song_hotttness))

### high frequency words
sort(colSums(lyr[,-1]), decreasing = T) # total count
sort(colSums(lyr[2,-1]), decreasing = T)[1:100]

### about artist
sort(table(v.artist_name), decreasing = T)[1:100]
n_distinct(v.artist_name)

### about artist tags
features[[4]]$meta$artist_terms
features[[4]]$meta$artist_terms_freq
features[[4]]$meta$artist_terms_weight
features[[4]]$musicbrainz$artist_mbtags
features[[4]]$musicbrainz$artist_mbtags_count

n_distinct(unlist(v.artist_terms))
n_distinct(unlist(v.artist_mbtags))

### loudness
features[[2]]$sound$songs$loudness

### year of the song
features[[2]]$musicbrainz$songs$year
table(v.year)
n_distinct(v.year)
v.year <- ordered(cut(v.year, breaks=c(1900,1950,1960,1970,1980,1990,2000,Inf), labels=c("before 50","50s","60s","70s","80s","90s","00s"), right=F))


### song title
n_distinct(v.title)
v.title_split <- v.title
v.title_split <- gsub(" \\(.+\\)","", v.title_split)
v.title_split <- gsub(" \\[.+\\]","", v.title_split)
v.title_split <- gsub('\\(|\\)|\\"',"", v.title_split)
v.title_split <- strsplit(tolower(v.title_split), split=" ")
v.title_split

### lyrics
words_appeared <- apply(lyr[,-1],1,function(row) colnames(lyr[-1])[row!=0])

