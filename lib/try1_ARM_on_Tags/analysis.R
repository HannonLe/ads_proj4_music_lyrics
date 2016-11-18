
#### ARM ####

# item list: 
# sound features: time signature(5 factors), tempo_cut(11 factors)
# artist features: artist_name(1305 factors), artist_terms(2078 dummies)
# song features: title, year(58 factors)
table(v.time_signature) # factor
table(v.tempo_cut) # factor
n_distinct(v.artist_name) # factor
n_distinct(v.title) # factor
class(v.artist_terms) # list of words
class(v.artist_mbtags) # list of words
class(v.title_split) # list of words
table(v.year) # factor

class()


# generate binary incidence matrix on the item list for 2350 transactions
m <- list()
for(i in 1:n){
  m[[i]] <- c(paste0("time_signature: ",as.character(v.time_signature[i])),
              paste0("tempo: ",v.tempo_cut[i]),
              paste0("artist_name: ",v.artist_name[i]),
              #paste0("artist_terms: ",v.artist_terms[[i]]),
              paste0("title: ",v.title_split[[i]]),
              paste0("year: ",v.year[[i]]),
              paste0("lyrics: ",words_appeared[[i]])
  )
  if(length(v.artist_mbtags[[i]]) > 0) m[[i]] <- c(m[[i]], paste0("mbtags: ",v.artist_mbtags[[i]]))
}
m[[3]]
sapply(m, length) # number of items in each transaction


library(arules)
trans <- as(m, "transactions") # duplicated items removed
trans
str(trans)
item.labels <- c(trans@itemInfo)$labels
item.lyrics <- item.labels[grep("lyrics: ", item.labels)]
item.features <- item.labels[-grep("lyrics: ", item.labels)]

ARM <- apriori(trans,
               parameter = list(support=0.01, smax=0.05, minlen=1, maxlen=3, confidence=0.96, target="rules"),
               appearance = list(lhs=item.features, rhs=item.lyrics),
               control = list(sort=-2)
)
summary(ARM)
# str(ARM)
inspect(ARM)


#### big regression ####

table(v.time_signature) # factor
table(v.tempo_cut) # factor
n_distinct(v.artist_name) # factor
class(v.artist_terms) # list of words
class(v.artist_mbtags) # list of words
class(v.title_split) # list of words
table(v.year) # factor
