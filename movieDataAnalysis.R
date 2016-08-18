rm(list = ls())
# Read and import data
movieData <- read.csv(file.choose(), header = T, sep = ",")
attach(movieData)
names(movieData)

# Ratings 
my.rating = You.rated
imdb.rating = IMDb.Rating
m <- matrix(data=cbind(my.rating, imdb.rating), nrow=length(my.rating), ncol=2)
colnames(m) <- c('My rating', 'IMDb rating')
df.rating <- as.data.frame(m)
df.rating.stack = stack(df.rating)
ggplot(df.rating.stack, aes(x=values)) + 
  geom_density(aes(group=ind, colour=ind, fill=ind), alpha=0.3)

# Genre
genre <- as.character(Genres)
genre[which(genre == "")] <- "drama"
first.genre = sapply(strsplit(genre, ","), "[", 1)
unique.genre = unique(first.genre)
df.genre = data.frame(first.genre)

count.genre <- NULL
for (g in unique.genre){
  count.genre = c(count.genre,length(df.genre[which(df.genre$first.genre == g),]))
}
df.count.genre = data.frame(count.genre, unique.genre)
ggplot(df.count.genre, aes(x = reorder(unique.genre, count.genre), y = count.genre)) +
   geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(color="black", 
                                   size=10, angle=35))


  
    
# Year
years = cut(Year, breaks = c(1930,1960,1990,2016), labels = c("1930-1960","1961-1990","1991-2016"))
head(years)
df.year = data.frame(years)
ggplot(df.year, aes(x=years)) +
  geom_bar() +
  theme(axis.text.x = element_text(color="black", 
                                   size=10, angle=35))

# My rating and genres
df.rating.genre = data.frame(rating = my.rating, genre = first.genre)
ggplot(df.rating.genre, aes(x=my.rating)) + 
  geom_density(aes(group=first.genre, colour=first.genre, fill=first.genre), alpha=0.3)


# Fitting (linear regression)
lm.fit <- lm(formula = my.rating ~ imdb.rating + first.genre)
fitted.val = fitted(lm.fit)
# abline(a=0,b=1,lty="dashed")
summary(lm.fit)
# head(as.data.frame(cbind(fitted(lm.fit), my.rating, imdb.rating)))

# Fitiing (random forest)
rf.train.1 <- data.frame(imdb.rating, first.genre)
rf.label <- as.factor(my.rating)
set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)


### TO-DO
# mean(my.rating[first.genre == "drama"])
mean.rating <- NULL
for (g in unique.genre){
    mean.rating = c(mean.rating,mean(df.rating.genre[which(df.rating.genre$genre == g),1]))
}
df.mean.rating.genre = data.frame(mean.rating, unique.genre)
ggplot(df.mean.rating.genre, aes(x = reorder(unique.genre,mean.rating), y = mean.rating)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(color="black", 
                                   size=10, angle=35))


  
  
