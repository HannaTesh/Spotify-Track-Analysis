  ##Analysis Based on the Spotify dataset 
df <- read.csv("dataset.csv")

#Hypothesis 1: Energy, Danceability, Valence, and Loudness have a stronger positive correlation with popularity.
str(df[, c("energy", "popularity")])

summary(df$energy)
summary(df$popularity)

model1 <- lm(popularity ~ energy + danceability + valence + loudness, data = df)
summary(model1)

#Hypothesis 2: A song's loudness can be predicted by the songs energy levels, danceability and tempo 

model2 <- lm(loudness ~ energy + danceability + tempo, data = df )
summary(model2)

##from the summary it is clear that they can predict a song's loudness
##Seeing if the R squared score can be improved further? Are there other effects that we aren't taking into consideration 
##Since danceability and energy are the most related, would like to see further if their interaction explains the model better 
model2a <- lm(loudness ~ energy*danceability + tempo, data = df )
summary(model2a)

#Hypothesis 3:Duration of the song and the danceability 
#are the most important predictors of determining the popularity of a song*.

model3 <- lm(popularity ~ duration_ms + danceability, data = df)
summary(model3)


############ Hypothesis 4: Can we predict a track's genre from its audio features? ############

library(tree)

# 1. Keep only audio features + track_genre and drop NAs
genre_df <- na.omit(df[, c("danceability", "energy", "valence",
                           "acousticness", "instrumentalness", "liveness",
                           "speechiness", "loudness", "tempo",
                           "mode", "key", "time_signature",
                           "track_genre")])

# Make sure genre is a factor
genre_df$track_genre <- factor(genre_df$track_genre)

# 2. Keep only the 5 most common genres
top_genres <- names(sort(table(genre_df$track_genre), decreasing = TRUE))[1:5]
genre_top  <- subset(genre_df, track_genre %in% top_genres)
genre_top$track_genre <- droplevels(genre_top$track_genre)

# 3. Train / test split (70% train, 30% test)
set.seed(123)
n <- nrow(genre_top)
train_idx   <- sample(1:n, size = 0.7 * n)
train_genre <- genre_top[train_idx, ]
test_genre  <- genre_top[-train_idx, ]

# 4. Baseline accuracy: always predict the most common genre in the test set
baseline_acc <- max(prop.table(table(test_genre$track_genre)))

# 5. Classification tree to predict genre from audio features
tree_genre <- tree(track_genre ~ ., data = train_genre)

# 6. Predict on test data + confusion matrix + accuracy
pred_genre <- predict(tree_genre, newdata = test_genre, type = "class")
conf_mat   <- table(Predicted = pred_genre, Actual = test_genre$track_genre)
test_acc   <- mean(pred_genre == test_genre$track_genre)

baseline_acc
conf_mat
test_acc

