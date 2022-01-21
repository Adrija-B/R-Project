library(glmnet)
library(caret)

## READ DATA
rm(list=ls())
setwd("/Users/ciro/Desktop/Spotify Project/")
df <- read.csv(paste(getwd(),"/otherData/top100all_clean2.csv",sep=""))
df <- subset(df, select = -c(top50, top50_min5))

mean(df$avg.streams[df$top50 == 0]) / 1000 * 4


## SPLIT DATA
set.seed(123)
train <- sample(c(TRUE,FALSE),dim(df)[1], replace=TRUE, prob=c(0.7,0.3))
df_train <- df[train,]
df_test <- df[!train,]


lm1 <- lm(max.popularity ~ acousticness + danceability + duration_ms + energy + 
            instrumentalness + as.factor(key) + liveness + loudness + mode + speechiness + 
            tempo + time_signature + valence, data = df_train); summary(lm1)


sqrt(mean((predict(lm1, newdata = df_test) - df_test$max.popularity)^2))
sqrt(mean((mean(df_test$max.popularity) - df_test$max.popularity)^2))

lm2 <- lm(num.t200 ~ acousticness + danceability + duration_ms + energy + 
            instrumentalness + as.factor(key) + liveness + loudness + mode + speechiness + 
            tempo + time_signature + valence, data = df_train); summary(lm2)

sqrt(mean((predict(lm2, newdata = df_test) - df_test$num.t200)^2))
sqrt(mean((mean(df_test$num.t200) - df_test$num.t200)^2))

lm3 <- lm(num.t50 ~ acousticness + danceability + duration_ms + energy + 
            instrumentalness + as.factor(key) + liveness + loudness + mode + speechiness + 
            tempo + time_signature + valence, data = df_train); summary(lm3)

sqrt(mean((predict(lm3, newdata = df_test) - df_test$num.t50)^2))
sqrt(mean((mean(df_test$num.t50) - df_test$num.t50)^2))


#Poisson
glm1 <- glm(max.popularity ~ acousticness + danceability + duration_ms + energy + 
              instrumentalness + as.factor(key) + liveness + loudness + mode + speechiness + 
              tempo + time_signature + valence, data = df_train, family = 'poisson'); summary(glm2)
sqrt(mean((predict(glm1, type='response', newdata = df_test) - df_test$max.popularity)^2))

glm2 <- glm(num.t200 ~ acousticness + danceability + duration_ms + energy + 
            instrumentalness + as.factor(key) + liveness + loudness + mode + speechiness + 
            tempo + time_signature + valence, data = df_train, family = 'poisson'); summary(glm2)


sqrt(mean((predict(glm2, type='response', newdata = df_test) - df_test$num.t200)^2))
sqrt(mean((sample(min(df_test$num.t200):max(df_test$num.t200), nrow(df_test), replace = T) - df_test$num.t200)^2))


glm3 <- glm(num.t50 ~ acousticness + danceability + duration_ms + energy + 
            instrumentalness + as.factor(key) + liveness + loudness + mode + speechiness + 
            tempo + time_signature + valence, data = df_train, family = 'poisson'); summary(glm3)

summary(glm3)

sqrt(mean((predict(glm3, type = 'response', newdata = df_test) - df_test$num.t50)^2))
sqrt(mean((sample(min(df_test$num.t50):max(df_test$num.t50), nrow(df_test), replace = T) - df_test$num.t50)^2))

#Lasso/Ridge/Elasticnet (change the alpa variable)
library(glmnet)
x <- data.matrix(df_train[, c('danceability', 'duration_ms', 'energy', 
                  'instrumentalness', 'liveness', 'mode', 'speechiness', 
                  'tempo', 'time_signature', 'valence', 'key')])
y <- df_train$max.popularity
ALPHA <- 1
cv_model <- cv.glmnet(x,y, alpha = ALPHA)

best_lambda <- cv_model$lambda.min; best_lambda
plot(cv_model)

best_model <- glmnet(x,y, alpha = ALPHA, lambda = best_lambda)
summary(best_model)

#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = data.matrix(df_train[, c('danceability', 'duration_ms', 'energy', 
                                                                                   'instrumentalness', 'liveness', 'mode', 'speechiness', 
                                                                                   'tempo', 'time_signature', 'valence', 'key')]))
#RMSE
sqrt(mean((y_predicted - df_train$max.popularity)^2))

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - df_train$max.popularity)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq


#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = data.matrix(df_test[, c('danceability', 'duration_ms', 'energy', 
                                                                                    'instrumentalness', 'liveness', 'mode', 'speechiness', 
                                                                                    'tempo', 'time_signature', 'valence', 'key')]))
#RMSE
sqrt(mean((y_predicted - df_test$num.t50)^2))


#KNN
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

df_train.n <- as.data.frame(lapply(df_train[, c('acousticness', 'danceability', 'duration_ms', 'energy', 
                                               'instrumentalness', 'key', 'liveness', 'loudness', 'mode', 'speechiness', 
                                               'tempo', 'time_signature', 'valence')], normalize)); df_train.n$key <- as.factor(df_train$key)

df_test.n <- as.data.frame(lapply(df_test[, c('acousticness', 'danceability', 'duration_ms', 'energy', 
                                                  'instrumentalness', 'key', 'liveness', 'loudness', 'mode', 'speechiness', 
                                                  'tempo', 'time_signature', 'valence')], normalize));  df_test.n$key <- as.factor(df_test$key)

df_train.n$Y <- df_train$max.popularity; df_test.n$Y <- df_test$max.popularity


knn1 <- knnreg(Y ~ acousticness + danceability + duration_ms + energy + 
                 instrumentalness + key + liveness + loudness + mode + speechiness + 
                 tempo + time_signature + valence, data = df_train.n, k=100)

sqrt(mean((predict(knn1, newdata = df_test.n) - df_test.n$Y)^2))


