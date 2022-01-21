## LIBRARIES
library(MASS)
library(glmnet)
library(pROC)
library(fields)
library(caret)
library(e1071)
library(tree)


## READ DATA
rm(list=ls())
setwd("/Users/kayvoncoffey/Desktop/1 Academics/CU Boulder/5 F21/MSBX 5415/project")
df <- read.csv(paste(getwd(),"/data/top100all_clean.csv",sep=""))

## SPLIT DATA
set.seed(123)
train <- sample(c(TRUE,FALSE),dim(df)[1], replace=TRUE, prob=c(0.7,0.3))
df_train <- df[train,]
df_test <- df[!train,]


## Classifiers
y <- df_test[,"topX"]

# logistic regression
fit.logit <- glm(topX ~ popularity+danceability+energy+valence+acousticness+
                   duration_ms+instrumentalness+key+liveness+loudness+mode+
                   speechiness+tempo+time_signature,data=df_train,family="binomial")
summary(fit.logit)

logit.probs <- predict(fit.logit, newdata=df_test, type="response")
logit.preds <- ifelse(logit.probs>0.5,1,0)

roc.logit <- roc(y, logit.probs)
plot(roc.logit, main="Comparison of ROC Curves")

best.logit <- roc.logit$thresholds[which.max(roc.logit$sensitivities - roc.logit$specificities)] 

mean(logit.preds == y)          # accuracy 
mean(logit.preds[y==1])         # sensitivity 
mean(logit.preds[y==0])         # specificity 

# LDA
fit.lda <- lda(topX ~ popularity+danceability+energy+valence+acousticness+
                 duration_ms+instrumentalness+key+liveness+loudness+mode+
                 speechiness+tempo+time_signature,data=df_train)
lda.probs <- predict(fit.lda, newdata=df_test)$posterior
temp <- predict(fit.lda, newdata=df_test)$class
lda.preds <- as.numeric(levels(temp))[temp]

roc.lda <- roc(y, lda.probs[,1])
lines(roc.lda, col="red")

mean(lda.preds == y)           # accuracy 
mean(lda.preds[y==1])          # sensitivity
mean(lda.preds[y==0])          # specificity

# QDA - some groups too small for qda?
fit.qda <- qda(topX ~ popularity+danceability+energy+valence+acousticness+
                 duration_ms+instrumentalness+key+liveness+loudness+mode+
                 speechiness+tempo+time_signature,data=df_train)

# KNN
fit.knn <- knnreg(topX ~ popularity+danceability+energy+valence+acousticness+
                    duration_ms+instrumentalness+key+liveness+loudness+mode+
                    speechiness+tempo+time_signature,data=df_train, k=5)

knn.preds <- predict(fit.knn, newdata=df_test)

mean(knn.preds == y)           # K=5 did terrible 
mean(knn.preds[y==1])          # K=1 sensitivity
mean(knn.preds[y==0])          # K=1 specificity

# SVM - linear kernel
dat <- df_train; dat$topX <- as.factor(df_train$topX)
dat.test <- df_test;dat.test$topX <- as.factor(df_test$topX)
fit.svm_lin <- svm(topX ~ popularity+danceability+energy+valence+acousticness+
                     duration_ms+instrumentalness+key+liveness+loudness+mode+
                     speechiness+tempo+time_signature,data=dat, kernel="linear", cost=1)

tune.out <- tune(svm, topX ~ popularity+danceability+energy+valence+acousticness+
                   duration_ms+instrumentalness+key+liveness+loudness+mode+
                   speechiness+tempo+time_signature,data=dat, kernel="linear", 
                   cost=seq(0.01,5,length.out=100))

fit.svm_lin.best <- tune.out$best.model
temp <- predict(fit.svm_lin.best, newdata=dat.test)
svm_lin.preds <- as.numeric(levels(temp))[temp]


mean(svm_lin.preds == y)        # accuracy
mean(svm_lin.preds[y==1])       # sensitivity
mean(svm_lin.preds[y==0])       # specificity

# SVM - polynomial kernel
fit.svm_poly <- svm(topX ~ popularity+danceability+energy+valence+acousticness+
                      duration_ms+instrumentalness+key+liveness+loudness+mode+
                      speechiness+tempo+time_signature,data=dat, kernel="polynomial",
                    degree = 3, cost=1)
tune.out <- tune(svm,topX ~ popularity+danceability+energy+valence+acousticness+
                   duration_ms+instrumentalness+key+liveness+loudness+mode+
                   speechiness+tempo+time_signature,data=dat, kernel="polynomial",
                 degree = 3, cost=seq(0.01,5,length.out=100))
fit.svm_poly.best <- tune.out$best.model
temp <- predict(fit.svm_poly.best, newdata=dat.test)
svm_poly.preds <- as.numeric(levels(temp))[temp]

mean(svm_poly.preds == y)      # accuracy 83% but still getting all 0's
mean(svm_poly.preds[y==1])     # sensitivity 0%
mean(svm_poly.preds[y==0])     # specificity 0%

# SVM - radial
fit.svm_radial <- svm(topX ~ popularity+danceability+energy+valence+acousticness+
                      duration_ms+instrumentalness+key+liveness+loudness+mode+
                      speechiness+tempo+time_signature,data=dat, kernel="radial",
                      cost=1)
tune.out <- tune(svm,topX ~ popularity+danceability+energy+valence+acousticness+
                   duration_ms+instrumentalness+key+liveness+loudness+mode+
                   speechiness+tempo+time_signature,data=dat, kernel="radial",
                 cost=seq(0.01,5,length.out=100))
fit.svm_radial.best <- tune.out$best.model
temp <- predict(fit.svm_radial.best, newdata=dat.test)
svm_radial.preds <- as.numeric(levels(temp))[temp]

mean(svm_radial.preds == y)      # accuracy 83% but still getting all 0's
mean(svm_radial.preds[y==1])     # sensitivity 0%
mean(svm_radial.preds[y==0])     # specificity 0%

# TREE
fit.tree <- tree(topX ~ popularity+danceability+energy+valence+acousticness+
                   duration_ms+instrumentalness+key+liveness+loudness+mode+
                   speechiness+tempo+time_signature, data=dat)
plot(fit.tree)
text(fit.tree, cex=0.5)

tree.probs <- predict(fit.tree, newdata=dat.test)
temp <- predict(fit.tree, newdata=df_test, type="class")
tree.preds <- as.numeric(levels(temp))[temp]

roc.tree <- roc(y, tree.probs[,2])
plot(roc.logit, main="Comparison of ROC Curves")
lines(roc.lda, col="red")
lines(roc.tree, col="green")

mean(tree.preds == y)          # accuracy
mean(tree.preds[y==1])         # sensitivity
mean(tree.preds[y==0])         # specificity

# output plot
jpeg(paste(getwd(),"/output/1yr roc curves.jpeg",sep=""))
plot(roc.logit, main="Comparison of ROC Curves")
lines(roc.lda, col="red")
lines(roc.tree, col="green")
legend(x=0.21,y=0.1,legend=c(paste("Logit  ",round(roc.logit$auc,3),sep=""),
                          paste("LDA ",round(roc.lda$auc,3),sep=""),
                          paste("Tree ",round(roc.tree$auc,3),sep="")),
        col=c("black","red","green"),lty=1,cex=0.8)
dev.off()

jpeg(paste(getwd(),"/output/1yr tree.jpeg",sep=""))
plot(fit.tree)
text(fit.tree, pretty = 0)
title(main="Preliminary Tree Classifier for Top 50")
dev.off()
