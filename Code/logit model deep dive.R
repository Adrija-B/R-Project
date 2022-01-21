## LIBRARIES
library(MASS)
library(glmnet)
library(pROC)
library(leaps)
library(bestglm)


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


##### Logistic Model Selection - Best Subsets (Exhaustive, AIC)
fit.logit_full <- glm(topX ~ popularity+danceability+energy+valence+acousticness+
                   duration_ms+instrumentalness+key+liveness+loudness+mode+
                   speechiness+tempo+time_signature,data=df_train,family="binomial")
logit_full.probs <- predict(fit.logit_full,newdata=df_test,type="response") 

Xy <- df_train[,c(4:16,18)]
Xy$y <- df_train[,"topX"]

fit.logit_bestglm <- bestglm(Xy=Xy,family=binomial,IC="AIC")
roc.logit_full <- roc(y, logit_full.probs)
brier.logit_full <- mean((y - logit_full.probs)^2)


logit_bestglm.probs <- predict(fit.logit_bestglm$BestModel,newdata=df_test,
                               type="response")
roc.logit_bestglm <- roc(y,logit_bestglm.probs)
brier.logit_bestglm <- mean((y - logit_bestglm.probs)^2)

##### Logistic Model Selection - GLM Net Shrinkage
x_elnet <- as.matrix(df_train[,c(4:16,18)])
y_elnet <- as.matrix(df_train[,"topX"])
x_elnet_test <- as.matrix(df_test[,c(4:16,18)])
y_elnet_test <- df_test[,"topX"]

fit.logit_ridge <- glmnet(x_elnet,y_elnet,family="binomial",
                          lambda=seq(0.01,1,length.out=1000),alpha=0)  #logit ridge
fit.logit_lasso <- glmnet(x_elnet, y_elnet,family="binomial",
                          lambda=seq(0.01,1,length.out=1000),alpha=1) #logit lasso

cv.out_ridge <- cv.glmnet(x_elnet,y_elnet,faimly="binomial",
                          lambda=seq(0.01,1,length.out=1000),alpha=0)
cv.out_lasso <- cv.glmnet(x_elnet,y_elnet,family="binomial",
                          lambda=seq(0.01,1,length.out=1000),alpha=0)


plot(cv.out_ridge)
plot(cv.out_lasso)
cv.ridge_lambda <- cv.out_ridge$lambda.min
cv.lasso_lambda <- cv.out_lasso$lambda.min

# Compare Full and Ridge Shrinkage for Logit Model
logit_ridge.probs <- as.vector(predict(fit.logit_ridge,s=cv.ridge_lambda,
                                       newx=x_elnet_test,type="response"))
logit_lasso.probs <- as.vector(predict(fit.logit_lasso,s=cv.lasso_lambda,
                                       newx=x_elnet_test,type="response"))

roc.logit_ridge <- roc(y_elnet_test, logit_ridge.probs)
roc.logit_lasso <- roc(y_elnet_test, logit_lasso.probs)

dev.new();plot(roc.logit_full,main="Logistic Model Selection - ROC Curves")
lines(roc.logit_bestglm,col="red")
lines(roc.logit_ridge,col="blue")
lines(roc.logit_lasso,col="green")

brier.logit_ridge <- mean((y_elnet_test - logit_ridge.probs)^2)
brier.logit_lasso <- mean((y_elnet_test - logit_lasso.probs)^2)

comp <- cbind(brier.logit_full,brier.logit_bestglm,brier.logit_ridge,brier.logit_lasso)
comp <- rbind(comp,cbind(roc.logit_full$auc,roc.logit_bestglm$auc,
                         roc.logit_ridge$auc,roc.logit_lasso$auc))
rownames(comp) <- c("Brier Score","AUC")
colnames(comp) <- c("Full Logit","Best GLM Logit","Ridge Logit","Lasso Logit")
comp

logit_full.preds <- ifelse(logit_full.probs > 0.5,1,0)
table(predicted=logit_full.preds,observed=y)

logit_bestglm.preds <- ifelse(logit_bestglm.probs > 0.5,1,0)
table(predcited=logit_bestglm.preds, observed=y)

####### output
leg <- c(NA,"Brier","AUC","Full",comp[,1],"Best Sub",comp[,2],
         "Ridge",comp[,3],"Lasso",comp[,4])
pdf(paste(getwd(),"/output/Logit Model Selection roc curves.pdf",sep=""))
plot(roc.logit_full,main="Logistic Model Selection - ROC Curves")
legend(x=1.0,y=1.0,legend=leg,ncol=5,cex=0.8)
lines(roc.logit_bestglm,col="red")
lines(roc.logit_ridge,col="blue")
lines(roc.logit_lasso,col="green")
legend(x=0.25,y=0.15,legend=c("Full Logit","Best Subset","Ridge CV Opt","Lasso CV Opt"),
       col=c("black","red","blue","green"),lty=1,cex=0.8)

dev.off()
