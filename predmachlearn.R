glm.pred <- prediction(predicted1.prob, y.test)
ridge.pred <- prediction(predicted2.prob, y.test)
lasso.pred <- prediction(predicted3.prob, y.test)
en.pred <- prediction(predicted4.prob, y.test)
glm.ROC <- performance(glm.pred, measure = "tpr", x.measure = "fpr")
lasso.ROC <- performance(ridge.pred, measure = "tpr", x.measure = "fpr")
ridge.ROC <- performance(ridge.pred, measure = "tpr", x.measure = "fpr")
lasso.ROC <- performance(lasso.pred, measure = "tpr", x.measure = "fpr")
en.ROC <- performance(en.pred, measure = "tpr", x.measure = "fpr")
plot(glm.ROC)
plot(ridge.ROC, col = "red", add = TRUE)
plot(lasso.ROC, col = "green", add = TRUE)
plot(en.ROC, col = "blue", add = TRUE)
as.numeric(performance(glm.pred, measure = "auc")@y.values)
as.numeric(performance(ridge.pred, measure = "auc")@y.values)
as.numeric(performance(lasso.pred, measure = "auc")@y.values)
as.numeric(performance(en.pred, measure = "auc")@y.values)
glm.auc <- as.numeric(performance(glm.pred, measure = "auc")@y.values)
ridge.auc <- as.numeric(performance(ridge.pred, measure = "auc")@y.values)
lasso.auc <- as.numeric(performance(lasso.pred, measure = "auc")@y.values)
en.auc <- as.numeric(performance(en.pred, measure = "auc")@y.values)
alphas <- seq(0.01, 1, length = 9)
alphas
alphas <- seq(0.01, 1, length = 100)
alphas
alphas <- seq(0.0000001, 1, length = 100)
alphas
alphas <- seq(0.0000001, 1, length = 100)
#The elastic net to find optimal alpha
##k-fold cv
folds <- sample(1:10, size=length(y), replace=TRUE)
##initializing placeholder for the for-loop
cvm <- numeric()
lambda.min <- numeric()
set.seed(1)
for(i in seq(1, 100)){
  temp <- cv.glmnet(x, y, foldid=folds, alpha=alphas[i], lambda=lambdas, family = "binomial")
  cvm[i] <- min(temp$cvm)
  lambda.min[i] <- temp$lambda.min
}
alphas
cvm
heart <- read.csv(file="http://www-bcf.usc.edu/~gareth/ISL/Heart.csv", row.names=1)
summary(heart)
heart <- na.omit(heart)
# convert to factors
heart$Sex <- as.factor(heart$Sex)
heart$Fbs <- as.factor(heart$Fbs)
heart$RestECG <- as.factor(heart$RestECG)
heart$ExAng <- as.factor(heart$ExAng)
heart$Slope <- as.factor(heart$Slope)
heart$Ca <- as.factor(heart$Ca)
summary(heart)
N <- nrow(heart)
set.seed(456)
train.index <- sample(1:N, round(N/2))
test.index <- - train.index
x <- model.matrix(AHD ~ ., heart[train.index, ])[,-1]
y <- heart[train.index, "AHD"]
x.test <- model.matrix(AHD ~ ., heart[-train.index, ])[,-1]
y.test <- heart[-train.index, "AHD"]
library("MASS")
#Training data
fit1.all <- glm(AHD ~ ., data = heart[train.index,], family=binomial())
fit1.aic <- stepAIC(fit1.all, direction = "backward")
fit1.aic$anova #results
#Test data
predicted1.prob <- predict(fit1.aic, newdata = heart[test.index,], type = "response")
predicted1.prob <-ifelse(predicted1.prob > 0.5,"Yes","No")
table(predicted1.prob, y.test)
predicted1.pred <-ifelse(predicted1.prob > 0.5,"Yes","No")
table(predicted1.pred, y.test)
predicted1.prob <- predict(fit1.aic, newdata = heart[test.index,], type = "response")
predicted1.pred <-ifelse(predicted1.prob > 0.5,"Yes","No")
table(predicted1.pred, y.test)
predictionPower <- length(which(predicted1.pred == y.test))/length(y.test)
cat("Logistic Regression accuracy: ",predictionPower)
library(glmnet)
lambdas <- 10 ^ seq(6,-1,length=100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = lambdas, family = "binomial")
set.seed(1)
ridge.cv <- cv.glmnet(x, y, alpha = 0, lambda = lambdas, family = "binomial")
ridge.lam <- ridge.cv$lambda.min
ridge.lam
log(ridge.lam)
lambdas <- 10 ^ seq(6,-4,length=100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = lambdas, family = "binomial")
set.seed(1)
ridge.cv <- cv.glmnet(x, y, alpha = 0, lambda = lambdas, family = "binomial")
ridge.lam <- ridge.cv$lambda.min
ridge.lam
log(ridge.lam)
predicted2.prob <- predict(ridge.mod, s = ridge.lam, newx = x.test, type = "response")
predicted2.pred <- ifelse(predicted2.prob > 0.5, "Yes", "No")
table(predicted2.pred, y.test)
table(predicted2.pred, y.test)
ridge.accuracy <- length(which(predicted2.pred == y.test))/length(y.test)
cat("Ridge accuracy: ", ridge.accuracy)
lambdas <- 10 ^ seq(6,-4,length=100)
lasso.mod <- glmnet(x, y, alpha = 1, lambda = lambdas, family = "binomial")
set.seed(1)
lasso.cv <- cv.glmnet(x, y, alpha = 1, lambda = lambdas, family = "binomial")
lasso.lam <- lasso.cv$lambda.min
lasso.lam
log(lasso.lam)
predicted3.prob <- predict(lasso.mod, s = lasso.lam, newx = x.test, type = "response")
predicted3.pred <- ifelse(predicted3.prob > 0.5, "Yes", "No")
table(predicted3.pred, y.test)
lasso.accuracy <- length(which(predicted3.pred == y.test))/length(y.test)
cat("Lasso accuracy: ",lasso.accuracy)
alphas <- seq(0, 1, length = 20)
en.cv <- list()
for(i in 1:length(alphas)){
  lambdas<=10^seq(6,-3, length=100)
  set.seed(100)
  folds<-sample(1:10,size=length(y),replace=TRUE)
  en.cv[[i]]<-cv.glmnet(x,y,foldid = folds,lambda=lambdas,alpha=alphas[i],family="binomial")
}
plot(log(en.cv[[1]]$lambda), en.cv[[1]]$cvm,col="red", xlab="Log(Lambda)", ylab=en.cv[[1]]$name, type="l", xlim=c(-7,0), ylim=c(0.75,1))
for(i in 2:length(alphas)){
  lines(log(en.cv[[i]]$lambda),en.cv[[i]]$cvm,col="green")
}
minimal_cvm <- rep(NULL,length(alphas))
for(i in 1:length(alphas)){
  minimal_cvm[i] <- min(en.cv[[i]]$cvm)
}
best_iteration <- which(minimal_cvm == min(minimal_cvm))
points(log(en.cv[[best_iteration]]$lambda.min), min(en.cv[[best_iteration]]$cvm), cex=3)
cat("Optimal alpha: ", alphas[best_iteration])
en.lam <- en.cv[[best_iteration]]$lambda.min
en.lam
en.mod<-glmnet(x,y,alpha=alphas[best_iteration],lambda=en.cv[[best_iteration]]$lambda.min,family="binomial")
predicted4.prob <- predict(en.mod, newx = x.test, type = "response")
predicted4.pred <- ifelse(predicted4.prob > 0.5, "Yes", "No")
table(predicted4.pred, y.test)
en.accuracy <- length(which(predicted4.pred == y.test))/length(y.test)
cat("Elastic Net accuracy: ", en.accuracy)
0.5 * (3.6 - 91/29) * 67/29
fractions(0.5 * (3.6 - 91/29) * 67/29)
library(MASS)
fractions(0.5 * (3.6 - 91/29) * 67/29)
0.5 * (5 - 91/29) * 108/29
fractions(18 - 5 * 91/29)
data("ChickWeight")
library(reshape2)
head(ChickWeight)
wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight")
head(wideCW)
?dcast
library(caret)
install.packages("caret")
library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y = spam$type, p = 0.75, list=FALSE)
inTrain
head(spam)
head(inTrain)
str(inTrain)
training <- spam[inTrain,]
test <- spam[-inTrain,]
set.seed(123)
modelFit <- train(type ~ ., data = training, method = "glm")
install.packages("e1071")
modelFit <- train(type ~ ., data = training, method = "glm")
modelFit
modelFit$finalModel
predictions <- predict(modelFit, newdata=test)
predictions
folds <- createFolds(y = spam$type, k=10, list=TRUE, returnTrain = TRUE)
sapply(folds, length)
library(caret)
args(trainControl)
?train.default
?trainControl
library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y = spam$type, p = 0.75, list=FALSE)
training <- spam[inTrain,]
test <- spam[-inTrain,]
preObj <- preProcess(training[, -58], method = c("center", "scale"))
trainCapAve <- predict(preObj, training[,-58])$capitalAve
mean(trainCapAve)
sd(trainCapAve)
library(caret)
library(kernlab)
data(spam)
library(ISLR)
data(Wage)
?inTrain
?createPartition
?createDataPartition
inTrain <- createPartition(Wage$wage, p = 0.75, list = FALSE)
inTrain <- createDataPartition(Wage$wage, p = 0.75, list = FALSE)
inTrain
head(inTrain)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass, data = training)
head(predict(dummies, newdata=training))
dummies
nsv <- nearZeroVar(training, saveMetrics = TRUE)
?nearZeroVar
library(splines)
bsBasis <- bs(training$age, df = 3)
head(bsBasis)
lm1 <- lm(wage ~ bsBasis, data=training)
plot(training$age, training$wage, data = training)
plot(training$age, training$wage, pch = 19)
plot(training$age, training$wage, pch = 19, cex = 0.5)
plot(training$age, predict(lm1, newdata=training), type = "l", col = "red")
plot(training$age, training$wage, pch = 19, cex = 0.5)
plot(training$age, predict(lm1, newdata=training), type = "l", col = "red", add = TRUE)
lines(training$age, predict(lm1, newdata=training), col = "red", add = TRUE)
plot(training$age, training$wage, pch = 19, cex = 0.5)
lines(training$age, predict(lm1, newdata=training), col = "red", add = TRUE)
plot(training$age, training$wage, pch = 19, cex = 0.5)
lines(training$age, predict(lm1, newdata=training), col = "red")
plot(training$age, training$wage, pch = 19, cex = 0.5)
points(training$age, predict(lm1, newdata=training), col = "red")
predict(lm1, newdata = training)
library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y = spam$type, p = 0.75, list=FALSE)
training <- spam[inTrain,]
test <- spam[-inTrain,]
M <- abs(cor(training[,-58]))
diag(M) <- 0
which(M > 0, arr.ind = T)
which(M > 0.8, arr.ind = T)
?which
M''
M
head(M)
which(M > 0.8)
typeColor <- (spam$type==1) * 1 + 1
prComp <- prcomp(log10(spam[, -58]) + 1)
prComp <- prcomp(log10(spam[, -58] + 1))
plot(prComp$x[, 1], prComp$x[, 2], col = typeColor, xlab="PC1", ylab = "PC2")
typeColor <- (spam$type=="spam") * 1 + 1
prComp <- prcomp(log10(spam[, -58] + 1))
plot(prComp$x[, 1], prComp$x[, 2], col = typeColor, xlab="PC1", ylab = "PC2")
preProc <- preProcess(log10(spam[,-58] + 1), method = "pca", pcaComp=2)
trainPC <- predict(preProc, log10(spam[,-58] + 1))
trainPC
head(trainPC)
str(trainPC)
modelFit <- train(training$type ~ ., method= "glm", data=trainPC)
modelFit <- train(training$type ~ ., method= "glm", data=trainPC)
preProc <- preProcess(log10(training[,-58] + 1), method = "pca", pcaComp=2)
trainPC <- predict(preProc, log10(training[,-58] + 1))
modelFit <- train(training$type ~ ., method= "glm", data=trainPC)
testPC <- predict(preProc, log10(test[,-58] + 1))
confusionMatrix(testing$type, testPC)
confusionMatrix(test$type, testPC)
confusionMatrix(test$type, predict(modelFit, testPC))
library(caret)
data(faithful)
set.seed(333)
inTrain <- createDataPartition(faithful, p = 0.75, list=TRUE)
set.seed(333)
inTrain <- createDataPartition(faithful, p = 0.75, list=FALSE)
set.seed(333)
head(faithful)
inTrain <- createDataPartition(y = faithful$waiting, p = 0.5, list=FALSE)
?createDataPartition
trainFaith <- faithful[inTrain,]
testFaith <- faithful[-inTrain,]
head(trainFaith)
plot(trainFaith$waiting, trainFaith$eruptions, pch=19)
plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col = "blue")
lm1 <- lm(eruptions ~ waiting, data=trainFaith)
summary(lm1)
lines(lm1)
lines(lm1, data=trainFaith)
abline(lm1)
abline(lm1, lwd=3)
summary(lm1)
pred1 <- predict(lm1, newdata=testFaith, interval="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting, testFaith$eruptions, pch=19, color = "blue")
plot(testFaith$waiting, testFaith$eruptions, pch=19, col = "blue")
matlines(testFaith$waiting[ord], pred1[ord], col = c(1, 2, 2), lwd=3)
matlines(testFaith$waiting[ord], pred1[ord], col = c(1, 2, 2), lty = c(1,1,1),lwd=3)
matlines(testFaith$waiting[ord], pred1[ord,], col = c(1, 2, 2), lty = c(1,1,1),lwd=3)
pred1
head(pred1)
?matlines
?order
ord
plot(testFaith$waiting, testFaith$eruptions, pch=19, col = "blue")
matlines(testFaith$waiting, pred1, col = c(1, 2, 2), lty = c(1,1,1),lwd=3)
modFit <- train(eruptions ~ waiting, data = trainFaith)
modFit <- train(eruptions ~ waiting, data = trainFaith, method = "lm")
summary(modFit)
library(caret)
library(ISLR)
library(ggplot2)
data(Wage)
head(Wage)
Wage <- subset(Wage, select=-c(logwage))
inTrain <- createDataPartition(Wage$wage, p = 0.75, list = FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
modFit <- train(wage ~ age + jobclass + education, method = "lm", data = training)
print(modFit)
finMod <- modFit$finalModel
finMod
str(Wage$education)
plot(finMod, 1, pch=19)
plot(finMod, 1, pch=19, cex=0.5)
library(AppliedPredictiveModeling)
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data(concrete)
head(concrete)
summary(concrete$Superplasticizer)
hist(concrete$Superplasticizer)
hist(log(concrete$Superplasticizer + 1))
library(caret)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
names(training)
grep(names(training), "IL")
?grep
grep("IL", names(training))
?preProcess
preProcess(training[,grep("IL", names(training))], method = "pca")
length(training[,grep("IL", names(training))])
preProcess(training[,grep("IL", names(training))], method = "pca", thresh = 0.8)
IL <- training[,grep("IL", names(training))
preProcess(training[,IL], method = "pca", thresh = 0.8)
IL
IL <- training[,grep("IL", names(training))
IL <- training[,grep("IL", names(training))]
preProcess(training[,IL], method = "pca", thresh = 0.8)
IL <- training[,grep("IL", names(training))]
preProcess(training[,IL], method = "pca", thresh = 0.8)
IL
preProcess(IL, method = "pca", thresh = 0.8)
IL_diagnosis <- cbind(IL, training$diagnosis)
newtraining <- cbind(IL, training$diagnosis)
head(adData)
train(diagnosis ~ ., data=newtraining, method = "glm")
str(newtraining)
train(training$diagnosis ~ ., data=newtraining, method = "glm")
modFit1 <- train(training$diagnosis ~ ., data=newtraining, method = "glm")
predict(modFit1, newdata=testing)
newtraining <- data.frame(IL, diagnosis = training$diagnosis)
head(newtraining)
modFit1 <- train(diagnosis ~ ., data=newtraining, method = "glm")
predict(modFit1, newdata=testing)
predictions <- predict(modFit1, newdata=testing)
sum(predictions == testing$diagnosis)/length(predictions)
?train
preProc <- preProcess(IL, method = "pca", thresh = 0.8)
trainPCA <- predict(preProc, training)
modFit2 <- train(diagnosis ~ ., data=trainPCA, method = "glm")
testPCA <- predict(preProc, testing)
confusionMatrix(testing$diagnosis, predict(modFit2, testPCA))
confusionMatrix(testing$diagnosis, predict(modFit1, testing))
str(newtraining)
confusionMatrix(testing$diagnosis, predict(modFit1, testing))
head(IL)
?grep
IL <- training[,grep("^IL", names(training))]
head(IL)
newtraining <- data.frame(IL, diagnosis = training$diagnosis)
modFit1 <- train(diagnosis ~ ., data=newtraining, method = "glm")
confusionMatrix(testing$diagnosis, predict(modFit1, testing))
trainPCA <- predict(preProc, training)
preProc <- preProcess(IL, method = "pca", thresh = 0.8)
trainPCA <- predict(preProc, training)
trainPCA <- predict(preProc, newtraining)
modFit2 <- train(diagnosis ~ ., data=trainPCA, method = "glm")
testPCA <- predict(preProc, testing)
confusionMatrix(testing$diagnosis, predict(modFit2, testPCA))
library(caret)
?trainControl
training <- read.csv("pml-training.csv")
training <- read.csv("pml-training.csv")
setwd("predmachlearn/")
training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")
dim(training)
training <- na.omit(training)
yaw_index <- grep("yaw", names(training))
library(caret)
modFit <- train(classe ~ ., data=training[, -yaw_index], method="rf")
?randomForest
randomForest(classe ~ ., data=training[, -yaw_index])
str(training)
View(training)
?read.csv
training <- read.csv("pml-training.csv", na.strings=c("NA", "DIV/0!"))
testing <- read.csv("pml-testing.csv", na.strings=c("NA", "DIV/0!"))
dim(training)
training <- na.omit(training)
str(training)
training <- read.csv("pml-training.csv", na.strings=c("NA", "#DIV/0!"))
testing <- read.csv("pml-testing.csv", na.strings=c("NA", "#DIV/0!"))
dim(training)
training <- na.omit(training)
str(training)
train(classe ~ ., data=training, method="rf")
training <- read.csv("pml-training.csv", na.strings=c("NA", "#DIV/0!"))
testing <- read.csv("pml-testing.csv", na.strings=c("NA", "#DIV/0!"))
dim(training)
training <- na.omit(training)
training <- read.csv("pml-training.csv", na.strings=c("NA", "#DIV/0!"))
testing <- read.csv("pml-testing.csv", na.strings=c("NA", "#DIV/0!"))
dim(training)
train(classe ~ ., data=training, method="rf")
inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
myTrain <- training[inTrain,]
myCV <- training[-inTrain]
myCV <- training[-inTrain,]
dim(myTrain)
dim(myCV)
?nsv
?nearZeroVar
nzv <- nearZeroVar(myTrain)
nzv
nzv <- nearZeroVar(myTrain, saveMetrics = TRUE)
nzv
nzv <- nearZeroVar(myTrain)
train(classe ~ ., data=training[,-nzv], method="rf")
modFit <- train(classe ~ ., data=myTrain[,-nzv], method="rf")
modFit
predict(modFit, newdata=myCV)
pred <- predict(modFit, newdata=myCV)
length(pred)
pred <- predict(modFit, myCV)
length(pred)
pred <- predict(modFit, newdata=myCV[,-nzv])
length(pred)
table(myCV$classe, pred)
table(myCV$classe)
table(pred)
nzv
modFit
myTrain <- myTrain[, -nzv]
myTrain[, 1]
head(myTrain[, 1])
View(myTrain)
myTrain <- myTrain[, -1]
modFit <- train(classe ~ ., data=myTrain, method="rf")
pred <- predict(modFit, newdata=myCV)
dim(pred)
length(pred)
dim(myCV)
pred <- predict(modFit, newdata=myCV[1:10,])
head(myCV)
head(myTrain)
head(myCV)
pred <- predict(modFit, newdata=myCV[1:10,])
myCV[1:10,]
pred <- predict(modFit, newdata=myCV[1:10,])
head(myTrain)
dim(myTrain)
str(myTrain)
nearZeroVar(myTrain, saveMetrics=TRUE)
summary(myTrain)
args(train)
args(train.default)
trainContro()
trainControl()
args(trainControl)
?train
?randomForest
?na.action
?na.exclude
modFit <- train(classe ~ ., data=myTrain, method="rf", na.action=na.exclude)
pred <- predict(modFit, newdata=myCV)
length(pred)
modFit
predict(modFit, myCV)
modFit <- train(classe ~ ., data=myTrain, method="rf", na.action=na.pass)
modFit <- train(classe ~ ., data=myTrain, method="rf", na.action=na.exclude)
pred <- predict(modFit, newdata=myCV, na.action=na.exclude)
length(pred)
pred <- predict(modFit, newdata=myCV, na.action=na.pass)
pred <- predict(modFit, newdata=myCV, na.action=na.exclude)
length(pred)
modFit <- train(classe ~ ., data=myTrain, method="rf", na.action=na.exclude())
?preProcess
head(myTrain[,160])
head(myTrain[,130])
preObj <- preProcess(myTrain[,-130], method="knnImpute")
myTrain <- predict(preObj, myTrain)
myTrain <- predict(preObj, myTrain[,-130])
install.packages("RANN")
preObj <- preProcess(myTrain, method="knnImpute")
myTrain <- predict(preObj, myTrain)
head(myTrain)
myTrain <- predict(preObj, myTrain)
summary(myTrain)
myTrain <- predict(preObj, myTrain)
predict(preObj, myTrain)
preObj <- preProcess(myTrain[,-130], method="knnImpute")
predict(preObj, myTrain)
predict(preObj, myTrain[,-130])
head(myTrain)
preObj
summary(preObj)
