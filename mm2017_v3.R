#########################################
# Next Steps
#   cross validate each model
#   export train and test predictions with y output
#   test nn build on predictions vs. simple avg
#########################################

#########################################
# Step 1 - import libraries and stuff
#########################################
rm(list = ls())

library(lattice)
library(ggplot2)
library(reshape2)
library(MASS)
library(DMwR)
library(randomForest)
library(boot)
library(caret)
library(lmtest)
library(hydroGOF)
library(neuralnet)
library(xgboost)
library(Matrix)
library(readr)
library(stringr)
library(caret)
library(car)
library(plyr)
library(pls)
library(LogicReg)
library(dplyr)

# set wd and load file

setwd('/Users/ntmill/OneDrive/Data/March Madness/2017')
train.orig <- as.data.frame(read.csv('training_export.csv'))
train.orig <- na.omit(train.orig)
test.final <- as.data.frame(read.csv('submission_test.csv'))

# normalize the data we'll use to build the models
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
normalize.rev <- function(x) {
  return ((x - max(x, na.rm = TRUE)) / (min(x, na.rm = TRUE) - max(x, na.rm = TRUE)))
}
norm_var <- c('team1_win','team1_pyth','team1_adjusto','team1_adjustt','team1_luck','team1_sospyth','team1_sosoppo',
              'team2_win','team2_pyth','team2_adjusto','team2_adjustt','team2_luck','team2_sospyth','team2_sosoppo')
rev_var <- c('team1_loss','team1_adjustd','team1_seed','team2_loss','team2_adjustd','team2_seed')


train.norm <- as.data.frame(lapply(train.orig[,norm_var],normalize))
train.norm.rev <- as.data.frame(lapply(train.orig[,rev_var],normalize))
train.full <- cbind(train.orig[,1:10],train.norm,train.norm.rev)
train.full$team1_victory <- train.orig[,31]
train.full <- na.omit(train.full)
rm(train.norm, train.norm.rev)
vars <- colnames(train.full[11:ncol(train.full)])

# create test and training sets
sample_size <- floor(0.6666 * nrow(train.full))
set.seed(1234)
train_ind <- sample(seq_len(nrow(train.full)), size = sample_size)
train <- as.data.frame(train.full[train_ind,])
test <- as.data.frame(train.full[-train_ind,])

train.no.t <- as.data.frame(train.orig[train_ind,])
test.no.t <- as.data.frame(train.orig[-train_ind,])

#########################################
# Step 2 - PCA
#########################################

# principal components.  don't include response variable
pca <- princomp(train.full[,vars], cor = T)
biplot(pca)
barplot(pca$loadings)
barplot(pca$loadings[,1])

pcr.model <- pcr(train.full$team1_victory ~., data = train.full[,11:ncol(train.full)], scale = TRUE, validation = "CV")
summary(pcr.model)
summary(pcr.model$loadings)
validationplot(pcr.model)

#########################################
# Step 3 - logistic regression
#########################################

glm <- glm(team1_victory ~ ., data = train.no.t[,vars], family = binomial)
glm.null <- glm(team1_victory ~ 1, data = train.no.t[,vars], family = binomial)
glm.step <- step(glm)

anova(glm.null, glm, test = "Chi")
anova(glm.null, glm.step, test = "Chi")
anova(glm, glm.step, test = "Chi")

glm.pred <- predict(glm, newdata = test.no.t, type = "response")
glm.step.pred <- predict(glm.step, newdata = test.no.t, type = "response")
glm.null.pred <- predict(glm.null, newdata = test.no.t, type = "response")
glm.pca.pred <- as.data.frame(predict(pcr.model, newdata = test.no.t, type = "response", ncomp = 15))

rmse(glm.pred, test$team1_victory)
rmse(glm.step.pred, test$team1_victory)
rmse(glm.null.pred, test$team1_victory)
rmse(glm.pca.pred, test$team1_victory)

cv.train <- trainControl(method = "cv", number = 5, savePredictions = TRUE)
glm.cv <- train(glm$formula, data = train.no.t[,11:ncol(train.no.t)], trControl = cv.train, method = "glm", family = 'binomial')
glm.cv.step <- train(glm.step$formula, data = train.no.t[,11:ncol(train.no.t)], trControl = cv.train, method = "glm", family = 'binomial')
glm.cv.null <- train(glm.null$formula, data = train.no.t[,11:ncol(train.no.t)], trControl = cv.train, method = "glm", family = 'binomial')

glm.cv.null
glm.cv
glm.cv.step

#########################################
# Step 4 - random forest
#########################################

train.rf <- randomForest(team1_victory ~., data = train[vars], ntree = 500, type = 'classification')       
plot(train.rf, main = "Train Random Forest")  
varImpPlot(train.rf, main = "Variable Importance for Train RF")
importance(train.rf)
train.rf.fit <- predict(train.rf, newdata = test)

vars_select <- c('team1_adjusto', 'team1_adjustd', 'team1_sospyth', 'team1_adjustt', 'team1_sosoppo', 'team1_conf','team1_seed',
                 'team2_adjusto', 'team2_adjustd', 'team2_sospyth', 'team2_adjustt', 'team2_sosoppo','team2_conf','team2_seed','team1_victory')

train.rf_v2 <- randomForest(team1_victory ~., data = train[vars_select], ntree = 500, type = 'classification')       
plot(train.rf_v2, main = "Train Random Forest")  
varImpPlot(train.rf_v2, main = "Variable Importance for Train RF")
importance(train.rf_v2)
train.rf.fit.v2 <- predict(train.rf_v2, newdata = test)


rmse(train.rf.fit, test$team1_victory)
rmse(train.rf.fit.v2, test$team1_victory)

xvars <- vars[1:20]
f.rf1 <- paste(xvars, collapse = ' + ')
f.rf1

rf.v1.cv <- train(team1_victory ~ ., data = train.full[,vars], trControl = cv.train, method = "rf")
rf.v2.cv <- train(team1_victory ~ ., data = train.full[,vars_select], trControl = cv.train, method = "rf")
rf.v1.cv
rf.v2.cv

#########################################
# Step 5 - xgboost
#########################################
require(xgboost)

# set up the cross-validated hyper-parameter search
xgb.grid <- expand.grid(
  nrounds = 1000,
  eta = c(0.01, 0.001, 0.0001),
  max_depth = c(2, 4, 6, 8, 10),
  gamma = 1,
  min_child_weight = c(1,3,5),
  colsample_bytree = c(0.5, 0.75, 1),
  subsample = c(0.5, 0.75, 1)
)

# pack the training control parameters
xgb.trcontrol <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

# train the model for each parameter combination in the grid,
#   using CV to evaluate
xgb.train <- train(
  x = as.matrix(train[,vars] %>% select(-team1_victory)),
  y = make.names(train$team1_victory),
  trControl = xgb.trcontrol,
  tuneGrid = xgb.grid,
  method = "xgbTree",
  verbose = TRUE
)

param <- list("objective" = "binary:logistic",
              "nthread" = 3,
              "eta" = 0.01,
              "gamma" = 1,
              "min_child_weight" = 5,
              "subsample"=0.75,
              "max_depth"= 10,
              "colsample_bytree" = 1)

x <- sapply(train[,11:ncol(train) -1], as.numeric)
x <- matrix(as.numeric(x),nrow(x),ncol(x))
y <- train[,'team1_victory']
y.test <- test[,'team1_victory']
trind <- 1:length(y)
teind <- (nrow(train)+1):nrow(x)

xgboost <- xgboost(param = param, data = x[trind,], label = y, nrounds = 1000)
xgboost.preds <- predict(xgboost, newdata = as.matrix(test[,vars]))
rmse(xgboost.preds, test$team1_victory)

#########################################
# Step 6 - neural net
#########################################

f <- paste(xvars, collapse = ' + ')
f <- paste('team1_victory ~', f)
f <- as.formula(f)
f

nn <- neuralnet(f,data = train, hidden=c(5,2), linear.output = FALSE, algorithm = 'backprop', learningrate = 0.1)
nn.fit <- neuralnet::compute(nn, test[,xvars])
nn.fit$net.result2 <- sapply(nn.fit$net.result,round,digits=0)
rmse(nn.fit$net.result2, test$team1_victory)

#########################################
# Step 7 - ensemble models with neural net
#########################################

# create test dataframe
xtest.glm.preds.null <- as.data.frame(glm.null.pred)
xtest.glm.preds.final <- as.data.frame(glm.step.pred)
xtest.xgb.preds.final <- as.data.frame(xgboost.preds)
xtest.rf.preds.final <- as.data.frame(train.rf.fit)
xtest.nn.preds.final <- as.data.frame(nn.fit$net.result)
ytest <- as.data.frame(test$team1_victory)
test.ens <- cbind(xtest.glm.preds.final, xtest.rf.preds.final,xtest.xgb.preds.final,xtest.nn.preds.final,ytest)
colnames(test.ens) <- c('glm.step','rf','xgb','nn','team1_victory')

# build ensemble nn
ens.vars <- colnames(test.ens)
ens.vars <- ens.vars[1:4]
f.ens <- paste(ens.vars, collapse = ' + ')
f.ens <- paste('team1_victory ~', f.ens)
f.ens <- as.formula(f.ens)
f.ens

plot(nn.ens)

nn.ens <- neuralnet(f.ens, data = test.ens, hidden = 3, linear.output = FALSE, algorithm = 'backprop', learningrate = 0.1)
nn.ens.fit <- neuralnet::compute(nn.ens, test.ens[,ens.vars])
nn.ens.fit$net.result2 <- sapply(nn.ens.fit$net.result,round,digits=0)
rmse(nn.ens.fit$net.result2, test.ens$team1_victory)
summary(nn.ens.fit$net.result)
summary(test.ens$team1_victory)

#########################################
# Step 8 - normalize final subimission file
#########################################

final.norm <- as.data.frame(lapply(test.final[,norm_var],normalize))
final.norm.rev <- as.data.frame(lapply(test.final[,rev_var],normalize))
final.full <- cbind(test.final[,1:8], final.norm, final.norm.rev)
final.full <- na.omit(final.full)
rm(final.norm, final.norm.rev)

#########################################
# Step 9 - final final predictions
#########################################

glm.null.pred.final <- predict(glm.null, newdata = test.final[,xvars], type = "response")
glm.step.pred.final <- predict(glm.step, newdata = test.final[,xvars], type = "response")
rf.pred.final.final <- predict(train.rf, newdata = final.full[,xvars])
xbg.pred.final <- predict(xgboost, newdata = as.matrix(final.full[,xvars]))
nn.pred.final.final <- neuralnet::compute(nn, final.full[,xvars])

glm.null.submit <- as.data.frame(glm.null.pred.final)
glm.step.submit <- as.data.frame(glm.step.pred.final)
rf.submit <- as.data.frame(rf.pred.final.final)
xgb.submit <- as.data.frame(xbg.pred.final)
nn.submit <- as.data.frame(nn.pred.final.final$net.result)

final.ens <- cbind(glm.step.submit, rf.submit, xgb.submit, nn.submit)
colnames(final.ens) <- c('glm.step','rf','xgb','nn')
summary(final.ens)

# fit nn ensemble
nn.ens.fit.submit <- neuralnet::compute(nn.ens, final.ens[,ens.vars])
nn.ens.submit <- as.data.frame(nn.ens.fit.submit$net.result)
colnames(nn.ens.submit) <- 'ens.nn'
nn.ens.submit$net.result <- sapply(nn.ens.fit.submit$net.result,round,digits=0)

final.full <- cbind(final.full, final.ens, nn.ens.submit)
head(final.full)

write.csv(final.full, 'finalsubmit.csv', row.names = FALSE)

