library(caret)
library(dplyr)

LogLoss<-function(obs, pred)
{
  pred<-(pmax(pred, 0.00001))
  pred<-(pmin(pred, 0.99999))
  result<- -1/length(obs)*(sum((obs*log(pred)+(1-obs)*log(1-pred))))
  return(result)
}


hr_merge_model <- hr_merge_mut2
hr_merge_train <- filter(hr_merge_model, RESIGNED != -1)
hr_merge_test <- filter(hr_merge_model, RESIGNED == -1)

set.seed(999)
trainSamples <- createDataPartition(hr_merge_train$RESIGNED,p=0.8,list=FALSE)

hr_merge_train_model <- hr_merge_train[trainSamples,]
hr_merge_train_valid <- hr_merge_train[-trainSamples,]


trainControl <- trainControl(method='cv',number=3,verboseIter = TRUE)
noneControl <- trainControl(method='none',verboseIter = TRUE)

#gbmGrid <- expand.grid(interaction.depth=c(6,9,12),n.trees=1000,shrinkage=0.1,
#                       n.minobsinnode=10)
#gbmGrid <- data.frame(interaction.depth=3,n.trees=3000,shrinkage=0.1,
#                                              n.minobsinnode=10)

rfGrid <- data.frame(mtry=10)
rfTrain <- train(RESIGNED~ ., data=hr_merge_train_model,trControl=noneControl,
                 tuneGrid=rfGrid, method='rf')

#xgGrid <- data.frame(nrounds=10,max_depth=10,eta=0.3)
xgGrid <- expand.grid(eta=0.01, max_depth=200, nrounds=1000)
xgTrain <- train(RESIGNED ~ . , data=hr_merge_train_model,trControl = noneControl,
                 tuneGrid=xgGrid, method='xgbTree')

#knnGrid <- data.frame(k=10)
knnTrain <- train(RESIGNED~ . - PERID, data=hr_merge_train_model,trControl=gbmControl,
                  tuneLength=1,preProcess = c("center","scale"), method='Boruta')

svmGrid <- data.frame(C=1, sigma=0.02)
svmTrain <- train(RESIGNED~ . , data=hr_merge_train_model,trControl=noneControl,
                   method='svmRadial', preProcess=c('scale','center'),tuneGrid=svmGrid)


nnGrid <- data.frame(size=5,decay=0.1)
nnTrain <- train(RESIGNED ~.*., data=hr_merge_train_model, trControl=noneControl, tuneGrid=nnGrid,
                 method='pcaNNet',preProcess=c('scale','center')) 

lrGrid <- expand.grid(alpha=0.6,lambda=0.1)
lrTrain <- train(RESIGNED ~ . , data=hr_merge_train_model,trControl=noneControl, tuneGrid=lrGrid,
                 method='glmnet',preProcess=c('scale','center'))

hr_validation <- predict(rfTrain,newdata = hr_merge_train_valid)
LogLoss(hr_merge_train_valid$RESIGNED,hr_validation)
