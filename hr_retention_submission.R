library(caret)
library(dplyr)

#predictions for submission
gbmGrid <- expand.grid(interaction.depth=3,n.trees=1000,shrinkage=0.1,
                       n.minobsinnode=10)
gbmTrain <- train(RESIGNED~ ., data=hr_merge_train,trControl=noneControl,
                  tuneGrid=gbmGrid, method='gbm')

rfGrid <- data.frame(mtry=20)
rfTrain <- train(RESIGNED~ . , data=hr_merge_train,trControl=noneControl,
                 tuneGrid=rfGrid, method='rf')

svmGrid <- data.frame(C=1, sigma=0.02)
svmTrain <- train(RESIGNED~ . , data=hr_merge_train,trControl=noneControl,
                  method='svmRadial', preProcess=c('scale','center'),tuneGrid=svmGrid)


nnGrid <- data.frame(size=5,decay=0.1)
nnTrain <- train(RESIGNED ~., data=hr_merge_train, trControl=noneControl, tuneGrid=nnGrid,
                 method='pcaNNet',preProcess=c('scale','center')) 

xgGrid <- expand.grid(eta=0.01, max_depth=200, nrounds=1000)
xgTrain <- train(RESIGNED ~ . , data=hr_merge_train,trControl = noneControl,
                 tuneGrid=xgGrid, method='xgbTree')



hr_rf_predictions <- predict(rfTrain,newdata = hr_merge_test)
hr_xg_predictions <- predict(xgTrain,newdata = hr_merge_test)
hr_nn_predictions <- predict(nnTrain,newdata = hr_merge_test)
hr_svm_predictions <- predict(svmTrain,newdata = hr_merge_test)
hr_gbm_predictions <- predict(gbmTrain,newdata = hr_merge_test)


pred_rf_submission <- cbind(hr_test$PERID,hr_rf_predictions)
pred_xg_submission <- cbind(hr_test$PERID,hr_xg_predictions)
pred_nn_submission <- cbind(hr_test$PERID,hr_nn_predictions)
pred_svm_submission <- cbind(hr_test$PERID,hr_svm_predictions)
pred_gbm_submission <- cbind(hr_test$PERID,hr_gbm_predictions)

pred_average <- (hr_rf_predictions+hr_xg_predictions+hr_nn_predictions+hr_svm_predictions)/4
pred_avg_submission <- cbind(hr_test$PERID,pred_average)

colnames(pred_rf_submission) <- c('PERID','RESIGNED')
colnames(pred_xg_submission) <- c('PERID','RESIGNED')
colnames(pred_nn_submission) <- c('PERID','RESIGNED')
colnames(pred_svm_submission) <- c('PERID','RESIGNED')
colnames(pred_gbm_submission) <- c('PERID','RESIGNED')
colnames(pred_avg_submission) <- c('PERID','RESIGNED')

write.csv(pred_rf_submission,file='submission_rf.csv',row.names = FALSE)
write.csv(pred_xg_submission,file='submission_xg.csv',row.names = FALSE)
write.csv(pred_nn_submission,file='submission_nn.csv',row.names = FALSE)
write.csv(pred_svm_submission,file='submission_svm.csv',row.names = FALSE)
write.csv(pred_gbm_submission,file='submission_gbm.csv',row.names = FALSE)
write.csv(pred_average,file='submission_ensemble.csv',row.names = FALSE)

write.csv(pred_avg_submission,file='submission_ensemble_rf_nn_xg_svm.csv',row.names = FALSE)


