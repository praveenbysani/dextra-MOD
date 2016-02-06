library(caret)
library(dplyr)
library(xgboost)

hr_train <- read.csv('20150803115609-HR_Retention_2013_training.csv',
                     na.strings = c('NA',''))
hr_test <- read.csv('20150803115608-HR_Retention_2013_to_be_predicted.csv',
                    na.strings = c('NA',''))

#remove the resignation details to have same dimensions as train data
hr_train <- select(hr_train,-one_of(c('RESIGN_DATE','STATUS')))
hr_train <- select(hr_train,-starts_with('RESIGNATION'))
#reorder the atrributes 
hr_train <- hr_train[,c(1:4,6:52,5)]
#merge train and test set for common factor levels
hr_test$RESIGNED <- -1
hr_merge <- rbind(hr_train,hr_test)

#handle sparsity in attributes
hr_merge <- select(hr_merge,-one_of(c('UPGRADED_CERT_DESC_3_YRS','HSP_CERT_DESC','UPGRADED_CERT_3_YRS',
                                      'HSP_CERTIFICATE','HOUSING_TYPE','PREV_HOUSING_TYPE','HSP_ESTABLISHMENT'
                                      )))

#count the number of NA for each row


#impute the NA values in merged data
for(attrib in colnames(hr_merge)){
  colvector <- hr_merge[,attrib]
  #replace empty values of nominal attributes with unknown
  if(class(colvector) == 'factor'){
    levels(colvector) <- c(levels(colvector),"UNKNOWN")
    hr_merge[,attrib] = replace(colvector,is.na(colvector),"UNKNOWN")
  }
  else
    hr_merge[,attrib] = replace(colvector,is.na(colvector),median(colvector,na.rm = TRUE))
  
}

#normalize the counts of unit changes and promotions in recent years
hr_merge <- cbind(hr_merge,NUM_UNIT_CHANGES= 
                    apply(select(hr_merge,starts_with('UNIT_CHG')),1,unitFunc <- function(...) { length(grep("YES",c(...)))}),
                  NUM_PROMS=
                    apply(select(hr_merge,starts_with('PROMO_LAST')),1,sum)
)
hr_merge <- mutate(hr_merge, PERC_YEAR_GRADE = YEARS_IN_GRADE/YEARS_OF_SERVICE,
                   W_TOT_INCREASE=TOT_PERC_INC_LAST_1_YR*AGE, W_BASE_INCREASE = BAS_PERC_INC_LAST_1_YR*AGE)

hr_merge_train <- filter(hr_merge, RESIGNED != -1)
hr_merge_test <- filter(hr_merge, RESIGNED == -1)

set.seed(999)
trainSamples <- createDataPartition(hr_merge_train$RESIGNED,p=0.8,list=FALSE)

hr_merge_train_model <- hr_merge_train[trainSamples,]
hr_merge_train_valid <- hr_merge_train[-trainSamples,]


hr_merge_train_model_sparse = select(hr_merge_train_model,-one_of(c("RESIGNED","PERID")))
hr_merge_train_valid_sparse = select(hr_merge_train_valid,-one_of(c("RESIGNED","PERID")))

hr_merge_train_sparse <- sparse.model.matrix(~.,data=hr_merge_train_model_sparse)
hr_merge_valid_sparse <- sparse.model.matrix(~.,data=hr_merge_train_valid_sparse)

xgb_hr_train <- xgboost(data = hr_merge_train_sparse, label = hr_merge_train_model$RESIGNED, max.depth = 200,
                        eta = 0.01, nthread = 4, nround = 500,objective = "reg:linear",eval_metric="logloss")



hr_xgb_predictions <- predict(xgb_hr_train,hr_merge_valid_sparse)