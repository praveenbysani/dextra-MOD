library(dplyr)
library(caret)
library(doParallel)

#nCores <- 4
#cl <- makeCluster(nCores)
#registerDoParallel(cl)
setwd("GitHub/MOD/")
LogLoss<-function(obs, pred)
{
  pred<-(pmax(pred, 0.00001))
  pred<-(pmin(pred, 0.99999))
  result<- -1/length(obs)*(sum((obs*log(pred)+(1-obs)*log(1-pred))))
  return(result)
}

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

#count the number of NA for each row
hr_merge <- cbind(hr_merge, NUM_NA = apply(hr_merge,1, function(x) length(which(is.na(x)))))
#create new features for RANK_GRADE, SERVICE_TYPE, EMPLOYEE_GROUP
hr_merge <- cbind(hr_merge, IS_RANK1 = apply(select(hr_merge,RANK_GRADE),1, function(x) ifelse(x=="RANK 1",1,0)))
hr_merge = cbind(hr_merge,IS_PREMIUM = apply(select(hr_merge,SERVICE_TYPE),1,
                                             function(x) length(grep("Premium",x))))
hr_merge = cbind(hr_merge,IS_NS = apply(select(hr_merge,EMPLOYEE_GROUP),1,
                                             function(x) ifelse(x=="MILITARY REGULARS",0,1)))

#transform boolean attributes to numerical
boolean2IntFunc <- function(x){
  x <- as.character(x)
  x[x=="YES"] <- "1"
  x[x != "YES"] <- "0"
  as.integer(x)
}

hr_merge[,c("DIVORCE_WITHIN_2_YEARS","UPGRADED_LAST_3_YRS","MARRIED_WITHIN_2_YEARS",
            "DIVORCE_REMARRIED_WITHIN_2_YEARS","MOVE_HOUSE_T_2","UNIT_CHG_LAST_3_YRS","UNIT_CHG_LAST_2_YRS","UNIT_CHG_LAST_1_YR")] <-
  sapply(hr_merge[,c("DIVORCE_WITHIN_2_YEARS","UPGRADED_LAST_3_YRS","MARRIED_WITHIN_2_YEARS",
                     "DIVORCE_REMARRIED_WITHIN_2_YEARS","MOVE_HOUSE_T_2","UNIT_CHG_LAST_3_YRS","UNIT_CHG_LAST_2_YRS","UNIT_CHG_LAST_1_YR")],boolean2IntFunc)


#delete un necessary attributes
hr_merge <- select(hr_merge,-one_of(c('PERID','UPGRADED_CERT_DESC_3_YRS','HSP_CERT_DESC','UPGRADED_CERT_3_YRS',
                                      'HSP_CERTIFICATE','HOUSING_TYPE','PREV_HOUSING_TYPE','HSP_ESTABLISHMENT',
                                      'PARENT_SERVICE','SERVICE_SUB_AREA','NATIONALITY','COUNTRY_OF_BIRTH',
                                      'RANK_GRADE','SERVICE_TYPE','RANK_GROUPING','EMPLOYEE_GROUP','VOC','UNIT',
                                      'GENDER','AGE_GROUPING','HOUSING_GROUP','MARITAL_STATUS')))


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
         apply(select(hr_merge,starts_with('UNIT_CHG')),1,sum),
        NUM_PROMS=
         apply(select(hr_merge,starts_with('PROMO_LAST')),1,sum)
        
        
       )

hr_merge <- mutate(hr_merge, PERC_YEAR_GRADE = 1- (YEARS_IN_GRADE/YEARS_OF_SERVICE),
                   TOT_SCORE = IPPT_SCORE*PES_SCORE , INJURY_PES_SCORE = PES_SCORE*SVC_INJURY_TYPE,
                   SAL_PERC_FRAC = BAS_PERC_INC_LAST_1_YR/(TOT_PERC_INC_LAST_1_YR+1),
                   KIDS_AGE = NO_OF_KIDS*AVE_CHILD_AGE)

#to compute values of numerical vars against resignation
#hr_train %>% group_by(RESIGNED) %>% summarise(sal_inc = mean(TOT_PERC_INC_LAST_1_YR,na.rm=TRUE))

#to compute resignation percentages for nominal attributes
# hr_merge %>% group_by(NO_OF_KIDS) %>% summarise(mean_resign=mean(RESIGNED),count=n()) %>% arrange(desc(mean_resign),desc(count))


#TODO: compute the average tot/base increase in salary across
#'RANK_GRADE','SERVICE_TYPE','RANK_GROUPING','EMPLOYEE_GROUP','VOC','UNIT'


#predictions for submission
gbmGrid <- expand.grid(interaction.depth=3,n.trees=3000,shrinkage=0.1,
                       n.minobsinnode=10)
gbmTrain <- train(RESIGNED~ . - PERID, data=hr_merge_train,trControl=submControl,
                  tuneGrid=gbmGrid, method='gbm')

rfGrid <- data.frame(mtry=20)
rfTrain <- train(RESIGNED~ . - PERID, data=hr_merge_train,trControl=submControl,
                 tuneGrid=rfGrid, method='rf')

hr_predictions <- predict(rfTrain,newdata = hr_merge_test)
pred_submission <- cbind(hr_test$PERID,hr_predictions)
colnames(pred_submission) <- c('PERID','RESIGNED')
write.csv(pred_submission,file='submission_rf_mtry20_mutated.csv',row.names = FALSE)



