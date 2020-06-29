# use caret package train function
model_gbm<-train(trainset[,-1], trainset$buy, method="gbm")

# predict on evalset
buy_gbm<-predict(model_gbm, evalset)

# summarize metrics
gbm_summary<-get_result_stats(buy_gbm, evalset$buy)

# use probablity predictions to calculate area under curve
gbm_y<- predict(model_gbm, evalset, type="prob")[,2]
pred <- prediction(gbm_y, evalset$buy)
gbm_auc = performance(pred, measure = "auc")

# summary table to add all model results
all_results<-rbind(all_results, data.frame(method="GBM", f1score=gbm_summary$f1score, 
                                           acc=gbm_summary$acc, precision=gbm_summary$precision,
                                           recall=gbm_summary$recall, AUC=round(gbm_auc@y.values[[1]],6)))



# run cutoff analysis for best model predictions
final_cutoff<-get_cutoff(gbm_y) 

# predict outcomes based on new cutoff
final_y<-ifelse(gbm_y>final_cutoff,1,0)

# final summary for train/test data
final_stats<-get_result_stats(final_y, evalset$buy)
final_stats









set.seed(123, sample.kind="Rounding")
model_xgb <- train(trainset[,-1], y = trainset$buy,
                   method = "xgbTree",
                   nrounds = 50,
                   max_depth = 6,
                   subsample=0.85,
                   colsample_bytree=0.7,
                   eta = 0.1,
                   verbose=TRUE)
