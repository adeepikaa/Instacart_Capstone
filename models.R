# This file contains all the different models evaluated

# this is a list of all features 

# [1] "user_id"              "product_id"           "order_id"            
# [4] "buy"                  "user_avg_cart_size"   "prod_pop_n"          
# [7] "prod_reord_tot"       "prod_org_flag"        "prod_avg_freq"       
# [10] "produce_flag"         "dairy_eggs_flag"      "snacks_flag"         
# [13] "user_n_items"         "user_orders_n"        "user_avg_hr"         
# [16] "user_avg_day"         "user_avg_freq"        "user_reord_pct"      
# [19] "user_prod_reord_pct"  "user_prod_cart_order" "p rev_flag"           
# [22] "last3_reorder_pct"    "last3_flag"

# selected specific features based on corelation analysis
xvalues<-c(            
  "buy",                 
  "user_avg_cart_size",           
  "prod_reord_tot",      
  "prod_org_flag",       
  "prod_avg_freq",      
  "produce_flag",        
  "dairy_eggs_flag",     
  "snacks_flag",        
  "user_orders_n",               
  "user_avg_hr",        
  "user_avg_day",        
  "user_avg_freq",       
  "user_reord_pct",     
  "user_prod_reord_pct",
  "user_prod_cart_order",
  "prev_flag",           
  "last3_reorder_pct",  
  "last3_flag"
)

# reducing dataset to contain only features and dependent variable
trainset<-train_set[,xvalues]
evalset<-eval_set[,xvalues]
testset<-test_set[,xvalues]

# write.csv(trainset, "trainset.csv")
# write.csv(testset, "testset.csv")
# write.csv(evalset, "evalset.csv")


#######################################################
# User defined function to create different metrics for classification analysis
# This dataset is an imbalanced dataset that means the number of observations
# between the two classes are significantly difficult. Hence using only accuracy
# as a measurement is insufficient

# F score and area under ROC curve have been used to analyze models. However, final
# decision to pick model was decided based on F Score

get_result_stats<-function(x,y){
  cm<-table(Predict=x, Reference=y)
  acc<-(cm[1,1]+cm[2,2])/sum(cm)
  precision<-cm[2,2]/(cm[1,2]+cm[2,2])
  recall<-cm[2,2]/(cm[2,1]+cm[2,2])
  f1score<-2*precision*recall/(precision+recall)
  
  list(cm=cm, acc=acc, precision=precision, recall=recall, f1score=f1score)
}

#######################################################
# Naive Bayes
#######################################################

# use caret package train function
model_nb<-train(trainset[,-1], trainset$buy, method="naive_bayes")

# predict on evalset
buy_nb<-predict(model_nb, evalset)

# summarize metrics
nb_summary<-get_result_stats(buy_nb, evalset$buy)

# use probablity predictions to calculate area under curve
nb_y<- predict(model_nb, evalset, type="prob")[,2]
pred <- prediction(nb_y, evalset$buy)
nb_auc = performance(pred, measure = "auc")

# summary table to add all model results
all_results<-data.frame(method="Naive Bayes", f1score=nb_summary$f1score, 
                        acc=nb_summary$acc, precision=nb_summary$precision,
                        recall=nb_summary$recall, AUC=round(nb_auc@y.values[[1]],6))

# Plot of ROC curve
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)


#######################################################
# Logistic Regression: GLM
#######################################################

# use caret package train function
model_glm<-train(trainset[,-1], trainset$buy, method="glm")

# predict on evalset
buy_glm<-predict(model_glm, evalset)

# summarize metrics
glm_summary<-get_result_stats(buy_glm, evalset$buy)

# use probablity predictions to calculate area under curve
glm_y<- predict(model_glm, evalset, type="prob")[,2]
pred <- prediction(glm_y, evalset$buy)
glm_auc = performance(pred, measure = "auc")


# summary table to add all model results
all_results<-rbind(all_results, data.frame(method="Logistic Regression", f1score=glm_summary$f1score, 
                                           acc=glm_summary$acc, precision=glm_summary$precision,
                                           recall=glm_summary$recall, AUC=round(glm_auc@y.values[[1]],6)))



#######################################################
# Classification Tree: CART
#######################################################

# use caret package train function
model_tree<-train(trainset[,-1], trainset$buy, method="rpart")

# predict on evalset
buy_tree<-predict(model_tree, evalset)

# summarize metrics
tree_summary<-get_result_stats(buy_tree, evalset$buy)

# use probablity predictions to calculate area under curve
tree_y<- predict(model_tree, evalset, type="prob")[,2]
pred <- prediction(tree_y,evalset$buy)
tree_auc = performance(pred, measure = "auc")

# summary table to add all model results
all_results<-rbind(all_results, data.frame(method="CART", f1score=tree_summary$f1score, 
                                           acc=tree_summary$acc, precision=tree_summary$precision,
                                           recall=tree_summary$recall, AUC=round(tree_auc@y.values[[1]],6)))

# fine tuning of cp parameter
plot(model_tree)

# Classification Tree plot
plot(model_tree$finalModel)
text(model_tree$finalModel)



#######################################################
# Random Forest: RF
#######################################################

#Random Forest function with 100 trees
model_forest<-randomForest(trainset[,-1], trainset$buy, ntree=100)

# predict on evalset
buy_rf<-predict(model_forest, evalset)

# summarize metrics
rf_summary<-get_result_stats(buy_rf, evalset$buy)

# use probablity predictions to calculate area under curve
rf_y<- predict(model_forest, evalset, type="prob")[,2]
pred <- prediction(rf_y, evalset$buy)
forest_auc = performance(pred, measure = "auc")

# summary table to add all model results
all_results<-rbind(all_results, data.frame(method="Random Forest", f1score=rf_summary$f1score, 
                                           acc=rf_summary$acc, precision=rf_summary$precision,
                                           recall=rf_summary$recall, AUC=round(forest_auc@y.values[[1]],6)))
all_results%>%knitr::kable()


# Fine tuning of mtry
t <- tuneRF(trainset[, -1], trainset[, 1],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 400,
            trace = TRUE,
            improve = 0.01)

t
# gives best mtry as 4


# Variable Importance Plot
imp <- as.data.frame(varImpPlot(model_forest))
imp$varnames <- rownames(imp) # row names to column
rownames(imp) <- NULL  


ggplot(imp, aes(x=reorder(varnames, -MeanDecreaseGini), weight=MeanDecreaseGini, fill=varnames)) + 
  geom_bar() +
  scale_fill_discrete(name="Variable Group") +
  ylab("MeanDecreaseGini") +
  xlab("Variable Name") +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 45, hjust = 1))+
  theme(legend.position = "none")+
  ggtitle("Variable Importance of Random Forest")


##############################################################################
# Based on results summary the best Fscore is for the Random Forest model. Hence, this 
# has been chosen as the final model.

# However, due to the imbalanced nature of the dataset the optimum cutoff to 
# classify the binary outcome may not be 0.5. Hence a cutoff analysis can yield
# more accurate predictions.

# The cutoff analysis uses the probability predictions for the analysis and sweeps 
# various cutoff values to calculate maximum F score.
##############################################################################

# Cutoff Analysis

# user deifned function for cutoff analysis
get_cutoff<-function(x){
  
  cutoff<-seq(0.1, 0.7, 0.025)
  f1s<-sapply(cutoff, function(z){
    y<-ifelse(x>z,1,0)
    scores<-get_result_stats(y, evalset$buy)
    scores$f1score
  })
  
  return(cutoff[which.max(f1s)])
}

# run cutoff analysis for random forest predictions
rf_cutoff<-get_cutoff(rf_y) 

# predict outcomes based on new cutoff
rf_final_y<-ifelse(rf_y>rf_cutoff,1,0)

# final summary for train/test data
rf_final_stats<-get_result_stats(rf_final_y, evalset$buy)


################################################################
# Test Dataset Results
################################################################

# Use random forest model to predict probablities
test_pred_y<- predict(model_forest, testset, type="prob")[,2]

# Use cutoff from Random forest analysis to cutoff the test dataset
test_final_y<-ifelse(test_pred_y>rf_cutoff,1,0)

# get final summary
test_stats<-get_result_stats(test_final_y, testset$buy)
test_stats

