

# [1] "user_id"              "product_id"           "order_id"            
# [4] "buy"                  "user_avg_cart_size"   "prod_pop_n"          
# [7] "prod_reord_tot"       "prod_org_flag"        "prod_avg_freq"       
# [10] "produce_flag"         "dairy_eggs_flag"      "snacks_flag"         
# [13] "user_n_items"         "user_orders_n"        "user_avg_hr"         
# [16] "user_avg_day"         "user_avg_freq"        "user_reord_pct"      
# [19] "user_prod_reord_pct"  "user_prod_cart_order" "prev_flag"           
# [22] "last3_reorder_pct"    "last3_flag"

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


trainset<-trainset[,xvalues]
evalset<-evalset[,xvalues]
testset<-testset[,xvalues]

# write.csv(trainset, "trainset.csv")
# write.csv(testset, "testset.csv")
# write.csv(evalset, "evalset.csv")

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

model_nb<-train(trainset[,-1], trainset$buy, method="naive_bayes")
buy_nb<-predict(model_nb, evalset)

nb_summary<-get_result_stats(buy_nb, evalset$buy)

nb_y<- predict(model_nb, evalset, type="prob")[,2]
pred <- prediction(nb_y, evalset$buy)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
nb_auc = performance(pred, measure = "auc")

all_results<-data.frame(method="Base: Naive Bayes", f1score=nb_summary$f1score, 
                        acc=nb_summary$acc, precision=nb_summary$precision,
                        recall=nb_summary$recall, AUC=round(nb_auc@y.values[[1]],6))


# Logistic Regression

model_glm<-train(trainset[,-1], trainset$buy, method="glm")
buy_glm<-predict(model_glm, evalset)

glm_summary<-get_result_stats(buy_glm, evalset$buy)


glm_y<- predict(model_glm, evalset, type="prob")[,2]
pred <- prediction(glm_y, evalset$buy)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
glm_auc = performance(pred, measure = "auc")

plot(roc.perf)
abline(a=0, b= 1)



all_results<-rbind(all_results, data.frame(method="Logistic Regression", f1score=glm_summary$f1score, 
                                           acc=glm_summary$acc, precision=glm_summary$precision,
                                           recall=glm_summary$recall, AUC=round(glm_auc@y.values[[1]],6)))



# Classification Tree
model_tree<-train(trainset[,-1], trainset$buy, method="rpart")

buy_tree<-predict(model_tree, evalset)
tree_summary<-get_result_stats(buy_tree, evalset$buy)

tree_y<- predict(model_tree, evalset, type="prob")[,2]
pred <- prediction(tree_y,evalset$buy)
tree_auc = performance(pred, measure = "auc")

all_results<-rbind(all_results, data.frame(method="CART", f1score=tree_summary$f1score, 
                                           acc=tree_summary$acc, precision=tree_summary$precision,
                                           recall=tree_summary$recall, AUC=round(tree_auc@y.values[[1]],6)))

plot(model_tree)
plot(model_tree$finalModel)
text(model_tree$finalModel)


# Random Forest
#model_forest<-randomForest(trainset[,c(6,7,11,21,22,23)], trainset$buy, mtry=3, ntree=10)
model_forest<-randomForest(trainset[,-1], trainset$buy, ntree=100)

buy_rf<-predict(model_forest, evalset)
rf_summary<-get_result_stats(buy_rf, evalset$buy)

rf_y<- predict(model_forest, evalset, type="prob")[,2]
pred <- prediction(rf_y, evalset$buy)
forest_auc = performance(pred, measure = "auc")


all_results<-rbind(all_results, data.frame(method="Random Forest", f1score=rf_summary$f1score, 
                                           acc=rf_summary$acc, precision=rf_summary$precision,
                                           recall=rf_summary$recall, AUC=round(forest_auc@y.values[[1]],6)))
all_results%>%knitr::kable()


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

# Cutoff Analysis


get_cutoff<-function(x){
  
  cutoff<-seq(0.1, 0.7, 0.025)
  f1s<-sapply(cutoff, function(z){
    y<-ifelse(x>z,1,0)
    scores<-get_result_stats(y, evalset$buy)
    scores$f1score
  })
  
  return(cutoff[which.max(f1s)])
}

glm_cutoff<-get_cutoff(glm_y) 
glm_final_y<-ifelse(glm_y>glm_cutoff,1,0)
glm_final_stats<-get_result_stats(glm_final_y, evalset$buy)

tree_cutoff<-get_cutoff(tree_y) 
tree_final_y<-ifelse(tree_y>tree_cutoff,1,0)
tree_final_stats<-get_result_stats(tree_final_y, evalset$buy)


rf_cutoff<-get_cutoff(rf_y) 
rf_final_y<-ifelse(rf_y>rf_cutoff,1,0)
rf_final_stats<-get_result_stats(rf_final_y, evalset$buy)


################################################################
# Test Data Results

test_pred_y<- predict(model_forest, testset, type="prob")[,2]
test_final_y<-ifelse(test_pred_y>rf_cutoff,1,0)
test_stats<-get_result_stats(test_final_y, testset$buy)
test_stats

