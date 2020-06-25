# This file has the full code used in the Instacart Market Basket Analysis

# The code was done in a modular fashion and can be run individually by using 
# the modules. However, the order needs to be maintained to get the intial
# files.

#########################################################################

# Part 1: reading input and setup ( read_input.R file)

#########################################################################

# Reading all the csv files which together have the Instacart Market Basket Analysis data

# Data source: https://www.kaggle.com/c/instacart-market-basket-analysis
# The data must be downloaded from this website and the csv files must be
# saved in a data folder or directory inside the working directory for the
# code to run smoothly.

# lists all aisles in the store
aisles = read.csv('data/aisles.csv')
# Data dict: aisle name and aisle id

# lists all aisles in the store
departments = read.csv('data/departments.csv')
# Data dict:department name and department id

# lists all aisles in the store
products = read.csv('data/products.csv')
# Data dict:product name, product id, aisle id and department id to which the product belongs

# lists all orders ( previous and current nth orders that need prediction)
orders = read.csv('data/orders.csv')
# data dict: 
#   order_id unique id for the order
#   user_id: id of user who placed the order
#   order_number: order number for that user (identifies which order in the past)
#   order_dow: day of the week the order was placed
#   order_hr:  hour of the day the order was placed

#lists all previous orders
order_products_prior = read.csv('data/order_products__prior.csv')
# data dict: 
#   order_id unique id for the order
#   product_id: id of the product the user ordered in that order
#   add_to_car_order: order in which the product was added to the cart
#   reordered: 1 indicated user ordered it before, 
#              0 indicates user ordered a new product that was never ordered by this user

# lists all orders that need prediction
order_products_train = read.csv('data/order_products__train.csv')
# data dict: 
#   order_id unique id for the order
#   product_id: id of the product the user ordered in that order
#   add_to_car_order: order in which the product was added to the cart
#   reordered: 1 indicated user ordered it before, 
#              0 indicates user ordered a new product that was never ordered by this user

# Loading different libraries
library(dplyr)
library(caret)
library(e1071) 
library(randomForest)
library(ROCR)
library(kernlab)

# Option to use 6 significant digits after the decimal point
options(digits=6)

## getting to know the data:
head(aisles)
head(products)
head(departments)
head(products)
head(orders)
head(order_products_train)
head(order_products_prior)

length(unique(aisles$aisle))
length(unique(products$aisle_id))
length(unique(departments$department))
length(unique(products$department_id))
length(unique(orders$user_id))

######
# Counting orders/products different sets

sum(orders$eval_set=="test")
sum(orders$eval_set=="train")
sum(orders$eval_set=="prior")


# Join Products and Aisles
products_all<-products%>%
  left_join(aisles, by.x=aisle_id, by.y=id)%>%
  left_join(departments, by.x=department_id, by.y=id)

# Join the orders in the dataset that has order ids the task wishes to predict with 
# more details from the orders file and join the product descriptions
order_products_train_all<-order_products_train %>%
  left_join(orders, by="order_id")%>%
  left_join(products_all, by="product_id")

# Join the orders in the dataset that has order ids from the past with 
# more details from the orders file and join the product descriptions
order_products_prior_all<-order_products_prior %>%
  left_join(orders, by="order_id")%>%
  left_join(products_all, by="product_id") 

# counting number of orders in the past and present list of orders 
total_orders_prior<-length(unique(order_products_prior_all$order_id))
total_orders_train<-length(unique(order_products_train_all$order_id))


#write.csv(order_products_prior_all, "instacart_table.csv")

##### Are there any user ids that are in train set but not in prior set
length(setdiff(order_products_prior_all$user_id, 
               order_products_train_all$user_id)
) #75000 that were given for test data

length(setdiff(order_products_train_all$user_id, 
               order_products_prior_all$user_id)
)# 0 no ids in train are missing from prior


rm(order_products_prior, order_products_train, products_all)
gc()

#########################################################################

# Part 2: data exploration ( data_explore.R file)

#########################################################################
# This part has some initial data exploration. More analysis was done
# in Tableau

# Total number of products ordered
order_products_prior_all%>%
  group_by(product_id, reordered)%>%
  summarize(n=n(), .groups='drop')


# Top 15 add to cart order
order_products_prior_all%>%
  group_by(add_to_cart_order)%>%
  summarize(n=n(), reorder_pct=sum(reordered==1)/n, .groups='drop')%>%
  slice(1:15)%>%
  ggplot(aes(as.factor(add_to_cart_order), n))+
  geom_bar(stat="identity")+
  ggtitle(" Add to Cart Order")+
  xlab("Add to cart order")+
  ylab(" Total count")+
  theme(axis.text.x=element_text(angle=90, hjust=1))

# Top 15 add to cart order reorder percent
order_products_prior_all%>%
  group_by(add_to_cart_order)%>%
  summarize(n=n(), reorder_pct=sum(reordered==1)/n, .groups='drop')%>%
  slice(1:15)%>%
  ggplot(aes(as.factor(add_to_cart_order), reorder_pct))+
  geom_bar(stat="identity")+
  ggtitle(" Add to Cart Order")+
  xlab("Add to cart order")+
  ylab(" Reorder percentage")+
  theme(axis.text.x=element_text(angle=90, hjust=1))


# Spread of day of the week
order_products_prior_all%>%
  group_by(order_dow)%>%
  summarize(n=n(), reorder_pct=sum(reordered==1)/n, .groups='drop')%>%
  ggplot(aes(as.factor(order_dow), n))+
  geom_bar(stat="identity")+
  ggtitle(" Day of the week")+
  xlab("Day of the week")+
  ylab(" Total count")+
  theme(axis.text.x=element_text(angle=90, hjust=1))

# reorder % for day of the week
order_products_prior_all%>%
  group_by(order_dow)%>%
  summarize(n=n(), reorder_pct=sum(reordered==1)/n, .groups='drop')%>%
  ggplot(aes(as.factor(order_dow), reorder_pct))+
  geom_bar(stat="identity")+
  ggtitle(" Day of the week")+
  xlab("Day of the week")+
  ylab(" Reorder percentage")+
  theme(axis.text.x=element_text(angle=90, hjust=1))


# Spread of hour of the day
order_products_prior_all%>%
  group_by(order_hour_of_day)%>%
  summarize(n=n(), reorder_pct=sum(reordered==1)/n, .groups='drop')%>%
  ggplot(aes(as.factor(order_hour_of_day), n))+
  geom_bar(stat="identity")+
  ggtitle(" Hour of the day")+
  xlab("Hour of the day")+
  ylab(" Total count")+
  theme(axis.text.x=element_text(angle=90, hjust=1))

# reorder % for hour of the day
order_products_prior_all%>%
  group_by(order_hour_of_day)%>%
  summarize(n=n(), reorder_pct=sum(reordered==1)/n, .groups='drop')%>%
  ggplot(aes(as.factor(order_hour_of_day), reorder_pct))+
  geom_bar(stat="identity")+
  ggtitle(" Hour of the day")+
  xlab("Hour of the day")+
  ylab(" Reorder percentage")+
  theme(axis.text.x=element_text(angle=90, hjust=1))


# Spread of the days since prior
order_products_prior_all%>%
  group_by(days_since_prior_order)%>%
  summarize(n=n(), reorder_pct=sum(reordered==1)/n, .groups='drop')%>%
  ggplot(aes((days_since_prior_order), n))+
  geom_bar(stat="identity")+
  ggtitle(" Days since prior order")+
  xlab("Days since prior order")+
  ylab(" Total count")+
  theme(axis.text.x=element_text(angle=90, hjust=1))

# reorder % for the days since prior
order_products_prior_all%>%
  group_by(days_since_prior_order)%>%
  summarize(n=n(), reorder_pct=sum(reordered==1)/n, .groups='drop')%>%
  ggplot(aes((days_since_prior_order), reorder_pct))+
  geom_bar(stat="identity")+
  ggtitle(" Days since prior order")+
  xlab("Days since prior order")+
  ylab(" Reorder percentage")+
  theme(axis.text.x=element_text(angle=90, hjust=1))





#########################################################################

# Part 3: Features defining ( features.R file)

#########################################################################

# This part creates features required to do predictive analysis.

# Features can be broadly classified into 3 patterns:
#  - User patterns
#  - Product patterns
#  - User-product combined patterns

###########################################
# User features
###########################################
# These features define the different user features based on the patterns of the user

# This feature is the average cart size for users
cart_size<-order_products_prior_all%>% 
  group_by(order_id)%>%
  summarize(user_id=first(user_id), cart_size=max(add_to_cart_order), .groups='drop')%>%
  group_by(user_id)%>%
  summarize(user_avg_cart_size=ceiling(mean(cart_size)), .groups='drop')

# These features define the number of items, the number of previous orders, 
# the average hour, day time, how frequently the user orders and what is reorder 
# percentage is for all previous orders & products
user_features<-order_products_prior_all%>%
  group_by(user_id)%>%
  summarize(user_n_items=n(), 
            user_orders_n=max(order_number), 
            user_avg_hr=mean(order_hour_of_day),
            user_avg_day=mean(order_dow),
            user_avg_freq=ceiling(mean(days_since_prior_order, na.rm=TRUE)), 
            user_reord_pct=sum(reordered==1)/n(),
            .groups='drop')


###########################################
# Product features
###########################################

# These features define the different product features based on the patterns 
# of how these products get ordered

# These features define the product popularity by counting the number of times it has been ordered,
# how many got reordered, if the product is organic and average frequency the products get ordered

prod_features<-order_products_prior_all%>%
  group_by(product_id)%>%
  summarize(
    prod_pop_n=n(), 
    prod_reord_tot=sum(reordered==1), 
    prod_org_flag=ifelse(grepl("Organic", first(product_name), fixed=TRUE),1,0),
    prod_avg_freq=ceiling(mean(days_since_prior_order, na.rm=TRUE)),
    .groups='drop')

# changing the NAs to 0. The average frequency has NAs for observations 
# where the product was ordered the 1st time
prod_features$prod_avg_freq<-ifelse(is.na(prod_features$prod_avg_freq), 0, prod_features$prod_avg_freq)

# Looking for top 3 departments
order_products_prior_all%>%
  group_by(department_id, department)%>%
  summarize(n=n(), .groups='drop')%>%arrange(desc(n))%>%slice(1:3)

# merge product names to create flags for the top 3 departments as they are frequently ordered
prod_features<-prod_features%>%
  left_join(products, by.x=product_id, by.y=product_id)

# flags to indicate produce, dairy eggs and snacks 
prod_features$produce_flag<-ifelse(prod_features$department_id==4, 1,0)
prod_features$dairy_eggs_flag<-ifelse(prod_features$department_id==16, 1,0)
prod_features$snacks_flag<-ifelse(prod_features$department_id==19, 1,0)

# removing additonal columns from the join that are not needed anymore
prod_features<-prod_features[,-c(6:8)]

#####################################################################
# User-Product combination features
#####################################################################

# These features define the different user-product combination features 
# based on the patterns of how a user orders these products

# these features define the reorder percentage of a user for a specific product and 
# the product's average position in the cart by the user
user_prod_features<-order_products_prior_all%>%
  group_by(user_id, product_id)%>%
  summarize(user_prod_reord_tot=sum(reordered==1),
            user_prod_cart_order=mean(add_to_cart_order),
            .groups='drop') 

# these features create a flag for products in the last 3 orders and 
# their reorder percentage 
last3order<-order_products_prior_all%>%
  group_by(user_id)%>%
  select(user_id, product_id, order_number, reordered)%>%
  filter(order_number==max(order_number)|order_number==max(order_number)-1|order_number==max(order_number)-2)%>%
  mutate(last3_flag=1)%>%
  group_by(user_id, product_id)%>%
  summarize(last3_reorder_pct=sum(reordered)/3, 
            last3_flag=first(last3_flag), 
            .groups='drop')

# these features create a flag for products in the last order
lastorder<-order_products_prior_all%>%
  group_by(user_id)%>%
  select(user_id, product_id, order_number)%>%
  filter(order_number==max(order_number))%>%
  mutate(prev_flag=1)



#########################################################################

# Part 4: Create Sets ( create_sets.R file)

#########################################################################

# This part creates the dataset with the dependent variable and all the features

# To create the dataset first all unique combinations of user ids and product ids 
# from prior orders must be obtained. Dependent variable be added based on the 
# order_products_train data which specifies the users and products bought in the 
# nth order the project aims to predict.

# The problem definition of this project has been modified from the original
# task on the Kaggle website. This project only aims to predict the products
# in the nth order based on n-1 orders for only those products that user has ordered
# in the past. The products the user orders in the nth order (as specified by the 
# zeros in the reordered column) are not included in this predictive analysis. 

# Get all unique combinations of the user and product combinations from prior orders
user_prod_ids<-order_products_prior_all%>%distinct(user_id,product_id)

# merge with nth orders to create the dependent variable "buy"
# buy=1 indicates the user ordered the product in the nth order
# buy=0 indicates the user did not order the product in the nth order
instacart_data<-user_prod_ids%>%
  left_join(order_products_train_all, by=c("user_id", "product_id"))%>%
  select(user_id, product_id, order_id, buy=reordered)

# Since prior order contain all other products the merge will result in NAs
# for products not ordered in the nth order. Hence change those NAs to zeros
instacart_data$buy<-ifelse(is.na(instacart_data$buy),0,1)%>%factor(.)

# Merge all features
instacart_data<-instacart_data%>%
  left_join(cart_size, by.x=user_id, by.y=user_id)%>%
  left_join(prod_features, by.x=product_id, by.y=product_id)%>%
  left_join(user_features, by.x=user_id, by.y=user_id)%>%
  left_join(user_prod_features, by=c("user_id", "product_id"))%>%
  left_join(lastorder, by=c("user_id", "product_id"))%>%
  left_join(last3order, by=c("user_id", "product_id"))

# Merge results in NAs for products not in last order, replace with 0
instacart_data$prev_flag<-ifelse(is.na(instacart_data$prev_flag),0,1)

# Merge results in NAs for products not in last 3 orders, replace with 0
instacart_data$last3_flag<-ifelse(is.na(instacart_data$last3_flag),0,1)
instacart_data$last3_reorder_pct<-
  ifelse(is.na(instacart_data$last3_reorder_pct),
         0,instacart_data$last3_reorder_pct)

# removing redundant column
instacart_data<-instacart_data[,-21] # deleting order number

# changing name to reorder percent from reorder total and calculating the percent by 
# calculating total divided by number of orders
instacart_data<-instacart_data%>%
  rename(
    user_prod_reord_pct= user_prod_reord_tot
  )
instacart_data$user_prod_reord_pct<-instacart_data$user_prod_reord_pct/instacart_data$user_orders_n

# checking any NAs
sum(is.na(instacart_data))
sum(is.na(instacart_data$order_id))
# only NAs are in order ids that are not in nth order

# remove vectors not needed after merge to free up memory
rm(user_prod_features, cart_size, user_features, prod_features, last3order, lastorder)
gc()

# factorize all flags
instacart_data$prod_org_flag<-as.factor(instacart_data$prod_org_flag)
instacart_data$produce_flag<-as.factor(instacart_data$produce_flag)
instacart_data$dairy_eggs_flag<-as.factor(instacart_data$dairy_eggs_flag)
instacart_data$snacks_flag<-as.factor(instacart_data$snacks_flag)
instacart_data$prev_flag<-as.factor(instacart_data$prev_flag)
instacart_data$last3_flag<-as.factor(instacart_data$last3_flag)


#######################################################################
# Create train, test and evaluation sets
#######################################################################

# get all unique user ids in the train set provided which is the nth order for users
user_ids<-orders%>%
  filter(eval_set=="train")%>%
  .$user_id

# This dataset contains 3 million orders in past which totals to about 34million 
# products in all with about 200K users
# The Kaggle task includes dataset in which there are 130K users with orders in the 
# order_products_train which has the nth orders for each user that need to be predicted
# This huge dataset requires high RAM infrastructure that is currently unavailable 
# to the author. Hence, reduced the dataset to include 10% of the users in the analysis
# That means number of users used for predictive analysis is about 13K which comes to 
# about a million observations

# Though the user ids have been reduced all features related to products have been taken
# from the original dataset


# randomly reducing user ids to 10%
set.seed("123")
index <- createDataPartition(y = user_ids, times = 1, p = 0.1, list = FALSE)
user_ids<-user_ids[index]

# Taking 20% for final testing of the final model
set.seed("123")
test_index <- createDataPartition(y = user_ids, times = 1, p = 0.2, list = FALSE)

temp <- user_ids[-test_index]
user_id_test <- user_ids[test_index]

# Splitting 80% 20% for training the model and fine tuning with evaluation model
set.seed("456")
eval_index <- createDataPartition(y = temp, times = 1, p = 0.2, list = FALSE)

user_id_train <- temp[-eval_index]
user_id_eval <- temp[eval_index]

# checking if any user ids overlapped
intersect(user_id_eval, user_id_test)
intersect(user_id_eval, user_id_train)

rm(temp, test_index, eval_index,index)


# merging user ids to get all features
train_set<-instacart_data%>%
  inner_join(data.frame(user_id=user_id_train), by.x=user_id, by.y=user_id)

eval_set<-instacart_data%>%
  inner_join(data.frame(user_id=user_id_eval), by.x=user_id, by.y=user_id)

test_set<-instacart_data%>%
  inner_join(data.frame(user_id=user_id_test), by.x=user_id, by.y=user_id)


nrow(train_set)
# trainset has 547802 observations

nrow(test_set)
# testset has 166744 observations

nrow(eval_set)
# evalset has 135834 observations


length(unique(train_set$user_id))
# trainset has  8396 users

length(unique(test_set$user_id))
# testset has 2628 users

length(unique(eval_set$user_id))
# evalset has 2100 users

#write.csv(instacart_data, "instacart_features.csv")

rm(user_id_eval, user_id_test, user_id_train)


#########################################################################

# Part 5: Features exploration ( features_explore.R file)

#########################################################################


# This part analyzes features

# checking features summary
str(instacart_data)
summary(instacart_data)

# corelation of non-categorical features
cor_table<-cor(instacart_data[,-c(1:4,8,10,11,12,21,23)])

#heatmap of corelation
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(cor_table, col=col, symm=TRUE)
# prod_pop_n which shows product popularity was heavily correlated with product reorders
# user_n_items which is total number of items the user ordered heavily correlated with number of orders
# these two were removed from the analysis


# Plot of average cart size of user across frequency of orders showed that cart sizes were smaller 
# when ordering frequently but did not matter after about 10 days
instacart_data%>%
  group_by(user_avg_freq)%>%
  summarize(cart=mean(user_avg_cart_size), .groups='drop')%>%
  ggplot()+
  geom_bar(aes( user_avg_freq, cart), stat="identity")

# The spread of user reorder percent shows a guassian distribution
instacart_data%>%
  group_by(re=round(user_reord_pct,2))%>%
  summarize(count=n(), .groups='drop')%>%
  ggplot(aes(x=re, y=count))+
  geom_line(size=2, col="blue")+
  ggtitle(" Spread of User Re-order Percent")+
  xlab("Reorder Percent")+
  ylab("Number of reorders at the percent")

# user reorder percent for specific product shows low value for most products(probably products 
# the user was never interested) 
instacart_data%>%
  group_by(re=round(user_prod_reord_pct,2))%>%
  summarize(count=n(), .groups='drop')%>%
  ggplot(aes(x=re, y=count))+
  geom_line()+
  ggtitle(" Spread of User Re-order Percent for specific product")+
  xlab("Reorder Percent")+
  ylab("Number of reorders at the percent")





#########################################################################

# Part 6: Model Analysis ( models.R file)

#########################################################################


# This part contains all the different models evaluated

# this is a list of all features 

# [1] "user_id"              "product_id"           "order_id"            
# [4] "buy"                  "user_avg_cart_size"   "prod_pop_n"          
# [7] "prod_reord_tot"       "prod_org_flag"        "prod_avg_freq"       
# [10] "produce_flag"         "dairy_eggs_flag"      "snacks_flag"         
# [13] "user_n_items"         "user_orders_n"        "user_avg_hr"         
# [16] "user_avg_day"         "user_avg_freq"        "user_reord_pct"      
# [19] "user_prod_reord_pct"  "user_prod_cart_order" "p rev_flag"           
# [22] "last3_reorder_pct"    "last3_flag"

# selected specific features based on correlation analysis
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
all_results<-data.frame(method="CART", f1score=tree_summary$f1score, 
                        acc=tree_summary$acc, precision=tree_summary$precision,
                        recall=tree_summary$recall, AUC=round(tree_auc@y.values[[1]],6))

# fine tuning of cp parameter
plot(model_tree)

# Classification Tree plot
plot(model_tree$finalModel)
text(model_tree$finalModel)

# Plot of ROC curve
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)



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
all_results<-rbind(all_results, data.frame(method="Naive Bayes", f1score=nb_summary$f1score, 
                                           acc=nb_summary$acc, precision=nb_summary$precision,
                                           recall=nb_summary$recall, AUC=round(nb_auc@y.values[[1]],6)))

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

all_results%>%knitr::kable()

##############################################################################
# Based on results summary the best Fscore is for the Naive Bayes model. Hence, this 
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

# run cutoff analysis for best model predictions
final_cutoff<-get_cutoff(nb_y) 

# predict outcomes based on new cutoff
final_y<-ifelse(nb_y>final_cutoff,1,0)

# final summary for train/test data
final_stats<-get_result_stats(final_y, evalset$buy)
final_stats

################################################################
# Test Dataset Results
################################################################

# Use Naive Bayes model to predict probabilities
test_pred_y<- predict(model_nb, testset, type="prob")[,2]

# Use cutoff obtained from analysis to assign labels
test_final_y<-ifelse(test_pred_y>final_cutoff,1,0)

# get final summary
test_stats<-get_result_stats(test_final_y, testset$buy)
test_stats

