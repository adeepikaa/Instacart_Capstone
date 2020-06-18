# This file creates the dataset with the dependent variable and all the features

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
# order_products_train which has the nth orders which need to be predicted
# This huge dataset requires high RAM infrastructure that is currently unavailable 
# to the author. Hence, reduced the dataset to include 10% of the users in the analysis
# That means number of users used for predictive analysis is about 13K which comes to 
# about a million observations

# Though the user ids have been reduced all features related to products have been taken
# from the original dataset

# reducing user ids to 10%
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
trainset<-instacart_data%>%
  inner_join(data.frame(user_id=user_id_train), by.x=user_id, by.y=user_id)

evalset<-instacart_data%>%
  inner_join(data.frame(user_id=user_id_eval), by.x=user_id, by.y=user_id)

testset<-instacart_data%>%
  inner_join(data.frame(user_id=user_id_test), by.x=user_id, by.y=user_id)


nrow(trainset)
# trainset has 547802 observations

nrow(testset)
# testset has 166744 observations

nrow(evalset)
# evalset has 135834 observations


length(unique(trainset$user_id))
# trainset has  users

length(unique(testset$user_id))
# testset has users

length(unique(evalset$user_id))
# evalset has users

#write.csv(instacart_data, "instacart_features.csv")

rm(user_id_eval, user_id_test, user_id_train)
