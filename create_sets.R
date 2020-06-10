

instacart_data<-order_products_prior_all%>%
  distinct(user_id,product_id)%>%
  left_join(order_products_train_all, by=c("user_id", "product_id"))%>%
  select(user_id, product_id, order_id, buy=reordered)

instacart_data$buy<-ifelse(is.na(instacart_data$buy),0,1)%>%factor(.)

instacart_data<-instacart_data%>%
  left_join(cart_size, by.x=user_id, by.y=user_id)%>%
  left_join(prod_features, by.x=product_id, by.y=product_id)%>%
  left_join(user_features, by.x=user_id, by.y=user_id)%>%
  left_join(user_prod_features, by=c("user_id", "product_id"))%>%
  left_join(lastorder, by=c("user_id", "product_id"))%>%
  left_join(last3order, by=c("user_id", "product_id"))



#instacart_data$prod_org_flag<-instacart_data$prod_org_flag%>%factor(.)
instacart_data$prev_flag<-ifelse(is.na(instacart_data$prev_flag),0,1)
#%>%factor(.)

instacart_data$last3_flag<-ifelse(is.na(instacart_data$last3_flag),0,1)
instacart_data$last3_reorder_pct<-
  ifelse(is.na(instacart_data$last3_reorder_pct),
         0,instacart_data$last3_reorder_pct)

instacart_data<-instacart_data[,-26] # deleting order number
sum(is.na(instacart_data))
sum(is.na(instacart_data$order_id)) 


rm(user_prod_features, cart_size, user_features, prod_features, last3order, lastorder, products_all)
gc()


user_ids<-orders%>%
  filter(eval_set=="train")%>%
  .$user_id

set.seed("123")
index <- createDataPartition(y = user_ids, times = 1, p = 0.1, list = FALSE)
user_ids<-user_ids[index]

set.seed("123")
test_index <- createDataPartition(y = user_ids, times = 1, p = 0.2, list = FALSE)

temp <- user_ids[-test_index]
user_id_test <- user_ids[test_index]


set.seed("456")
eval_index <- createDataPartition(y = temp, times = 1, p = 0.2, list = FALSE)

user_id_train <- temp[-eval_index]
user_id_eval <- temp[eval_index]

intersect(user_id_eval, user_id_test)
intersect(user_id_eval, user_id_train)

rm(temp, test_index, eval_index,index)



trainset<-instacart_data%>%
  inner_join(data.frame(user_id=user_id_train), by.x=user_id, by.y=user_id)

evalset<-instacart_data%>%
  inner_join(data.frame(user_id=user_id_eval), by.x=user_id, by.y=user_id)

testset<-instacart_data%>%
  inner_join(data.frame(user_id=user_id_test), by.x=user_id, by.y=user_id)

nrow(trainset)
nrow(testset)
nrow(evalset)
length(unique(trainset$user_id))
length(unique(testset$user_id))
length(unique(evalset$user_id))

 #write.csv(instacart_data, "instacart_features.csv")
