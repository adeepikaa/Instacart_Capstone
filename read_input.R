aisles = read.csv('data/aisles.csv')
departments = read.csv('data/departments.csv')
products = read.csv('data/products.csv')
orders = read.csv('data/orders.csv')
order_products_prior = read.csv('data/order_products__prior.csv')
order_products_train = read.csv('data/order_products__train.csv')

library(dplyr)
library(caret)
library(e1071) 
library(randomForest)
library(ROCR)
library(kernlab)
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
# Clean orders table to remove test data 

sum(orders$eval_set=="test")
sum(orders$eval_set=="train")
sum(orders$eval_set=="prior")



products_all<-products%>%
  left_join(aisles, by.x=aisle_id, by.y=id)%>%
  left_join(departments, by.x=department_id, by.y=id)


order_products_train_all<-order_products_train %>%
  left_join(orders, by="order_id")%>%
  left_join(products_all, by="product_id")


order_products_prior_all<-order_products_prior %>%
  left_join(orders, by="order_id")%>%
  left_join(products_all, by="product_id") 

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
