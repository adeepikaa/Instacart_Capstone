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
