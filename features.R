# This file creates features required to do predictive analysis.

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

#####################################################################
