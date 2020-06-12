cart_size<-order_products_prior_all%>% 
  group_by(order_id)%>%
  summarize(user_id=first(user_id), cart_size=max(add_to_cart_order), .groups='drop')%>%
  group_by(user_id)%>%
  summarize(user_avg_cart_size=ceiling(mean(cart_size)), .groups='drop')

###########################################
# Product features


prod_features<-order_products_prior_all%>%
  group_by(product_id)%>%
  summarize(
    prod_pop_n=n(), 
    prod_reord_tot=sum(reordered==1), 
    prod_org_flag=ifelse(grepl("Organic", first(product_name), fixed=TRUE),1,0),
    prod_avg_freq=ceiling(mean(days_since_prior_order, na.rm=TRUE)),
    .groups='drop')

prod_features$prod_avg_freq<-ifelse(is.na(prod_features$prod_avg_freq), 0, prod_features$prod_avg_freq)

order_products_prior_all%>%
  group_by(department_id, department)%>%
  summarize(n=n(), .groups='drop')%>%arrange(desc(n))%>%slice(1:3)

prod_features<-prod_features%>%
  left_join(products, by.x=product_id, by.y=product_id)

prod_features$produce_flag<-ifelse(prod_features$department_id==4, 1,0)
prod_features$dairy_eggs_flag<-ifelse(prod_features$department_id==16, 1,0)
prod_features$snacks_flag<-ifelse(prod_features$department_id==19, 1,0)

prod_features<-prod_features[,-c(6:8)]
#####################################################################
getmode <- function(v) {
  uniqv <- unique(v, na.rm=TRUE)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

user_features<-order_products_prior_all%>%
  group_by(user_id)%>%
  summarize(user_n_items=n(), 
            user_orders_n=max(order_number), 
            user_pop_hr=getmode(order_hour_of_day),
            user_pop_day=getmode(order_dow),
            user_pop_freq=getmode(days_since_prior_order),
            user_med_hr=median(order_hour_of_day),
            user_med_day=median(order_dow),
            user_med_freq=median(days_since_prior_order, na.rm=TRUE), 
            user_avg_hr=mean(order_hour_of_day),
            user_avg_day=mean(order_dow),
            user_avg_freq=ceiling(mean(days_since_prior_order, na.rm=TRUE)), 
            user_reord_pct=sum(reordered==1)/n(),
            .groups='drop')

user_features$user_pop_freq<-ifelse(is.na(user_features$user_pop_freq), 0, user_features$user_pop_freq)

#####################################################################

user_prod_features<-order_products_prior_all%>%
  group_by(user_id, product_id)%>%
  summarize(user_prod_reord_tot=sum(reordered==1), 
            .groups='drop') 

#####################################################################

last3order<-order_products_prior_all%>%
  group_by(user_id)%>%
  select(user_id, product_id, order_number, reordered)%>%
  filter(order_number==max(order_number)|order_number==max(order_number)-1|order_number==max(order_number)-2)%>%
  mutate(last3_flag=1)%>%
  group_by(user_id, product_id)%>%
  summarize(last3_reorder_pct=sum(reordered)/n(), 
            last3_flag=first(last3_flag), 
            .groups='drop')



lastorder<-order_products_prior_all%>%
  group_by(user_id)%>%
  select(user_id, product_id, order_number)%>%
  filter(order_number==max(order_number))%>%
  mutate(prev_flag=1)

#####################################################################