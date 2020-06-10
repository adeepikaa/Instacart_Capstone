# Total number of products ordered
order_products_prior_all%>%
  group_by(product_id, reordered)%>%
  summarize(n=n())

# Total number of products re-ordered
order_products_prior_all%>%
  group_by(product_id)%>%
  summarize(n=sum(reordered==1))%>%
  ggplot(aes(factor(product_id), n))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90, hjust=1))

# Top 15 add to cart order
order_products_prior_all%>%
  group_by(add_to_cart_order)%>%
  summarize(n=n())%>%
  slice(1:15)%>%
  ggplot(aes(as.factor(add_to_cart_order), n))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90, hjust=1))

# Spread of day of the week
order_products_prior_all%>%
  group_by(order_dow)%>%
  summarize(n=n())%>%
  ggplot(aes(as.factor(order_dow), n))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90, hjust=1))

# Spread of hour of the day
order_products_prior_all%>%
  group_by(order_hour_of_day)%>%
  summarize(n=n())%>%
  ggplot(aes(as.factor(order_hour_of_day), n))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90, hjust=1))

# Spread of the days since prior
order_products_prior_all%>%
  group_by(days_since_prior_order)%>%
  summarize(n=n())%>%
  ggplot(aes((days_since_prior_order), n))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90, hjust=1))

# Spread of cart size for reordered
order_products_prior_all%>%
  group_by(add_to_cart_order)%>%
  summarize(n=sum(reordered==1))%>%
  ggplot(aes(factor(add_to_cart_order), n))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90, hjust=1))

# Spread of day of the week for reordered
order_products_prior_all%>%
  group_by(order_dow)%>%
  summarize(n=sum(reordered==1))%>%
  ggplot(aes(factor(order_dow), n))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90, hjust=1))

# Spread of hour of the day for reordered
order_products_prior_all%>%
  group_by(order_hour_of_day)%>%
  summarize(n=sum(reordered==1))%>%
  ggplot(aes(factor(order_hour_of_day), n))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90, hjust=1))

# Spread of the days since prior order for reordered
order_products_prior_alll%>%
  group_by(days_since_prior_order)%>%
  summarize(n=sum(reordered==1))%>%
  ggplot(aes(factor(days_since_prior_order), n))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90, hjust=1))


# number of products per user
order_products_prior_all%>%
  group_by(user_id)%>%
  summarize(n=n())%>%
  arrange(desc(n))%>%
  slice(1:40)%>%
  ggplot(aes(user_id, n))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90, hjust=1))


# Most ordered products
order_products_prior_all%>%
  group_by(product_id)%>%
  summarize(n=n(), product=product_name)%>%  
  arrange(desc(n))%>%
  slice(1:40)%>%
  ggplot(aes(product, n))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90, hjust=1))


# Most ordered departments
order_products_prior_all%>%
  group_by(department_id)%>%
  summarize(n=n(), dept=department_name)%>%
  arrange(desc(n))%>%
  slice(1:40)%>%
  ggplot(aes(dept, n))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90, hjust=1))