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



