################################################################
# This file analyzes features

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

#######################################################

