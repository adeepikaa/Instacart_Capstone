
str(instacart_data)
summary(instacart_data)

cor_table<-cor(instacart_data[,-c(1:4,8,10,11,12,21,23)])

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(cor_table, col=col, symm=TRUE)



instacart_data%>%
  group_by(user_avg_freq)%>%
  summarize(cart=mean(user_avg_cart_size), .groups='drop')%>%
  ggplot()+
  geom_bar(aes( user_avg_freq, cart), stat="identity")

instacart_data%>%
  group_by(re=round(user_reord_pct,2))%>%
  summarize(count=n(), .groups='drop')%>%
  ggplot(aes(x=re, y=count))+
  geom_line(size=2, col="blue")+
  ggtitle(" Spread of User Re-order Percent")+
  xlab("Reorder Percent")+
  ylab("Number of reorders at the percent")

instacart_data%>%
  group_by(re=round(user_prod_reord_pct,2))%>%
  summarize(count=n(), .groups='drop')%>%
  ggplot(aes(x=re, y=count))+
  geom_line()+
  ggtitle(" Spread of User Re-order Percent for specific product")+
  xlab("Reorder Percent")+
  ylab("Number of reorders at the percent")

#######################################################

