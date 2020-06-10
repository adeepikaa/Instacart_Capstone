

cor_table<-cor(instacart_data[,c(5:28)])

col<- colorRampPalette(c("blue", "white", "red"))(20) 
heatmap(cor_table, col=col, symm=TRUE)



