#################################################

# This file has the code to create the sample 
# submission file for the Kaggle competition 

# The Fscore received was 0.362 

# create oreder_ids, user_ids for sample test set
test_sample<-orders%>%
  filter(eval_set=="test")%>%
  select(order_id, user_id)

# 75000 order ids that need prediction
length(unique(test_sample$user_id))

# uder ids of test set
sample_ids<-orders%>%
  filter(eval_set=="test")%>%
  .$user_id

# get features for these user ids
sample_set<-instacart_data%>%
  inner_join(data.frame(user_id=sample_ids), by.x=user_id, by.y=user_id)%>%
  select(-order_id)%>%
  left_join(test_sample, by.x=user_id, by.y=user_id)

# remove features not needed
sampleset<-sample_set%>%
  select(-prod_pop_n, -user_n_items)

# Use Naive Bayes model to predict probabilities
y<- predict(model_nb, sampleset[,-c(1:3)], type="prob")[,2]

# Use cutoff from Random forest analysis to cutoff the test dataset
sampleset$buy<-ifelse(y>final_cutoff,1,0)


# get products that will be ordered
sample_output<-sampleset%>%
  filter(buy==1)%>%
  group_by(order_id)%>%
  summarise(
    products = paste(product_id, collapse = " ")
  )


# sample format file
sample_orderids = read.csv('data/sample_submission.csv')

# create sample list
sample_submission<-sample_orderids%>%
  select(order_id)%>%
  left_join(sample_output, by.x=order_id, by.y=order_id)

# looks for any order ids not ordering any products 
sum(is.na(sample_submission$products))

# Add None to enpty orders
sample_submission$products<-ifelse(is.na(sample_submission$products), "None",
                                    sample_submission$products)

# write the csv format file
write.csv(sample_submission, "sample_submission.csv", row.names=F)
