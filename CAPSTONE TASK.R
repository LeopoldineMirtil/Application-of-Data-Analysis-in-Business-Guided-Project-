###PROBLEM STATEMENT:
###You are provided with a dataset of a departmental store. 
###It contains details of products from May, 2020, a period marked by covid-19.
###Your manager wants you to find out that investing in which products will be more profitable.
###Your objective is to analyse the patterns and trends of the products, and gather insights for 
###strategic decision making.(At this level, you don’t need to make reports/recommendations now.
###You will build reports and make recommendations in future, with the knowledge you gained from 
###this project.)



###LOAD THE PACKAGE
require(ggplot2)
library(corrplot)
library(ggcorrplot)
library(tidyverse)


###LOAD THE DATASET
setwd("C:/Users/Leopoldine/Desktop/Mine/Coding Projects & Portfolio/Guided Projects/R Guided Project/Application of Data Analysis in Business with R Programming/Department Store Project")

store <- read.csv("FINAL DEPARTMENTAL STORE.csv")


######## PART 1: DATA MANIPULATION
###i. ARRANGE YOUR DATASET IN DESCENDING ORDER OF PROFIT
###ii. USE FIRST 360 ROWS 

store1 <- arrange(store, desc(PROFIT))%>%
  slice_head(n=360) 






###iii. FIND THE AVERAGE, MAXIMUM AND MINIMUM OF PROFIT GROUPED BY PRODUCT CATEGORY

store1 %>%
group_by(PRODUCT_CATEGORY)%>%
  summarise(AVG=mean(PROFIT), MAX=max(PROFIT), MIN=min(PROFIT)) %>%
  print(n=38)






######## PART 2: DATA VISUALIZATION
### i.BUILD A COLUMN PLOT FOR AVERAGE_NET_PROFIT & COMPANY

store1 %>% 
  group_by(COMPANY, PRODUCT_TYPE) %>% 
  summarise(AVERAGE_NET_PROFIT=mean(NET_PROFIT)) %>%
  ggplot(aes(x=COMPANY, y=AVERAGE_NET_PROFIT, fill=PRODUCT_TYPE))+
  geom_col() +
  scale_y_continuous(labels = scales::comma) 




### ii. BUILD A SCATTER PLOT FOR SELLING_PRICE & QUANTITY_DEMANDED

store1 %>%
  ggplot(aes(x=QUANTITY_DEMANDED, y=SELLING_PRICE, color= PRODUCT_CATEGORY))+
  geom_point()


### COLUMN CHART OF PROD TYPE & AVERAGE NET PROFIT

store1 %>% 
  group_by(PRODUCT_TYPE) %>% 
  summarise(AVERAGE_NET_PROFIT=mean(NET_PROFIT, na.rm = TRUE)) %>%
  arrange(desc(AVERAGE_NET_PROFIT)) %>%
  ggplot(aes(x=fct_reorder(PRODUCT_TYPE, +AVERAGE_NET_PROFIT), y=AVERAGE_NET_PROFIT))+
  geom_col(fill="cadetblue3") +
  xlab("PRODUCT_TYPE") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip()




### HISTOGRAM FOR PROFIT PERCENT AND PRODUCT TYPE

store1 %>%
  ggplot(aes(x=PROFIT_PERCENT, fill=PRODUCT_TYPE))+geom_histogram(binwidth = 20)



### PIE CHART FOR  PRODUCT CATEGORIES (OF TOP 3 PROD TYPES) & TOTAL NET PROFIT

store1 %>%
  filter(PRODUCT_TYPE=="hygiene" | PRODUCT_TYPE=="baby product" |PRODUCT_TYPE=="household") %>%
  group_by(PRODUCT_CATEGORY) %>%
  summarise(TOTAL_NET_PROFIT=sum(NET_PROFIT)) %>%
  arrange(desc(PRODUCT_CATEGORY)) %>%
  mutate(percentage=round(TOTAL_NET_PROFIT*100/sum(TOTAL_NET_PROFIT))) %>% 
  mutate(y_pos = cumsum(percentage)-0.5*percentage) %>%
  ggplot(aes(x="", percentage, fill=PRODUCT_CATEGORY))+
  geom_bar(width = 1, stat = "identity", color= "white", alpha=.5)+
  coord_polar("y", start=0)+
  geom_text(aes(y=y_pos, label=paste0(percentage, "%")), color="black")+
  scale_fill_manual(values=rainbow(10))+ theme_void()
  



######## PART 3:CORRELATION

#### i.BUILD A CORRELATION MATRIX OF THE COMPLETE DATASET

store<-dplyr::select_if(store, is.numeric)
r<-cor(store, use="complete.obs")
round(r,2)


#### ii.PLOT THE HEAT MAP OF THE CORRELATION MATRIX  

ggcorrplot(r)



