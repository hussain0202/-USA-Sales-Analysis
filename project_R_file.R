install.packages("tidyverse")
library(tidyverse)
sales_data<-read.csv("sales.csv")
View(sales_data)

library(statsr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(lubridate)

#Data Cleaning.....................
#dropping waste no use columns

new_sales_data<-sales_data[,!names(sales_data) %in% c("Name.Prefix","User.Name","E.Mail","Middle.Initial","Name.prefix","sku","full_name","Last.Name","ref_num","Zip","Phone.No.","SSN")]
View(new_sales_data)

#null values
sum(is.null(new_sales_data$discount_amount))
sum(is.na(new_sales_data))

#sales
#Sales = Number of Units Sold * Average Selling Price Per Unit
#adding sales column in data set

sales<-new_sales_data
sales<-mutate(new_sales_data,sale=(qty_ordered*price)-discount_amount,cost_of_goods=(price*0.4),revenue=(qty_ordered*price))
sales<-mutate(sales,profit=revenue*0.6)

View(sales)



#high no. of order on the base of region
high_sale<-new_sales_data %>% group_by(Region) %>% summarise(count=n_distinct(order_id))
View(high_sale)

ggplot(high_sale,aes(x=Region, y=count, label=count)) + 
  geom_col(width=0.5, position='dodge',fill="skyblue") +
  geom_text(position=position_dodge(0.5), vjust=-0.25) + 
  labs(y="Total No.Order", x="Region",title="Region Wise Sale")+
  theme_minimal()

#getting  frequency of categories in data_set
ggplotly(
  ggplot(new_sales_data, aes(x =category, fill = category)) +
    geom_bar() +
    
    labs(x = "Category", y = "Frequency",title = "Frequency of Category"))

#status of orders

modified_status<-filter(new_sales_data, status %in%  c("received","canceled","cod","complete","order_refunded","refund"))
View(modified_status)
#we use filter because there are too many statuses which have exist but in very less count like  closed,holded,payment_review,pending,pending_paypal,processing

ggplotly(
  ggplot(modified_status, aes(x =status, fill = status)) +
    geom_bar() +
    labs(x = "Order Status", y = "Count of Orders",title = "Status of Orders"))



# Q.most preferred mode for payment

modified_payment<-new_sales_data%>%group_by(payment_method)%>%tally()
View(modified_payment)

rank_modified_payment<-modified_payment%>%arrange(desc(n))%>%head(5)
#we filtered the data because some of the payment methods are not used more than once in payment so thats why we dropped it.
ggplotly(ggplot(rank_modified_payment,aes(x=payment_method, y=n,label=n)) + 
  geom_col(width=0.5, position='dodge') +
  geom_text(position=position_dodge(0.5), vjust=-0.25) + 
  labs(y="Count", x="Payment Method",title=" Modes of Payment")+
  theme_minimal()+theme_gray())


#Q.most ordered categories....................

modified_category<-new_sales_data%>%group_by(category)%>%tally()

#in this we ranked top5 category in desc order
rank_modified_category<-modified_category%>%arrange(desc(n))%>%head(5)
View(modified_category)
View(rank_modified_category)

ggplotly(ggplot(rank_modified_category,aes(x=category, y=n,label =n)) + 
  geom_col(width=0.5, position='dodge',fill="purple") +
  geom_text(position=position_dodge(0.5), vjust=-0.25) + 
  labs(y="Total No.Order", x="Category",title="Category Wise Orders")+
  theme_minimal()+theme_dark())

#Q.customer getting most of the orders depending on one_last month
#it means we get count of orders based on the last month given in the data frame as it is sep so we do around it
#top5 most frequent customers

max_month<-max(sales$month)
max_month
#Here we find maximum of month so that we can get the latest month>>then we count who is the customer who placed most of orders in last latest month
most_order <-new_sales_data %>%
 group_by(cust_id,First.Name) %>%filter(month==max_month)%>%
 summarize(count_orders = n_distinct(order_id))
View(most_order)

#ranking the top customers now
rank_most_order<-most_order%>%arrange(desc(count_orders))%>%head(5)

View(rank_most_order)

ggplotly(ggplot(rank_most_order,aes(x=First.Name, y=count_orders, label=count_orders)) + 
  geom_col(width=0.5, position='dodge',fill="orange") +
  geom_text(position=position_dodge(0.5), vjust=-0.25) + 
  labs(y="Order_Count", x="Customer_Name",title="Top 5 Cust. with Highest Orders in last Month")+
  theme_minimal())


#top 5 cities with highest sales
View(top_sale_city<-sales%>%group_by(City)%>%summarise(sum=sum(sale)))
rank_modified_city<-top_sale_city%>%arrange(desc(sum))%>%head(5)
View(rank_modified_city)

library(scales)
ggplotly(
ggplot(rank_modified_city,aes(x=City, y=sum, label=sum)) + 
  geom_col(width=0.5, position='dodge',fill="orange") +
  geom_text(position=position_dodge(0.5), vjust=-0.25) + 
  labs(y="Sale_Generated", x="City_Name",title="top 5 cities with highest sales")+
  theme_grey()+
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-6)))
View(new_sales_data)

#Highest Orders in which Month?
now<-sales%>%group_by(month)%>%summarise(m_orders=n_distinct(order_id))#%>%arrange(desc(m_orders))
View(now)

ggplotly(
  ggplot(now,aes(x = month,y=m_orders,fill = month)) + 
    geom_bar(stat="identity")+
    geom_text(aes(label=m_orders),vjust=0)+
    labs(title = "",y="No of Orders",x="Month"))

#top 5 customer who highly spent avg_money per order
avg_money_spent<-sales %>%
  mutate(subtotal = qty_ordered * price) %>%
  group_by(First.Name)%>%
  summarise(avg_purchase_values = as.integer(mean(subtotal)))
View(avg_money_spent)

rank_avg_money_spent<-avg_money_spent%>%arrange(desc(avg_purchase_values))%>%head(5)
View(rank_avg_money_spent)

ggplotly(ggplot(rank_avg_money_spent,aes(x = First.Name,y=avg_purchase_values,fill = First.Name)) + 
  geom_bar(stat="identity")+
  geom_text(aes(label=avg_purchase_values),vjust=-0.25)+
  labs(title = "",x="Customer Name",y="Avg Purchase Value"))


#Count of customers in different regions
count_by_region<-sales%>%group_by(Region)%>%count(cust_id)%>%summarise(count_of_cust=sum(n)                                                                       )

ggplotly(ggplot(count_by_region,aes(x = Region,y=count_of_cust,fill = Region)) + 
  geom_bar(stat="identity")+
  geom_text(aes(label=count_of_cust),vjust=-0.25)+
  labs(x="Region",y="Count of Customers"))






install.packages("corrplot")
library(corrplot)

#correlation between price and discount
data<-data.frame(sales$price,sales$discount_amount)
cor(data)
View(data)
corrplot(cor(data),method = "circle")

plot(x = sales$sale,y = sales$discount_amount,
     xlab = "dics",ylab = "sale",main = "...")
abline(lm(sales$discount_amount ~ sales$sale))
abline(lm(sales$discount_amount ~ sales$sale), col='blue' , lty='dashed')

pairs(~sale+profit+discount_amount+price,data = sales,
      main = "...")

print("...")
#correlation between qty_ordered and discount amount given
data2<-data.frame(sales$qty_ordered,sales$discount_amount)
corrplot(cor(data2),method = "circle")

#correlation between qty_ordered and revenue generated
data3<-data.frame(sales$sale,sales$discount_amount)
corrplot(cor(data3),method = "circle")

data3<-data.frame(sales$profit,sales$sale)
corrplot(cor(data3),method = "circle")


#all correlationship
datall<-data.frame(sales$sale,sales$discount_amount,sales$profit)
corrplot(cor(datall),method = "circle")

dev.off()


#comparison of sales over MoM
salee3data<-sales
View(salee3data)

salee3data$order_date=as.Date(salee3data$order_date)

daily_orders2 <- salee3data %>%mutate(YearMonth2 = format(order_date, "%Y-%m"))
View(daily_orders2)

class(daily_orders2$order_date)
monthly_orders2<-daily_orders2%>%group_by(YearMonth2)%>%summarize(Monthlyorders2 = sum(sale))%>%arrange(YearMonth2)
View(monthly_orders2)

monthly_report2<- monthly_orders2 %>%mutate(MoM2 = (Monthlyorders2 - lag(Monthlyorders2)) / lag(Monthlyorders2))
monthly_report2 <- monthly_report2 %>%mutate(MoM2 = round(MoM2 * 100, 1))

View(monthly_report2)

class(monthly_report2$MoM2)
class(monthly_report2$YearMonth2)

monthly_report2$YearMonth2=as.Date(paste0(monthly_report2$YearMonth2,"-01"))

ggplotly(
  ggplot(monthly_report2, aes(x=YearMonth2, y=MoM2)) +geom_line(color="blue")+geom_point()+labs(x="Month",y="MoM Changes"))


#monthly active customer.......................


monthly_cust<-sales%>%
  mutate(order_date=as.Date(order_date))%>%
  mutate(monthy=format(order_date, "%Y-%m"))%>%
  group_by(monthy)%>%
  summarise(month_cus=n_distinct(cust_id))%>%
  arrange(monthy)
View(monthly_cust)

monthly_cust$monthy=as.Date(paste0(monthly_cust$monthy,"-01"))
class(monthly_cust$monthy)

ggplotly(
  ggplot()+
    geom_line(data=monthly_cust,mapping = aes(x=monthy,y=month_cus),color="blue")+
    geom_point(data=monthly_cust,mapping = aes(x=monthy,y=month_cus),color="blue")+
    labs(x="Month",y="Active Customer"))



#.....................................................................................
#.................................churn rate................................................
#.....................................................................................

#count of new customers pr month
fminrank <- sales %>%
  mutate(order_date=as.Date(order_date))%>%
  mutate(monthy=format(order_date, "%Y-%m"))%>%
  group_by(cust_id) %>%
  mutate(minrnk = min_rank((order_date)))%>%
  ungroup()%>%
  group_by(monthy)%>%
  filter(minrnk==1)%>%
  summarise(new_cust=n_distinct(cust_id))
View(fminrank)


fminrank$monthy=as.Date(paste0(fminrank$monthy,"-01"))
class(df$monthy)

#merging two data_frame on same column
df = merge(x=monthly_cust,y=fminrank,by="monthy")

df=mutate(df,prev_month_cust=lag(month_cus))
View(df)

#calculation of retention_rate and churn_rate....................
df=mutate(df,retention_rate=(month_cus-new_cust)/prev_month_cust)
df=mutate(df,churn_rate=1-retention_rate)
View(df)


#visuallization

#churn rate of customer
ggplotly(
  ggplot(df, aes(x=monthy, y=churn_rate)) +geom_line()+geom_point()+labs(x="Month",y="Churn Rate"))

#retention rate
ggplotly(
  ggplot(df, aes(x=monthy, y=retention_rate)) +geom_line()+geom_point()+labs(x="Month",y="retention Rate"))

#comparison of churn rate  vs retention rate 
ggplotly(ggplot()+
           geom_line(data=df,mapping = aes(x=monthy,y=churn_rate),color="blue")+
           geom_point(data=df,mapping = aes(x=monthy,y=churn_rate),color="blue")+
           geom_line(data=df,mapping = aes(x=monthy,y=retention_rate),color="red")+
           geom_point(data=df,mapping = aes(x=monthy,y=retention_rate),color="red"))



#>>>>>>>>>>>>>>>>>>>>>>>>>> >>><<::Life Time Value::<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#average purchase frequency................#

avg_purchase_frequency<-sales%>%
  mutate(order_date=as.Date(order_date))%>%
  mutate(monthy=format(order_date, "%Y-%m"))%>%
  group_by(monthy,cust_id)%>%
  summarise(n_transaction = n_distinct(order_id))%>%
  ungroup()%>%
  group_by(monthy)%>%
  summarise(avg_purchase_freq = mean(n_transaction))

View(avg_purchase_frequency)

#...............................................................

#purchase value..............#

avg_purchase_value<-sales %>%
  mutate(order_date=as.Date(order_date))%>%
  mutate(monthy=format(order_date, "%Y-%m"))%>%
  mutate(subtotal = qty_ordered * price) %>%
  group_by(monthy)%>%
  summarise(avg_purchase_values = as.integer(mean(subtotal)))
View(avg_purchase_value)

View(sales)


#df is calculated in churn rate portion
profy <- df
View(profy)

#average per month lifespan

profy2<-profy%>%mutate(profy,life_span=(1/churn_rate))
View(profy2)

#we did this in below step to make one column of same type with same syntax to apply join on them 
avg_purchase_frequency$monthy=as.Date(paste0(avg_purchase_frequency$monthy,"-01"))
avg_purchase_value$monthy=as.Date(paste0(avg_purchase_value$monthy,"-01"))

library(plyr)
joined_data<-join_all(list(avg_purchase_frequency,avg_purchase_value,profy2), by = 'monthy', type = 'full')
View(joined_data)

joined_data2<-select(joined_data,monthy,avg_purchase_freq,avg_purchase_values,life_span)
View(joined_data2)


joined_data2=mutate(joined_data2,Life_Time_Value=avg_purchase_freq*avg_purchase_values*life_span*0.4)
View(joined_data2)

#visualization of Life time Value............................................

ggplotly(ggplot()+
           geom_line(data=joined_data2,mapping = aes(x=monthy,y=Life_Time_Value),color="blue")+
           geom_point(data=joined_data2,mapping = aes(x=monthy,y=Life_Time_Value),color="blue")+labs(x="Month",y="Avg Life Time Value"))


#CLV <- avg_purchase_frequency * avg_purchase_value * cus_lifespan * (0.3)

#.......................................................................................................................................
#.......................................RFM Analyses..............................................
#......................................................................................................................................
library(dplyr)
r_sales<-sales
View(r_sales)

r_sales$order_date<- as.Date(r_sales$order_date, "%Y-%m-%d")

analysis_date <- max(r_sales$order_date)
rfm_df <- r_sales %>% 
  group_by(cust_id) %>% 
  dplyr::summarise(Recency = as.integer(analysis_date- max(order_date)), Frequency = n(), Monetary = sum(total))

nrow(rfm_df)
View(rfm_df)

library(gridExtra)

#distribution of the three quantities calculated here.

r <- ggplot(rfm_df) +geom_density(aes(x= Recency))
f <- ggplot(rfm_df) +geom_density(aes(x = Frequency))
m <- ggplot(rfm_df) +geom_density(aes(x = Monetary))
View(m)
grid.arrange(r, f, m, nrow = 3)

summary(rfm_df)


rfm_df$R_score <- 0

rfm_df$R_score[rfm_df$Recency >= 277.0] <- 1
rfm_df$R_score[rfm_df$Recency >= 187.0 & rfm_df$Recency <277.0] <- 2
rfm_df$R_score[rfm_df$Recency >= 133.0 & rfm_df$Recency <187.0] <- 3
rfm_df$R_score[rfm_df$Recency < 133.0] <- 4

rfm_df$F_score<- 0

rfm_df$F_score[rfm_df$Frequency >=4] <- 4
rfm_df$F_score[rfm_df$Frequency <4 & rfm_df$Frequency >= 2.0] <- 3
rfm_df$F_score[rfm_df$Frequency <2.0 & rfm_df$Frequency >= 1.0] <- 2
rfm_df$F_score[rfm_df$Frequency <1.0] <- 1

rfm_df$M_score <- 0

rfm_df$M_score[rfm_df$Monetary >= 2348.5] <- 4
rfm_df$M_score[rfm_df$Monetary < 2348.5 & rfm_df$Monetary >= 467.9] <- 3
rfm_df$M_score[rfm_df$Monetary >= 127.5 & rfm_df$Monetary < 467.9] <- 2
rfm_df$M_score[rfm_df$Monetary <127.5] <- 1


rfm_df <- rfm_df %>% mutate(RFM_score = 100 *R_score +10 * F_score + M_score)


rfm_df$Segment <- "0"
rfm_df$Segment[which(rfm_df$RFM_score %in% c(444,434,443, 344, 442, 244, 424, 441  ))] <-"Loyalists"
rfm_df$Segment[which(rfm_df$RFM_score %in% c(332,333,342, 343, 334, 412,413,414,431,432,441,421,422,423, 424, 433 ))] <- "Potential Loyalists"
rfm_df$Segment[which(rfm_df$RFM_score %in% c(233,234, 241,311, 312, 313,314,321,322,323,324, 331,  341))] <- "Promising"
rfm_df$Segment[which(rfm_df$RFM_score %in% c(124, 133, 134, 142, 143, 144, 214,224,234, 242, 243, 232 ))] <- "Hesitant"
rfm_df$Segment[which(rfm_df$RFM_score %in% c(122, 123,131 ,132, 141, 212, 213, 221, 222, 223, 231 ))] <- "Need attention"
rfm_df$Segment[which(rfm_df$RFM_score %in% c(111, 112, 113, 114, 121, 131, 211, 311, 411 ))] <-"Detractors"

table(rfm_df$Segment)

library(plotrix)
x <- table(rfm_df$Segment)
View(x)
piepercent<- round(100*x/sum(x), 1)
lbls = paste(names(x), " ", piepercent,"%")
pie(x, labels = lbls, main = "Pie chart for Customer Segments", explode = 0.1)
