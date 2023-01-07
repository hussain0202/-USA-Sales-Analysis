
#monthly active customer.......................

monthly_cust<-sales%>%
  mutate(order_date=as.Date(order_date))%>%
  mutate(monthy=format(order_date, "%Y-%m"))%>%
  group_by(monthy)%>%
  summarise(month_cus=n_distinct(cust_id))%>%
  arrange(monthy)
View(monthly_cust)


#Count of new customers per month
fminrank <- sales %>%
  mutate(order_date=as.Date(order_date))%>%
  mutate(monthy=format(order_date, "%Y-%m"))%>%
  group_by(cust_id) %>%
  mutate(minrnk = min_rank((order_date)))%>%
  ungroup()%>%
  group_by(monthy)%>%
  filter(minrnk==1)%>%
  summarise(new_cust=n_distinct(cust_id))

#merging two data_frame on same column

df = merge(x=monthly_cust,y=fminrank,by="monthy")

df=mutate(df,prev_month_cust=lag(month_cus))
View(df)

#calculation of retention_rate and churn_rate....................

df=mutate(df,retention_rate=(month_cus-new_cust)/prev_month_cust)
df=mutate(df,churn_rate=1-retention_rate)

