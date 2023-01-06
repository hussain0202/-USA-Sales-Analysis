# Churn Rate & Retention Rate Calculation

The Churn rate is the rate at which customers stop doing business with an entity. It gives you insight into how many customers you may be losing in a given time period. Knowing how to calculate churn rate enables you to understand the financial health of a business and implement changes to help it grow. The Retention rate which states that it is a metric that measures the number of users still using your product or service after a given period of time.

It is useful for a company to compare churn rate with other competitors & see their performance in retaining customers compared to other 
companies in the same industry.It is important for a company to have low churn rate, because high churn rate implies high attrition, hence
 company with high churn rate needs to spend money to bring new customers on board just to maintain the vacuum created by customers 
leaving the company. Hence, it is important to understand the churn rate because it impacts profitability & growth for a company.

The Steps to calculate churn rate is given below:

- In the first step, calculate the monthly active customers . In this, I simply count the distinct customers with the group_by on month which gave me the monthly active customers.
- After that calculate the new customers per month . For this, first used min_rank window function which is used to rank the top values based on rank. I used it to find the new customers in every new month. Used the min_rank function on order_date function with the group_by on customers. Then I filtered the rank=1 to get the first orders of new customers with group_by on month. In the end, count the distinct customers.
- Merged the above two dataframes based on the same column month
- In the last, calculate the retention rate. Then subtract retention rate from 1 to get the churn rate.
