#Introduction

###This data anaytlytics project is performed as an assigned task in fulfilment of the Oasis Inforbyte internship program (December Batch 2025).
###The data was provided by the team through an open source data platform, Kaggle 
###It contains a sales record of a retail store from the year 2023 alongsider key info like Gender, Product Name, Age, Product type and Amount and Quantity purchased by each customer.

##Project Overview
#The Retail store collected a well rounded data of their customers to get insgight into their customer beahaviour towards their products and in thatw ay make a more infomred business decsion.
###Through the analysis of this data, insights will be provided into customer purchasing behaviour through the year 2023 with each month clearly representated.

##My Role:
###On this project, I assume the role of the data analyst to uncover these insights,provide recommendations that influence decision making

##Retail Sales Data load libraries required for this data analysis process
library(tidyverse)
library(dplyr)
library(lubridate)
library(skimr)
library(janitor)
library(readr)
library(tibble)

readr
##View the retail sales data to see the rows and columns it contains.I have formatted the date in spreadsheet.

Retail_sales<-unique(Retail_Sales_Data_retail_sales_dataset)  #reassigned the data name


View(Retail_Sales_Data_retail_sales_dataset)

Retail_sales<-Retail_Sales_Data_retail_sales_dataset %>% 
  filter(Year==2023)

head(Retail_sales)

dim(Retail_sales)

names(Retail_sales)

unique(Retail_sales)

str_length(Retail_sales$Age)
is.na(Retail_sales)

str(Retail_sales$Year)

##Find duplicates and inconsistentcies


##Perform basic statistics on the Total amount spent

#calculate the total amount to ensure accuracy.
Retail_sales$Total_Amount<-(Retail_sales$Quantity*Retail_sales$Price_per_Unit)

##Sum of Total Amount
Total_rev_2023<-sum(Retail_sales$Total_Amount)

##Average Total Amount
Avg_rev_2023<-mean(Retail_sales$Total_Amount)

##missing values 

is.na(Retail_sales)

##Overall Calculation
Retail_Revenue<-Retail_sales %>%  
  summarise(Average_Revenue = mean(Total_Amount), Total_Revenue =sum(Total_Amount), Median_Revenue = median(Total_Amount), Standard_DV= sd(Total_Amount), Count = n(), Total_QTY = sum(Quantity), Avearge_QtY = mean(Quantity), .groups = "drops")

## Product Sales CAtegorization to determine price and purchase

##By product category
Total_spent_on_product<-Retail_sales %>% 
  group_by(Product_Category) %>% 
  summarise(total_amount=sum(Total_Amount), avg_amount=round(mean(Total_Amount),2), total__Quantity = sum(Quantity), avg_qty=round(mean(Quantity), 2) 
            ,transaction_count=n(),.groups = "drop")

##By month

Total_Purchases_Per_Month<-Retail_sales %>% 
  group_by(Month) %>%
  summarise(total_amount=sum(Total_Amount), avg_amount =mean(Total_Amount), total_qty = sum(Quantity)
            ,avg_quantity=mean(Quantity), transaction_count=n())

##By Product Type & Month
Total_Per_month_Product<-Retail_sales %>% 
  group_by(Month, Product_Category) %>%
  summarise(total_amount=sum(Total_Amount), avg_amount=round(mean(Total_Amount),2), avg_qty = mean(Quantity), total_quantity=sum(Quantity), transaction_count=n(), .groups =  "drop") %>% 
  arrange(desc(total_amount)) 

Statistics_Pricing<-Retail_sales %>% 
  group_by(Product_Category) %>%
  summarise(Avg_Price=mean(Price_per_Unit), total_price = sum(Price_per_Unit), average_amount=round(mean(Total_Amount),2), Avg_qty=round(mean(Quantity),2), total_amount= sum(Total_Amount), total_quantity=sum(Quantity), transaction_count=n(), .groups =  "drop")

DOW<-Retail_sales %>% 
  group_by(Day_of_Week) %>%
  summarise(Avg_Price=mean(Price_per_Unit), average_amount=round(mean(Total_Amount),2), Avg_qty=round(mean(Quantity),2), total_amount= sum(Total_Amount), total_quantity=sum(Quantity), transaction_count=n(), .groups =  "drop")

Product_DOW<-Retail_sales %>% 
  group_by(Product_Category,Day_of_Week) %>%
  summarise(Avg_Price=mean(Price_per_Unit), average_amount=round(mean(Total_Amount),2), Avg_qty=round(mean(Quantity),2), total_amount= sum(Total_Amount), total_quantity=sum(Quantity), transaction_count=n(), .groups =  "drop")

##By Day of week
Total_Purchases_DOW<-Retail_sales %>% 
  group_by(Day_of_Week) %>%
  summarise(total_amount=sum(Total_Amount), avg_amount= mean(Total_Amount), avg_qty=round(mean(Quantity), 2), transaction_count=n(), total_quantity=sum(Quantity)) 

##By Day of Week & Product    
Total_Purchases_DOW_Product<-Retail_sales %>% 
  group_by(Day_of_Week, Product_Category) %>%
  summarise(total_amount=sum(Total_Amount),avg_amount=round(mean(Total_Amount),2), transaction_count=n(), total_quantity=sum(Quantity), average_qty=round(mean(Quantity),2), .groups = "drop") 


##Customer Age group categorization
Retail_sales<-Retail_sales %>% 
  mutate(age_segment= case_when(Age>=18 & Age <=24 ~"1. Young Adult(18-24)", Age>=25 & Age <=34 ~"2. Middle Adult(25-34)", 
                                Age>=35 & Age<=44 ~"3. Established Consumer(35-44)", Age>=45 & Age<=54~"4. Late Career(45-54)", Age>=55 & Age<=64 ~"5. Pre_retirement(55-64)", TRUE ~ "00. Unknown"))

##Customer Purchasing Behaviour Based on Age_segment
Customer_Behaviour<- Retail_sales %>% 
  group_by(age_segment) %>% 
  summarise(total_amount=sum(Total_Amount), avg_amount=round(mean(Total_Amount),2), 
   transaction_count=n(), avg_quantity =mean(Quantity), total_quantity=sum(Quantity),.groups = "drop")

Gender_behaviour<- Retail_sales %>% 
  group_by(Gender) %>% 
  summarise(total_amount=sum(Total_Amount), avg_amount=round(mean(Total_Amount),2), 
            transaction_count=n(), avg_qty = mean(Quantity), total_qty=sum(Quantity), .groups = "drop")

Gender_product<- Retail_sales %>% 
  group_by(Gender, Product_Category) %>% 
  summarise(total_amount=sum(Total_Amount), avg_amount=round(mean(Total_Amount),2), 
            transaction_count=n(), avg_quantity = mean(Quantity),total_quantity=sum(Quantity),.groups = "drop")

##Customer Purchases per month
Agegroup_purchase_per_month<-Retail_sales %>% 
  group_by(Month, age_segment) %>% 
  summarise(total_spent_per_month = sum(Total_Amount), average_amount=round(mean(Total_Amount),2), avg_qty = mean(Quantity), total_quantity=sum(Quantity), .groups = "drop")

##Customer Purchases per age age group 
Agegroup_purchase_product<-Retail_sales %>% 
  group_by(Product_Category, age_segment) %>% 
  summarise(total_spent_per_month = sum(Total_Amount), average_amount=round(mean(Total_Amount, ),2), avg_qty = mean(Quantity), total_quantity=sum(Quantity), .groups = "drop")

##Exportin required tables in CSV for Visualization

###Age Group Purchase of product
write.csv(Retail_Revenue, "Retail_Revenue_01", row.names = FALSE)

write.csv(Retail_sales, "Retail_Sales_data_01", row.names = FALSE)

write.csv(Total_spent_on_product, "Retail_Sales_revenue_01.csv", row.names = FALSE)

write.csv(Total_Purchases_Per_Month, "Monthly_Purchase_01.csv", row.names = FALSE)

write.csv(Total_Per_month_Product, "Monthly_Product_Purchase_01.csv", row.names = FALSE)

write.csv(Statistics_Pricing, "Statistics_Pricing_01.csv", row.names =FALSE)

write.csv(DOW, "DOW-01.csv", row.names =FALSE)

write.csv(Total_Purchases_DOW_Product, "DOW_Puchase_Product_01.csv", row.names = FALSE)

write.csv(Customer_Behaviour, "Customer_Behaviour_01.csv", row.names = FALSE)

write.csv(Gender_behaviour, "Gender_Behaviour_01", row.names = FALSE)

write.csv(Gender_product, "Gender_Product_01.csv", row.names = FALSE)

write.csv(Agegroup_purchase_per_month, "Agegroup_Purchase_month_01", row.names =  FALSE)

write.csv(Agegroup_purchase_product, "Agegroup_purchase_product_01", row.names =  FALSE)














