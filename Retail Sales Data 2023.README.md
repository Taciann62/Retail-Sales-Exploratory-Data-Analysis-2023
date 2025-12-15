# Retail Sales Data 2023 

## Introduction

### This data analytics project is performed as an assigned task in fulfilment of the Oasis Infobyte internship program (December Batch 2025).
### The data was provided by the team through an open-source data platform, Kaggle. It contains a sales record of a retail store from 2023, alongside key fields such as Customer Gender, Product Name, Age, Product type, and the amount and Quantity purchased by each customer.

## Project Overview
### The Retail store collected a well-rounded dataset of its customers to get insight into their customer behaviour towards the products sold and, in that way, make a more informed business decision. The goal is to perform exploratory data analysis (EDA) to uncover patterns, trends, and insights that can help the retail business make informed decisions.

### Through the analysis of this data, insights will also be provided into customer purchasing behaviour over the year 2023, with each month clearly represented.

## My Role:

### On this project, I assume the role of the data analyst to uncover these insights, provide recommendations that influence decision-making.

## Data Analytics Objective:

 - Data Loading and Cleaning: Load the retail sales dataset.
 - Descriptive Statistics: Calculate basic statistics (mean, median, mode, standard deviation).
 - Time Series Analysis: Analyse sales trends over time using time series techniques.
 - In Customer and Product Analysis: Analyse customer demographics and purchasing behaviour.
 - Visualisation: Present insights through bar charts, line plots, and heatmaps.
 - Recommendations: Provide actionable recommendations based on the EDA.


## Business Question:
### Sales performance
   - What product generates the most revenue
   - What days are the busiest?
   - What month has the busiest sales?
   - What is the product revenue trend in the year 2023?
   - How did the customers behave towards the products(Gender & Age segment)

## Data Analytics steps taken:
- Data pulling and wrangling: Following the link provided, the dataset was accessible on Kaggle, downloaded from the source as a zip folder. After which, it was unzipped, and I conducted a preliminary review of the fields and records on Google Sheet to get a clear understanding of the dataset and its content.
- I proceeded to work on the date, on Google sheet, and formatted the date column to consist of Day-of-week, Day, Month, and Year.
- Then, I filtered through the rows to ensure consistency of records. All these were done before proceeding to R studio. You may decide to move straight to R Studio. However, I often prefer to have a direct overview of datasets on a spreadsheet before analysing on other platforms such as BigQuery or R studio.


### Tools used in this Analysis
- Google Sheet - Data set preview and partial formatting
- R Studio - Data Analysis
- Tableau - Data Visualisation

## Data Analysis and Wrangling Procedures on R Studio
#### Start by installing the necessary packages for this data analysis process.

```R
Install.packages("tidyverse")
Install.packages("dplyr")
Install.packages("skimr")
Install.packages("janitor")
Install.packages("lubridate")
```

#### Load the library of the installed packages.

```R
library(tidyverse)
library(dplyr)
library(lubridate)
library(skimr)
library(janitor)
library(readr)
library(tibble)
```

#### View the retail sales data to see the rows and columns it contains. Recall, I have formatted the date in the spreadsheet.

```R
View(Retail_sales)
```

#### To locate duplicate rows
```R
unique(Retail_Sales_Data_retail_sales_dataset)
```

#### To remove duplicates and have only a unique set
```R
Retail_Sales<-duplicate(Retail_Sales_Data_retail_sales_dataset) *<- renames the dataset*
```

```R
head(Retail_sales)` *<-shows the first 10 rows of the dataset*

names(Retail_sales)` *<-shows the column names*
```

- Transaction_ID
- Day_of_Week
- Day
- Month
- Year
- Customer_ID
- Gender
- Age
- Product_Category
- Quantity
- Price_per_unit
- Total_Amount

#### Note: The product _category are 3: *Beauty, Clothing, Electronics*
  ```R
  dim(Retail_sales) *<- shows the last few rows of the dataset*

 str_length(Retail_sales$Age)
```

## Find duplicates and inconsistencies, missing values 

```R
   is.na(Retail_sales) *<- checks for mssing values*

unique(Retail_sales)
```

## Exploratory Data Analysis

### Calculate the Total Amount to Ensure Accuracy.

```R
Retail_sales$Total_Amount<-(Retail_sales$Quantity*Retail_sales$Price_per_Unit)
```

#### 1. How much was generated overall in 2023? 
```R
Total_rev_2023<-sum(Retail_sales$Total_Amount)
```
##### Result =  456,000

#### 2. How much is the average amount generated in 2023?
```R
Avg_rev_2023<-mean(Retail_sales$Total_Amount)
```
##### Result = 456

### Create Table of Revenue Generated
```R
Retail_Revenue<-Retail_sales %>%
summarise(Average_Revenue= mean(Total_Amount), Total_Revenue_2023=sum(Total_Amount),
  Median_purchase=median(Total_Amount), Standard_DV= sd(Total_Amount), .groups = "drops")
  ```

### Calculate the revenue for each product to get the Average, Average Quantity, Total Amount, Total Quantity, and Total Frequency by Product.

```R
Total_spent_on_product<-Retail_sales %>%
group_by(Product_Category) %>%
 summarise(Average_amount=round(mean(Total_Amount),2), average_purchase=round(mean(Quantity), 2),
  total_amount=sum(Total_Amount), total__Quantity_sold=sum(Quantity),transaction_count=n(),.groups = "drop")
```


### Calculate the revenue generated by each month to gain insight into product performance.
##### This answers the business question: How much was generated, total quantity purchased & total count per month?

```R
Total_Purchases_Per_Month<-Retail_sales %>% 
  group_by(Month) %>%
  summarise(total_amount=sum(Total_Amount), transaction_count=n(), total_quantity=sum(Quantity))
```

### Calculate the revenue generated by each month to gain insight into product performance grouped by product category.
```R
Total_Per_month_Product<-Retail_sales %>% 
  group_by(Month, Product_Category) %>%
  summarise(total_amount=sum(Total_Amount), average_purchase=round(mean(Total_Amount),2), total_quantity=sum(Quantity), transaction_count=n(), .groups =  "drop") %>% 
  arrange(desc(total_amount))
```

## Calculate the revenue generated by each day of week to gain insight into product performance.

```R
Total_Purchases_DOW<-Retail_sales %>% 
  group_by(Day_of_Week) %>%
  summarise(total_amount=sum(Total_Amount), average_purchase=round(mean(Quantity), 2), transaction_count=n(), total_quantity=sum(Quantity))
``` 

## Calculate the revenue generated by each month to gain insight into product performance grouped by product category.

```R
Total_Purchases_DOW_Product<-Retail_sales %>% 
  group_by(Day_of_Week, Product_Category) %>%
  summarise(total_amount=sum(Total_Amount),average_purchase=round(mean(Total_Amount),2), transaction_count=n(), total_quantity=sum(Quantity), average_qty=round(mean(Quantity),2), .groups = "drop")
```

## Customer Age group categorisation or segmentation.
### Having reviewed the dataset, I observed that the age ranges from 18-64. To analyse customer behaviour by age group, I performed an age segmentation grouping the ages thus
- Age 18 - 24 = Young Adult
- Age 25 - 34 = Middle Adult
- Age 35 - 44 = Established Consumer
- Age 45 - 54 = Late Career
- Age 55 - 64 = Pre_retirement

### R code for adequate age segmentation
```R
Retail_sales_data_updated<-Retail_sales %>% 
  mutate(age_segment= case_when(Age>=18 & Age <=24 ~"1. Young Adult(18-24)", Age>=25 & Age <=34 ~"2. Middle Adult(25-34)", 
    Age>=35 & Age<=44 ~"3. Established Consumer(35-44)", Age>=45 & Age<=54~"4. Late Career(45-54)", Age>=55 & Age<=64 ~"5. Pre_retirement(55-64)", TRUE ~ "00. Unknown"))
```

## Customer Purchasing Behaviour Based on Age Segment
```R
Customer_Behaviour<- Retail_sales_data_updated %>% 
  group_by(age_segment) %>% 
  summarise(total_spent=sum(Total_Amount), average_purchase=round(mean(Total_Amount, na.rm=TRUE),2), 
   transaction_count=n(), avg_quantity=mean(Quantity), total_quantity=sum(Quantity),.groups = "drop")
```

## Customer Purchases per Month Grouped by Age Segment
```R
Agegroup_purchase_per_month<-Retail_sales_data_updated %>% 
  group_by(Month, age_segment) %>% 
  summarise(total_spent_per_month = sum(Total_Amount), average_amount=round(mean(Total_Amount),2), total_quantity_customer=sum(Quantity), .groups = "drop")
```

## Customer Purchases per age group 
```R
Agegroup_purchase_product<-Retail_sales_data_updated %>% 
  group_by(Product_Category, age_segment) %>% 
  summarise(total_AMOUNT = sum(Total_Amount), average_amount=round(mean(Total_Amount),2), total_quantity_customer=sum(Quantity), .groups = "drop")
```

## Gender 
```R
Gender_behaviour<- Retail_sales_data_updated %>% 
  group_by(Gender) %>% 
  summarise(total_spent=sum(Total_Amount), average_purchase=round(mean(Total_Amount),2), 
            transaction_count=n(), total_quantity=sum(Quantity), .groups = "drop")
```

```R
Gender_product<- Retail_sales_data_updated %>% 
  group_by(Gender, Product_Category) %>% 
  summarise(total_spent=sum(Total_Amount), average_purchase=round(mean(Total_Amount),2), 
            transaction_count=n(), total_quantity=sum(Quantity),.groups = "drop")
```


## Exporting required tables in CSV for Visualisation

```R
write.csv(Agegroup_purchase_product, "Agegroup_Product_Purchase.csv", row.names = FALSE)

write.csv(Agegroup_purchase_per_month, "Monthly_Agegroup_Trend.csv", row.names =  FALSE)

write.csv(Customer_Behaviour, "Customer_Behaviour.csv", row.names = FALSE)

write.csv(Total_Per_month_Product, "Monthly_Product_Purchase.csv", row.names = FALSE)

write.csv(Gender_product, "Gender_Product_Purchase.csv", row.names = FALSE)

write.csv(Total_spent_on_product, "Retail_Sales_revenue.csv", row.names = FALSE)

write.csv(Total_Purchases_DOW_Product, "Sales_Trend_DOW.csv", row.names = FALSE)

write.csv(Retail_Revenue, "KPI Cards.csv", row.names = FALSE)
```











