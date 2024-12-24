library(tidyverse)
library(dplyr)
library(ggplot2)

library(readxl)

Adidas_sales_data <- read_excel("C:/Users/pg240/OneDrive/Desktop/Adidas_US_Sales_Datasets.xlsx")
View(Adidas_sales_data)

## view the first 10 rows of the dataset
View(head(Adidas_sales_data, 5))


## view the last rows of the dataset.
View(tail(Adidas_sales_data, 5))


## check the structure of the dataset.
str(Adidas_sales_data)

## summary statistics of the dataset.
print(summary(Adidas_sales_data))


## rename specific columns

Adidas_sales_data <- rename(Adidas_sales_data,
                      Product_name = Product ,
                      Retailer_name = Retailer,
                      Total_Sales = Total.Sales
                    )
View(Adidas_sales_data)


### Question 1
## identify the most common retailer who deals in adidas shoes
most_common_retailer_name <- Adidas_sales_data %>% 
  count(Retailer_name) %>% 
  arrange(desc(n)) %>% 
  slice(5)

print(most_common_retailer_name)
View(most_common_retailer_name)

### Question 2
## find the avg total sales by particular region

avg_total_sales_by_region <- Adidas_sales_data %>% 
  group_by(Region) %>% 
  summarize(avg_total_sales_by_region = mean(Total_Sales, na.rm = TRUE)) %>% 
  arrange(desc(avg_total_sales_by_region))


View(head(avg_total_sales_by_region))


### Question 3
## Identify the most common product name sold by adidas

most_common_product_name <- Adidas_sales_data %>% 
  count(Product_name) %>% 
  arrange(desc(n)) %>% 
  slice(5)

print(most_common_product_name)
View(most_common_product_name)


### Queston 4
## Find the top 5 highest selling products along with their unit sold.

top_5_highest_selling_products <- Adidas_sales_data %>%
  select(Product_name, Total_Sales, Units.Sold) %>%
  filter(!is.na(Total_Sales), !is.na(Units.Sold)) %>%
  arrange(desc(Total_Sales)) %>%
  slice(1:5)

View(top_5_highest_selling_products)


### Question 5
## find the top 5 lowest selling products along with their unit sold.

top_5_lowest_selling_products <- Adidas_sales_data %>%
  select(Product_name, Total_Sales, Units.Sold) %>%
  filter(!is.na(Total_Sales), !is.na(Units.Sold)) %>%
  arrange(Total_Sales) %>%
  slice(1:5)

View(top_5_lowest_selling_products)


### Question 6 
## Analyze the impact of sales method on total sales of adidas

Sales_Method_impact <- Adidas_sales_data %>% 
  group_by(Sales.Method) %>% 
  summarize(avg_total_sales = mean(Total_Sales, na.rm = TRUE))

View(Sales_Method_impact)



### Question 7
## Find the median of unit sold.

median_Units_sold<- median
(Adidas_sales_data$Units.Sold)
print(paste("Median Units Sold:
  ", median_Units_sold))


### Question 8 
## Find the mean of total sales

mean_total_sales <- mean(Adidas_sales_data$Total_Sales)
print(paste("Mean of Total Sales:", mean_total_sales))



### Queston 9 
## Calculate the range of operating profit

range_operating_profit <- range(Adidas_sales_data$Operating.Profit)
print(paste("Range of Operating Profit:", diff(range_operating_profit)))


### Question 10 
## Calculate the variance of the operating margin

var_operating_margin <- var(Adidas_sales_data$Operating.Margin)
print(paste("Variance of Operating margin:", var_operating_margin))

### Quesion 11
## Calculate Pearson's correlation

correlation <- cor(Adidas_sales_data$Units.Sold,
                   Adidas_sales_data$Total_Sales,
                   method = "pearson")
print(correlation)


### Question 12
### LINE CHART

Adidas_sales_dataset_clean <- Adidas_sales_data %>%
  filter(!is.na(Region), !is.na(Units.Sold)) %>%
  mutate(Units.Sold)  

# Create a line chart of Region wise Units sold

ggplot(Adidas_sales_data, aes(x = Region, y = Units.Sold)) +
  geom_line(color = "green", size = 2.5) + 
  labs(title = "Region wise units sold",
       x = "Region",
       y = "Units.Sold") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### Question 13
## SCATTER PLOT


Adidas_sales_dataset_clean <- Adidas_sales_data %>%
  filter(!is.na(Total_Sales), !is.na(Units.Sold))

# Create a scatter plot of Total_Sales vs. Units_Sold

ggplot(Adidas_sales_dataset_clean, aes(x = Total_Sales,
                                       y = Units.Sold)) +
  geom_point(aes(color = Product_name), size = 1) +  
  labs(title = "Scatter Plot of Total Sales vs.
       Units Sold",
       x = "Total Sales",
       y = "Units Sold") +
  theme_minimal()


### Question 14
##  BOX PLOT


Adidas_sales_dataset_clean <- Adidas_sales_data %>%
  filter(!is.na(Total_Sales), !is.na(Units.Sold))

# Boxplot for Total Sales by Product

ggplot(Adidas_sales_dataset_clean, aes(x = 
            Product_name, y = Total_Sales)) +
  geom_boxplot(aes(fill = Product_name)
               , color = "black") +
  labs(title = "Boxplot of Total Sales by Product",
       x = "Product ",
       y = "Total Sales") +
  theme(axis.text.x = element_text(angle 
                    = 45, hjust = 1))  


### Question 15
### BAR GRAPH

sales_summary <- Adidas_sales_data %>%
  group_by(Retailer_name) %>%
  summarise(Total_Sales = sum(Total_Sales
                  , na.rm = TRUE)) %>%
  arrange(desc(Total_Sales))

# Plotting the bar graph using ggplot2

ggplot(sales_summary, aes(x = reorder
    (Retailer_name, Total_Sales), y = Total_Sales, 
              fill = Retailer_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by retailers", 
       x = "Retailer Names", y 
       = "Total Sales") +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) 


### Question 16
## plotting histogram 


head(Adidas_sales_data)
ggplot(Adidas_sales_data, aes
       (x = Total_Sales)) +
  geom_histogram(binwidth = 100, fill = 
  "green", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Adidas Sales",
       x = "Total_sales",
       y = "Frequency") +
  theme_minimal()


### Question 17
## density

ggplot(Adidas_sales_data, aes
       (x = Region, fill = Total_Sales)) +
  geom_density(alpha = 1.5) +
  labs(title = "Density of Total sales
       by Region", x = "Region", 
       y = "Total_Sales")


### Question 18
## faceted chart

ggplot(Adidas_sales_data, aes
       (x = City, y = Total_Sales)) +
  geom_point(aes(color = Region),
             alpha = 0.7) +
  facet_wrap(~ Region) +
  labs(title = "City vs. Total sales
       - region wise", x = "City",
       y = "Total_Sales") +
  theme_minimal()

### Question 19
## Pie chart

product_sold_in_regions <- Adidas_sales_data %>%
  count(Region) %>%
  mutate(percentage = n / sum(n) * 100)
print(product_sold_in_regions )

# Create a pie chart with percentages

ggplot(product_sold_in_regions , aes(x = "", y = percentage, 
                                    fill = Region)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(percentage,0), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Proportion of Regions") +
  theme_void()

### Question 20
## Correlation Matrix

install.packages("corrplot")
library(corrplot)

cor_matrix <- cor(select_if(Adidas_sales_data
      , is.numeric), use = "complete.obs")
corrplot(cor_matrix, method = "circle")





