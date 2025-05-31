imputed_train <- read.csv("imputed_train.csv")
imouted_test <- read.csv("imputed_test.csv")

#1 Top-selling products (by quantity and revenue

library(ggplot2)

ggplot(imputed_train, aes(x = Item_Weight, y = Item_Outlet_Sales)) +
geom_point() +
geom_smooth(method = "loess") +
labs(title = "Relationship Between Item Weight and Sales",
x = "Item Weight", y = "Item Outlet Sales")

print(paste("Correlation = ", cor(imputed_train$Item_Weight, imputed_train$Item_Outlet_Sales, use = "complete.obs"), 
            ". Item weight does not have a strong relationship with Sales", sep=""))


#2 Average MRP and average sales per product type

library(dplyr)

#######by MEAN

product_type_summary <- imputed_train %>%
  group_by(Item_Type) %>%
  summarize(
    Average_MRP = round(mean(Item_MRP, na.rm = TRUE), 2),
    Average_Sales = round(mean(Item_Outlet_Sales, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(Average_Sales))

print(product_type_summary)

correlation <- cor(product_type_summary$Average_MRP, product_type_summary$Average_Sales)

ggplot(product_type_summary, aes(x = Average_MRP, y = Average_Sales)) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(aes(label = Item_Type), check_overlap = TRUE, nudge_y = 10, size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_minimal() +
  labs(title = "Correlation between Average MRP and Average Sales",
       subtitle = paste("Correlation coefficient=", round(correlation, 3),", strong positive relationship"),
       x = "Average MRP", 
       y = "Average Sales")


########## by MEDIAN
product_type_summary <- imputed_train %>%
  group_by(Item_Type) %>%
  summarize(
    Median_MRP = round(median(Item_MRP, na.rm = TRUE), 2),
    Median_Sales = round(median(Item_Outlet_Sales, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(Median_Sales))

print(product_type_summary)

correlation <- cor(product_type_summary$Median_MRP, product_type_summary$Median_Sales)

ggplot(product_type_summary, aes(x = Median_MRP, y = Median_Sales)) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(aes(label = Item_Type), check_overlap = TRUE, nudge_y = 10, size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_minimal() +
  labs(title = "Correlation between Median MRP and Median Sales",
       subtitle = paste("Correlation coefficient =", round(correlation, 3), 
                        ", moderate positive relationship"),
       x = "Median MRP", 
       y = "Median Sales")


#3 Sales by fat content

ggplot(imputed_train, aes(x = Item_Fat_Content, y = Item_Outlet_Sales)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Distribution of Sales by Fat Content",
       subtitle = "Both distributions are right-skewed with outliers. No significant difference in sales performance between fat content types.",
       caption = "Conclusion: Both fat content types can achieve similar sales results",
       x = "Fat Content",
       y = "Sales") +
  theme(
    plot.subtitle = element_text(size = 10, face = "italic"),
    plot.caption = element_text(size = 10, face = "bold", hjust = 0)
  )

#4 Impact of product visibility on sales

visibility_correlation <- cor(imputed_train$Item_Visibility, imputed_train$Item_Outlet_Sales)

print(paste("Correlation between Item Visibility and Sales:", round(visibility_correlation, 3)))

ggplot(imputed_train, aes(x = Item_Visibility, y = Item_Outlet_Sales)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "red") +
  theme_minimal() +
  labs(title = "Impact of Product Visibility on Sales",
       subtitle = paste("Correlation:", round(visibility_correlation, 3),", weak negative relationship"),
       x = "Item Visibility",
       y = "Item Outlet Sales")






