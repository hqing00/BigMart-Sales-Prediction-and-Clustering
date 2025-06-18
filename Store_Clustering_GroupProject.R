library(tidyverse)
library(ggplot2)
library(cluster)
library(factoextra)
library(gridExtra)

# Load Dataset
df <- read.csv("imputed_train.csv")


#Set datapoint to store granularity
store_performance <- df %>%
  group_by(Outlet_Identifier, Outlet_Type, Outlet_Size, 
           Outlet_Location_Type, Outlet_Establishment_Year) %>%
  summarise(
    Total_Sales = sum(Item_Outlet_Sales, na.rm = TRUE),
    Avg_Sales_Per_Item = mean(Item_Outlet_Sales, na.rm = TRUE),
    Sales_Consistency = sd(Item_Outlet_Sales, na.rm = TRUE),
    Total_Items = n(),
    Unique_Item_Types = n_distinct(Item_Type),
    Avg_Item_Price = mean(Item_MRP, na.rm = TRUE),
    Price_Range = max(Item_MRP, na.rm = TRUE) - min(Item_MRP, na.rm = TRUE),
    Avg_Item_Visibility = mean(Item_Visibility, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Store_Age = 2025 - Outlet_Establishment_Year,
    Product_Diversity_Ratio = Unique_Item_Types / Total_Items,
    Performance_Index = Total_Sales / mean(Total_Sales),
    Consistency_Score = 1 / (1 + (Sales_Consistency / Avg_Sales_Per_Item))
  )

#Select key features for clustering
clustering_features <- store_performance %>%
  select(
    Total_Sales,
    Avg_Sales_Per_Item,
    Product_Diversity_Ratio,
    Store_Age,
    Consistency_Score,
    Avg_Item_Price 
  )

#Normalize data
scaled_features <- scale(clustering_features)
rownames(scaled_features) <- store_performance$Outlet_Identifier

set.seed(123)

#k_optimal is directly set at 3 as it creates meaningful business performance tiers (High/Medium/Low)
k_optimal <- 3

#Performing Hierarchical Clustering
dist_matrix <- dist(scaled_features)
hc_result <- hclust(dist_matrix, method = "ward.D2")
hc_clusters <- cutree(hc_result, k = k_optimal)

store_performance$Cluster <- as.factor(hc_clusters)

#Visualisation
consistency_performance_viz <- store_performance %>%
  ggplot(aes(x = Consistency_Score, y = Performance_Index, color = Cluster)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_text(aes(label = Outlet_Identifier), vjust = -0.8, size = 3) +
  labs(title = "Operational Consistency vs Performance",
       x = "Consistency Score",
       y = "Sales Performance Index") +
  theme_minimal()

consistency_performance_viz

# Rank outlets by performance
sales_percentiles <- quantile(store_performance$Total_Sales, c(0.33, 0.67))
performance_percentiles <- quantile(store_performance$Performance_Index, c(0.33, 0.67))

success_ranking <- store_performance %>%
  arrange(desc(Performance_Index)) %>%
  select(Outlet_Identifier, Outlet_Type, Total_Sales, Performance_Index, Cluster) %>%
  mutate(
    Success_Likelihood = case_when(
      Performance_Index > performance_percentiles[2] ~ "High",
      Performance_Index > performance_percentiles[1] ~ "Medium", 
      TRUE ~ "Low"
    ),
    Rank = row_number()
  )

print(success_ranking)
