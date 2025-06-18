# Import data
library(tidyverse)
library(ggplot2)
library(factoextra)
library(cluster)
library(dplyr)
library(gridExtra)
library(plotly)

# Load Dataset
df <- read.csv("C:/Users/Yap Hui Qing/Downloads/WQD7004 Programming for Data Science/R Project/imputed_train.csv")
print(dim(df))
print(summary(df))

# Select the item features
item_features <- df[, c("Item_Weight", "Item_Visibility", "Item_MRP", "Item_Outlet_Sales")]

# Normalize data
scale_features <- item_features
final_scale_features <- scale(scale_features)

# Find the optimum number of clusters (k) using Elbow and Silhouette Methods
fviz_nbclust(final_scale_features, kmeans, method = "wss") + 
  labs(subtitle = "Elbow Method for Optimal K")

fviz_nbclust(final_scale_features, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette Method for Optimal K")

# Perform K-means clustering with the optimal number of clusters
set.seed(123)

# K = 2
kmeans_result_2 <- kmeans(final_scale_features, centers = 2, nstart = 25)
plot1 <- fviz_cluster(kmeans_result_2, geom = "point", data = final_scale_features) + ggtitle("K = 2")

# K = 3
kmeans_result_3 <- kmeans(final_scale_features, centers = 3, nstart = 25)
plot2 <- fviz_cluster(kmeans_result_3, geom = "point", data = final_scale_features) + ggtitle("K = 3")

# K = 4
kmeans_result_4 <- kmeans(final_scale_features, centers = 4, nstart = 25)
plot3 <- fviz_cluster(kmeans_result_4, geom = "point", data = final_scale_features) + ggtitle("K = 4")

# K = 5
kmeans_result_5 <- kmeans(final_scale_features, centers = 5, nstart = 25)
plot4 <- fviz_cluster(kmeans_result_5, geom = "point", data = final_scale_features) + ggtitle("K = 5")

# Plot comparison
grid.arrange(plot1, plot2, plot3, plot4, nrow = 2)

# Add cluster labels to final_scale_features
df_clustered <- as.data.frame(final_scale_features)
df_clustered$cluster_2 <- as.factor(kmeans_result_2$cluster)
df_clustered$cluster_3 <- as.factor(kmeans_result_3$cluster)
df_clustered$cluster_4 <- as.factor(kmeans_result_4$cluster)
df_clustered$cluster_5 <- as.factor(kmeans_result_5$cluster)

# Apply PCA to the scaled features
pca_result <- prcomp(final_scale_features)

# Combine PCA results with cluster labels
pca_df <- as.data.frame(pca_result$x[, 1:2])  # Use first two PCs
pca_df$Cluster <- df_clustered$cluster_3

# PCA + Cluster Visualization
ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "Clusters Visualized with PCA",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()

# Copy the original dataset
final_df <- df

# Add cluster assignments to the original dataset
final_df$K2 <- as.factor(kmeans_result_2$cluster)
final_df$K3 <- as.factor(kmeans_result_3$cluster)
final_df$K4 <- as.factor(kmeans_result_4$cluster)
final_df$K5 <- as.factor(kmeans_result_5$cluster)

cluster_summary <- final_df %>%
  group_by(K2) %>%
  summarise(
    Avg_Weight = mean(Item_Weight, na.rm = TRUE),
    Avg_Visibility = mean(Item_Visibility, na.rm = TRUE),
    Avg_MRP = mean(Item_MRP, na.rm = TRUE),
    Avg_Sales = mean(Item_Outlet_Sales, na.rm = TRUE),
    Count = n()
  )
print(cluster_summary)

# Cluster-wise Average Sales
avg_sales <- final_df %>%
  group_by(K2) %>%
  summarise(Avg_Sales = mean(Item_Outlet_Sales, na.rm = TRUE))

# Average sales for each clusters
ggplot(avg_sales, aes(x = K2, y = Avg_Sales, fill = K2)) +
  geom_col() +
  labs(title = "Average Sales by Cluster", x = "Cluster", y = "Avg Sales") +
  theme_minimal()

# Total number of items in each cluster
barplot1 <- ggplot(final_df, aes(x = K2, fill = K2)) +
    geom_bar() +
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
    labs(title = "Number of Items in Each Cluster (K2)", x = "Cluster", y = "Count") +
    theme_minimal()

barplot2 <- ggplot(final_df, aes(x = K3, fill = K3)) +
    geom_bar() +
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
    labs(title = "Number of Items in Each Cluster (K=3)", x = "Cluster", y = "Count") +
    theme_minimal()

barplot3 <- barplot2 <- ggplot(final_df, aes(x = K4, fill = K4)) +
    geom_bar() +
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
    labs(title = "Number of Items in Each Cluster (K=4)", x = "Cluster", y = "Count") +
    theme_minimal()

barplot4 <- ggplot(final_df, aes(x = K5, fill = K5)) +
    geom_bar() +
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
    labs(title = "Number of Items in Each Cluster (K=5)", x = "Cluster", y = "Count") +
    theme_minimal()

# Arrange bar plots in a grid
grid.arrange(barplot1, barplot2, barplot3, barplot4, nrow = 2)

# Item Type Distribution per Cluster
heatmap_itemtype <- function(data, cluster_col) {
    type_cluster_counts <- data %>%
        group_by(!!sym(cluster_col), Item_Type) %>%
        summarise(Count = n(), .groups = "drop")
    
    ggplot(type_cluster_counts, aes_string(x = cluster_col, y = "Item_Type", fill = "Count")) +
        geom_tile() +
        labs(title = paste("Heatmap of Item Types by", cluster_col), x = "Cluster", y = "Item Type") +
        scale_fill_gradient(low = "white", high = "blue") +
        theme_minimal()
}
hm1 <- heatmap_itemtype(final_df, "K2")
hm2 <- heatmap_itemtype(final_df, "K3")
hm3 <- heatmap_itemtype(final_df, "K4")
hm4 <- heatmap_itemtype(final_df, "K5")

# Arrange heatmaps in a grid
grid.arrange(hm1, hm2, hm3, hm4)