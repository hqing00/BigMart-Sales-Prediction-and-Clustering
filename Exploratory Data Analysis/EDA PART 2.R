#Load Dataset
library(readr)
df_imputed_train <- read_csv("imputed_train.csv")
View(df_imputed_train)

#Sales performance by store (Outlet_Identifier)
#Summary Statistics
library(dplyr)
all_outlets_sales_summary <- df_imputed_train %>%
       group_by(Outlet_Identifier) %>%
       summarise(
             Mean_Sales = mean(Item_Outlet_Sales, na.rm = TRUE),
             Median_Sales = median(Item_Outlet_Sales, na.rm = TRUE),
             SD_Sales = sd(Item_Outlet_Sales, na.rm = TRUE),
             Min_Sales = min(Item_Outlet_Sales, na.rm = TRUE),
             Max_Sales = max(Item_Outlet_Sales, na.rm = TRUE),
             Total_Sales = sum(Item_Outlet_Sales, na.rm = TRUE),
             N_Observations = n()
         ) %>%
       arrange(desc(Total_Sales))
 
print(all_outlets_sales_summary)


#Visualize Sales Performance by outlet
#convert the scientific notation to normal numerical display
options(scipen = 999)
format(1000000, big.mark = ",") 
#2 decimal places
format(1234567.89, big.mark = ",", nsmall = 2)

#Statistical Summary
summary(all_outlets_sales_summary$Total_Sales)

#Bar Chart plotting 
library(dplyr)
library(scales)
library(ggplot2)
all_outlets_sales_summary %>%
  mutate(Color = ifelse(Total_Sales == max(Total_Sales), "Top",
                        ifelse(Total_Sales == min(Total_Sales), "Bottom", "Normal"))) %>%
  ggplot(aes(x = reorder(Outlet_Identifier, -Total_Sales), y = Total_Sales, fill = Color)) +
  geom_col() +
  scale_fill_manual(values = c("Top" = "forestgreen", "Bottom" = "red", "Normal" = "grey70")) +
  labs(title = "Total Sales by Outlet",
       x = "Outlet",
       y = "Total Sales") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

#Pie Chart-% of Total Sales by Outlet
install.packages("ggrepel")
library(ggplot2)
library(ggrepel)
library(dplyr)

ggplot(sales_summary, aes(x = 1, y = Sales_Percent, fill = Outlet_Identifier)) +
  geom_col(color = "white", width = 1) +
  coord_polar(theta = "y") +
  geom_label_repel(
    aes(
      y = label_pos,
      label = Label,
      fill = Outlet_Identifier
    ),
    size = 3.5,
    nudge_x = 1.5,
    segment.color = "grey50",
    show.legend = FALSE,
    box.padding = 0.3,
    point.padding = 0.5,
    direction = "y",
    segment.size = 0.5,
    segment.curvature = 0.1,
    segment.ncp = 5,
    segment.angle = 20
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Sales Percentage by Outlet")

# Density Plot of Sales by Outlet
ggplot(df_imputed_train, aes(x = Item_Outlet_Sales, color = Outlet_Identifier)) +
  geom_density(size = 1) +
  labs(
    title = "Density of Sales by Store",
    x = "Sales",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  )
  



#Compare stores by size, type, and location
# Cross-tabulation: Outlet Size by Outlet Type
size_type_tbl <- df_imputed_train %>%
  count(Outlet_Size, Outlet_Type) %>%
  pivot_wider(names_from = Outlet_Type, values_from = n, values_fill = 0)

# Cross-tabulation: Outlet Size by Outlet Location Type
size_location_tbl <- df_imputed_train %>%
  count(Outlet_Size, Outlet_Location_Type) %>%
  pivot_wider(names_from = Outlet_Location_Type, values_from = n, values_fill = 0)

# Cross-tabulation: Outlet Type by Outlet Location Type
type_location_tbl <- df_imputed_train %>%
  count(Outlet_Type, Outlet_Location_Type) %>%
  pivot_wider(names_from = Outlet_Location_Type, values_from = n, values_fill = 0)

# View the tables
size_type_tbl
size_location_tbl
type_location_tbl

install.packages("gridExtra")
library(gridExtra)
library(ggplot2)

# Plot Size by Type
p1 <- ggplot(df_imputed_train, aes(x = Outlet_Size, fill = Outlet_Type)) +
  geom_bar(position = "dodge") +
  labs(title = "Store Size by Outlet Type", x = "Outlet Size", y = "Count") +
  theme_minimal()

# Plot Size by Location
p2 <- ggplot(df_imputed_train, aes(x = Outlet_Size, fill = Outlet_Location_Type)) +
  geom_bar(position = "dodge") +
  labs(title = "Store Size by Outlet Location", x = "Outlet Size", y = "Count") +
  theme_minimal()

# Plot Type by Location
p3 <- ggplot(df_imputed_train, aes(x = Outlet_Type, fill = Outlet_Location_Type)) +
  geom_bar(position = "dodge") +
  labs(title = "Outlet Type by Location Type", x = "Outlet Type", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Arrange plots side by side
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)


library(vcd)
# Mosaic plot for Outlet_Size vs Outlet_Type
mosaic(~ Outlet_Size + Outlet_Type, data = df_imputed_train,
       shade = TRUE, legend = TRUE,
       main = "Mosaic Plot: Outlet Size vs Outlet Type")
# Add legend using grid.text()
legend_text <- c(
  "a = Grocery",
  "b = Supermarket Type 1",
  "c = Supermarket Type 2",
  "d = Supermarket Type 3"
)

grid.text("Legend:", x = unit(0.85, "npc"), y = unit(0.9, "npc"),
          gp = gpar(fontsize = 12, fontface = "bold"))

# Add legend items one by one
for (i in seq_along(legend_text)) {
  grid.text(legend_text[i],
            x = unit(0.85, "npc"),
            y = unit(0.9 - i * 0.05, "npc"),
            just = "left",
            gp = gpar(fontsize = 8))
}

# Mosaic plot for Outlet_Size vs Location
mosaic(~ Outlet_Size + Outlet_Location_Type, data = df_imputed_train,
       shade = TRUE, legend = TRUE, main = "Mosaic Plot: Outlet Size vs Location")

# Mosaic plot for Outlet_Type vs Location
mosaic(~ Outlet_Type + Outlet_Location_Type, data = df_imputed_train,
       shade = TRUE, legend = TRUE, main = "Mosaic Plot: Outlet Type vs Location")

# Add legend using grid.text()
legend_text <- c(
  "a = Grocery",
  "b = Supermarket Type 1",
  "c = Supermarket Type 2",
  "d = Supermarket Type 3"
)

grid.text("Legend:", x = unit(0.85, "npc"), y = unit(0.9, "npc"),
          gp = gpar(fontsize = 12, fontface = "bold"))

# Add legend items one by one
for (i in seq_along(legend_text)) {
  grid.text(legend_text[i],
            x = unit(0.85, "npc"),
            y = unit(0.9 - i * 0.05, "npc"),
            just = "left",
            gp = gpar(fontsize = 8))
}




#Yearly revenue trend by store establishment year
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Summarize total and average sales per establishment year
sales_by_year <- df_imputed_train %>%
  group_by(Outlet_Establishment_Year) %>%
  summarise(
    Total_Sales = sum(Item_Outlet_Sales, na.rm = TRUE),
    Average_Sales = mean(Item_Outlet_Sales, na.rm = TRUE),
    Store_Count = n_distinct(Outlet_Identifier)
  ) %>%
  arrange(Outlet_Establishment_Year)

# Line plot: Total sales by establishment year
p1 <- ggplot(sales_by_year, aes(x = Outlet_Establishment_Year, y = Total_Sales)) +
  geom_line(group = 1, color = "steelblue", size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Total Sales by Store Establishment Year",
       x = "Year Established", y = "Total Sales") +
  theme_minimal()

# Bar plot: Average sales per establishment year
p2 <- ggplot(sales_by_year, aes(x = factor(Outlet_Establishment_Year), y = Average_Sales)) +
  geom_col(fill = "darkorange") +
  geom_text(aes(label = round(Average_Sales, 1)), vjust = -0.3) +
  labs(title = "Average Sales per Store by Establishment Year",
       x = "Year Established", y = "Average Sales") +
  theme_minimal()

# arrange plots side by side
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)




#Sales distribution by item category
library(ggplot2)
library(dplyr)

# Summarize total sales by Item_Type
sales_by_item <- df_imputed_train %>%
  group_by(Item_Type) %>%
  summarise(Total_Sales = sum(Item_Outlet_Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales))

# Bar plot of sales by item category
ggplot(sales_by_item, aes(x = reorder(Item_Type, Total_Sales), y = Total_Sales)) +
  geom_col(fill = "skyblue", color = "black") +
  coord_flip() +  # Flip for better readability
  labs(
    title = "Total Sales Distribution by Item Category",
    x = "Item Category",
    y = "Total Sales"
  ) +
  theme_minimal()

#pie chart of sales distribution by item category
sales_pie <- sales_total %>%
  mutate(
    Percent = Total_Sales / sum(Total_Sales),
    Label = paste0(Item_Type, "\n", round(Percent * 100, 1), "%")
  ) %>%
  # Sort for correct stacking and cumulative percentage calculation in polar coords
  arrange(desc(Percent)) %>% # IMPORTANT: arrange by Percent in descending order
  mutate(
    # Calculate the cumulative percentage, then find the midpoint of each slice
    cum_percent = cumsum(Percent) - (0.5 * Percent)
  )

# Pie chart
ggplot(sales_pie, aes(x = "", y = Percent, fill = Item_Type)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text_repel(
    aes(
      label = Label,
      y = cum_percent # Use the newly created 'cum_percent' for positioning
    ),
    size = 3,
    nudge_x = 1.2, # Increase nudge_x to push labels further out
    direction = "y", # Prioritize vertical repulsion
    segment.colour = "grey50", # Make segment lines a bit more subtle
    segment.size = 0.2,
    min.segment.length = 0.1, # Don't draw segments if very close
    box.padding = 0.6, # Increase padding around the text boxes
    point.padding = 0.2, # Increase padding around the point (slice)
    force = 1.5, # Increase repulsion force
    max.iter = 5000 # Increase max iterations for better placement
  ) +
  labs(
    title = "Sales Distribution by Item Category",
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  theme(legend.position = "none")



#Load library and price elasticity: How does MRP affect units sold?
library(tidyverse)
library(ggplot2)
library(corrplot)
library(GGally)

#Create Units_Sold
df_imputed_train <- df_imputed_train %>%
  mutate(Units_Sold = Item_Outlet_Sales / Item_MRP)

#Basic summary statistics
summary(df_imputed_train %>% select(Item_MRP, Item_Outlet_Sales, Units_Sold))
skimr::skim(df_imputed_train %>% select(Item_MRP, Units_Sold)) 


# Histogram of Item MRP
ggplot(df_imputed_train, aes(x = Item_MRP)) + 
  geom_histogram(bins = 30, fill = "skyblue", color = "black") + 
  ggtitle("Distribution of Item MRP") +
  theme_minimal()

# Histogram of Units Sold
ggplot(df_imputed_train, aes(x = Units_Sold)) + 
  geom_histogram(bins = 30, fill = "orange", color = "black") + 
  ggtitle("Distribution of Units Sold") +
  theme_minimal()

#Scatter plot: MRP vs Units Sold
ggplot(df_imputed_train, aes(x = Item_MRP, y = Units_Sold)) +
       geom_point(alpha = 0.5) +
       geom_smooth(method = "lm", color = "red", se = TRUE) +
       ggtitle("Price vs Units Sold") +
       xlab("Item MRP") + ylab("Units Sold")

# Correlation
correlation <- cor(df_imputed_train$Item_MRP, df_imputed_train$Units_Sold, use = "complete.obs")
print(paste("Correlation between Item MRP and Units Sold:", round(correlation, 3)))

#Linear regression to estimate elasticity
model <- lm(log(Units_Sold) ~ log(Item_MRP), data = df_imputed_train)
summary(model)

#Plot the log-log relationship
ggplot(df_imputed_train, aes(x = log(Item_MRP), y = log(Units_Sold))) +
       geom_point(alpha = 0.5) +
       geom_smooth(method = "lm", color = "blue", se = TRUE) +
       ggtitle("Log-Log Plot: Price Elasticity") +
       xlab("log(Item MRP)") + ylab("log(Units Sold)")




#High vs low visibility items: Do highly visible items sell more?
#Summary statistics
summary(df_imputed_train %>% select(Item_Visibility, Item_Outlet_Sales))

#Visualize distributions
ggplot(df_imputed_train, aes(x = Item_Visibility)) +
       geom_histogram(bins = 30, fill = "lightblue", color = "black") +
       ggtitle("Distribution of Item Visibility")
ggplot(df_imputed_train, aes(x = Item_Outlet_Sales)) +
       geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
       ggtitle("Distribution of Item Outlet Sales")

#Scatter plot: Visibility vs Sales
ggplot(df_imputed_train, aes(x = Item_Visibility, y = Item_Outlet_Sales)) +
       geom_point(alpha = 0.5, color = "darkblue") +
       geom_smooth(method = "lm", color = "red", se = TRUE) +
       ggtitle("Item Visibility vs Item Outlet Sales") +
       xlab("Item Visibility") + ylab("Item Outlet Sales")

#Correlation
correlation <- cor(df_imputed_train$Item_Visibility, df_imputed_train$Item_Outlet_Sales, use = "complete.obs")
print(paste("Correlation between Item Visibility and Sales:", round(correlation, 3)))

#Create visibility category: High vs Low (using median split)
median_visibility <- median(df_imputed_train$Item_Visibility, na.rm = TRUE)
df_imputed_train <- df_imputed_train %>%
       mutate(Visibility_Level = ifelse(Item_Visibility >= median_visibility, "High", "Low"))

#Boxplot: Sales by visibility level
ggplot(df_imputed_train, aes(x = Visibility_Level, y = Item_Outlet_Sales, fill = Visibility_Level)) +
       geom_boxplot() +
       ggtitle("Sales by Item Visibility Level") +
       ylab("Item Outlet Sales") +
       xlab("Visibility Level")

#Compare mean sales
visibility_sales_summary<- df_imputed_train %>%
       group_by(Visibility_Level) %>%
       summarise(
             Mean_Sales = mean(Item_Outlet_Sales, na.rm = TRUE),
             Median_Sales = median(Item_Outlet_Sales, na.rm = TRUE),
             Count = n()
         )
print(visibility_sales_summary)

#Statistical test (t-test)
t_test_result <- t.test(Item_Outlet_Sales ~ Visibility_Level, data = df_imputed_train)




#Which item types drive the most revenue?
library(forcats)

#Check basic structure and summary
summary(df_imputed_train %>% select(Item_Type, Item_Outlet_Sales))
table(df_imputed_train$Item_Type)
item_sales_summary <- df_imputed_train %>%
       group_by(Item_Type) %>%
       summarise(
             Total_Sales = sum(Item_Outlet_Sales, na.rm = TRUE),
             Average_Sales = mean(Item_Outlet_Sales, na.rm = TRUE),
             Count = n()
         ) %>%
       arrange(desc(Total_Sales))
print(item_sales_summary)

#Bar plot – Total sales by Item_Type
library(tidyverse)

ggplot(item_sales_summary, aes(x = fct_reorder(Item_Type, Total_Sales), y = Total_Sales)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  ggtitle("Total Sales by Item Type") +
  xlab("Item Type") +
  ylab("Total Sales") +
  theme_minimal()

#Boxplot – Distribution of sales by Item_Type
ggplot(df_imputed_train, aes(
  x = fct_reorder(Item_Type, Item_Outlet_Sales, .fun = median), 
  y = Item_Outlet_Sales, 
  fill = Item_Type
)) +
  geom_boxplot(show.legend = FALSE) +
  coord_flip() +
  ggtitle("Sales Distribution by Item Type") +
  xlab("Item Type") +
  ylab("Item Outlet Sales") +
  theme_minimal()

# Density plot – Sales distribution by Item Type
ggplot(df_imputed_train, aes(x = Item_Outlet_Sales, fill = Item_Type)) +
  geom_density(alpha = 0.4) +
  ggtitle("Sales Density by Item Type") +
  xlab("Item Outlet Sales") +
  theme_minimal() +
  theme(legend.position = "right")

#Item sales summary 
item_sales_summary <- df_imputed_train %>%
       group_by(Item_Type) %>%
       summarise(
             Total_Revenue = sum(Item_Outlet_Sales, na.rm = TRUE),
             Average_Revenue = mean(Item_Outlet_Sales, na.rm = TRUE),
             Count = n()
        ) %>%
       arrange(desc(Total_Revenue))
print(item_sales_summary)

#Bar plot of total revenue by item type
ggplot(item_sales_summary, aes(x = fct_reorder(Item_Type, Total_Revenue), y = Total_Revenue)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Total Revenue by Item Type",
    x = "Item Type",
    y = "Total Revenue"
  ) +
  theme_minimal()




#Which outlet types perform best in sales volume and revenue?
# Create 'Units_Sold' column
df_imputed_train <- df_imputed_train %>%
       mutate(Units_Sold = Item_Outlet_Sales / Item_MRP)
#Summary statistics by Outlet_Type
outlet_sales_summary <- df_imputed_train %>%
       group_by(Outlet_Type) %>%
       summarise(
             Total_Revenue = sum(Item_Outlet_Sales, na.rm = TRUE),
             Average_Revenue = mean(Item_Outlet_Sales, na.rm = TRUE),
             Total_Volume = sum(Units_Sold, na.rm = TRUE),
             Average_Volume = mean(Units_Sold, na.rm = TRUE),
             Count = n()
         ) %>%
       arrange(desc(Total_Revenue))
print(outlet_sales_summary)

# Bar plot - Total Revenue by Outlet Type
ggplot(outlet_sales_summary, aes(x = fct_reorder(Outlet_Type, Total_Revenue), y = Total_Revenue)) +
     geom_col(fill = "steelblue") +
     coord_flip() +
     labs(title = "Total Revenue by Outlet Type",
                       x = "Outlet Type", y = "Total Revenue")

#Bar plot - Total Volume by Outlet Type
ggplot(outlet_sales_summary, aes(x = fct_reorder(Outlet_Type, Total_Volume), y = Total_Volume)) +
       geom_col(fill = "darkgreen") +
       coord_flip() +
       labs(title = "Total Units Sold by Outlet Type",
                       x = "Outlet Type", y = "Estimated Units Sold")

#Boxplot - Revenue distribution by Outlet Type
ggplot(df_imputed_train, aes(x = Outlet_Type, y = Item_Outlet_Sales, fill = Outlet_Type)) +
       geom_boxplot() +
       labs(title = "Revenue Distribution by Outlet Type", x = "Outlet Type", y = "Item Outlet Sales")

#Boxplot - Volume distribution by Outlet Type
ggplot(df_imputed_train, aes(x = Outlet_Type, y = Units_Sold, fill = Outlet_Type)) +
       geom_boxplot() +
       labs(title = "Sales Volume Distribution by Outlet Type", x = "Outlet Type", y = "Estimated Units Sold")




#How does outlet size or location affect sales?
sales_by_size <- df_imputed_train %>%
       group_by(Outlet_Size) %>%
       summarise(
             Total_Revenue = sum(Item_Outlet_Sales, na.rm = TRUE),
             Average_Revenue = mean(Item_Outlet_Sales, na.rm = TRUE),
             .groups = "drop"
        )
print(sales_by_size)

sales_by_location <- df_imputed_train %>%
     group_by(Outlet_Location_Type) %>%
     summarise(
         Total_Revenue = sum(Item_Outlet_Sales, na.rm = TRUE),
         Average_Revenue = mean(Item_Outlet_Sales, na.rm = TRUE),
         .groups = "drop"
       )
print(sales_by_location)




#What pricing strategies correlate with higher sales?
#Create Units_Sold column
df_imputed_train <- df_imputed_train %>%
       mutate(Units_Sold = Item_Outlet_Sales / Item_MRP)

# Bin Item_MRP into 4 tiers using quartiles
df_imputed_train <- df_imputed_train %>%
       mutate(
             MRP_Tier = cut(
                   Item_MRP,
                   breaks = quantile(Item_MRP, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                   labels = c("Low", "Medium", "High", "Premium"),
                   include.lowest = TRUE
              )
        )

#Summarize revenue and volume by MRP_Tier
pricing_summary <- df_imputed_train %>%
       group_by(MRP_Tier) %>%
       summarise(
             Total_Revenue = sum(Item_Outlet_Sales, na.rm = TRUE),
             Avg_Revenue = mean(Item_Outlet_Sales, na.rm = TRUE),
             Total_Volume = sum(Units_Sold, na.rm = TRUE),
             Avg_Volume = mean(Units_Sold, na.rm = TRUE),
             .groups = "drop"
        )
# View summary
print(pricing_summary)

# Scatterplot: Item MRP vs Revenue
ggplot(df_imputed_train, aes(x = Item_MRP, y = Item_Outlet_Sales)) +
       geom_point(alpha = 0.4, color = "steelblue") +
       geom_smooth(method = "lm", se = FALSE, color = "darkred", linewidth = 1.2) +
       labs(
             title = "Correlation Between Item MRP and Revenue",
             x = "Item MRP (Price)",
             y = "Item Outlet Sales (Revenue)"
         ) +
       theme_minimal()

