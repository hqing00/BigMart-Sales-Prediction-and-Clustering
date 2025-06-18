# Load necessary libraries
install.packages("e1071")
install.packages("DescTools")
install.packages("mice")
install.packages("car")
install.packages("Hmisc")
install.packages("randomForest")

library(tidyverse)
library(skimr)
library(ggplot2)
library(e1071)
library(DescTools)
library(mice)
library(car)
library(Hmisc)
library(randomForest)

# Load the dataset
train = read_csv("train.csv")
test = read_csv("test.csv")

# Data Overview
## Structure and summary
summary(train)
summary(test)
glimpse(train)
glimpse(test)

## Checking for missing values
sum(is.na(train))
sum(is.na(test))

## Check for empty strings and NA counts in each column
train %>% 
    reframe(across(everything(), ~ sum(. == "" | is.na(.)))) %>% 
    pivot_longer(everything(), names_to = "column", values_to = "empty_values") %>% 
    arrange(desc(empty_values))

test %>% 
    reframe(across(everything(), ~ sum(. == "" | is.na(.)))) %>% 
    pivot_longer(everything(), names_to = "column", values_to = "empty_values") %>% 
    arrange(desc(empty_values))

# Data Cleaning
## Replace possible empty strings to NA values
train$Outlet_Size[train$Outlet_Size == ""] <- NA
test$Outlet_Size[test$Outlet_Size == ""] <- NA

train$Item_Weight[train$Item_Weight == ""] <- NA
test$Item_Weight[test$Item_Weight == ""] <- NA

### Check the data types and structure again to avoid deviation
str(train)
str(test)

skim(train)
skim(test)

# Data Conversion
## Schema validation (data types, ranges, inconsistencies （factors） etc.)

colnames(train)
column_to_change <- c("Item_Type","Outlet_Size","Outlet_Type","Outlet_Location_Type","Item_Fat_Content")

train[column_to_change] <- lapply(train[column_to_change], factor)
test[column_to_change] <- lapply(test[column_to_change], factor)

## Check for inconsistencies of factor levels
output <- data.frame()
for (i in column_to_change){
    temp <- data.frame(
        variable = i,
        levels_train = levels(train[[i]]),
        levels_test = levels(test[[i]])
        )
    output <- rbind(output,temp)
}
print(output)


## Standardize factor levels
### Inconsistencies in Item_Fat_Content spotted, proceed to recode the factors
### Recode High to Large for Outlet_size for better understanding
train$Item_Fat_Content[train$Item_Fat_Content %in% c("LF","low fat")] <- "Low Fat"
train$Item_Fat_Content[train$Item_Fat_Content %in% c("reg")] <- "Regular"
test$Item_Fat_Content[test$Item_Fat_Content %in% c("LF","low fat")] <- "Low Fat"
test$Item_Fat_Content[test$Item_Fat_Content %in% c("reg")] <- "Regular"

train$Item_Fat_Content <- factor(train$Item_Fat_Content)
test$Item_Fat_Content <- factor(test$Item_Fat_Content)

levels(train$Outlet_Size)[levels(train$Outlet_Size) == "High"] <- "Large"
levels(test$Outlet_Size)[levels(test$Outlet_Size) == "High"] <- "Large"

## Validate Item_Identifier and Outlet_Identidier
range(nchar(train$Item_Identifier))
range(nchar(train$Outlet_Identifier))

train %>%
    filter(!grepl("^[A-Z]{3}[0-9]{2}$", Item_Identifier)) %>%
    sum()
test %>%
    filter(!grepl("^[A-Z]{3}[0-9]{2}$", Item_Identifier))%>%
    sum()

train %>%
    filter(!grepl("^[A-Z]{3}[0-9]{3}$", Outlet_Identifier))%>%
    sum()
test %>%
    filter(!grepl("^[A-Z]{3}[0-9]{3}$", Outlet_Identifier))%>%
    sum()

## Validate other numerical variables
range(train$Outlet_Establishment_Year)
range(train$Item_Visibility)
range(train$Item_MRP)
range(train$Item_Outlet_Sales)
range(train$Item_Weight, na.rm = TRUE)

## Check for duplicates
sum(train[duplicated(train), ])
sum(test[duplicated(test), ])

# Imputation 
## Check distribution
### Item_Weight (numerical)
#### Boxplot
train %>%
    ggplot(aes(x = Item_Weight), na.rm = TRUE) +
    geom_boxplot(fill = "skyblue") +
    labs(title = "Boxplot of Item Weight", x = "Item Weight")
test %>%
    ggplot(aes(x = Item_Weight), na.rm = TRUE) +
    geom_boxplot(fill = "skyblue") +
    labs(title = "Boxplot of Item Weight", x = "Item Weight")

#### Histogram
train %>%
    ggplot(aes(x = Item_Weight), na.rm = TRUE) +
    geom_histogram(fill = "skyblue", color = "black") +
    labs(title = "Distribution of Item Weight", x = "Item Weight", y = "Frequency")
test %>%
    ggplot(aes(x = Item_Weight), na.rm = TRUE) +
    geom_histogram(fill = "skyblue", color = "black") +
    labs(title = "Distribution of Item Weight", x = "Item Weight", y = "Frequency")


skewness(train$Item_Weight, na.rm = TRUE)
kurtosis(train$Item_Weight, na.rm = TRUE)

skewness(test$Item_Weight, na.rm = TRUE)
kurtosis(test$Item_Weight, na.rm = TRUE)

### Outlet_Size (categorical)
as.data.frame(summary(train$Outlet_Size))
train %>%
    drop_na(Outlet_Size) %>%
    group_by(Outlet_Type, Outlet_Size) %>%
    summarise(count = n(), .groups = "drop") %>%
    ggplot(aes(x = Outlet_Type, y = count, fill = Outlet_Size, na.rm =TRUE)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Outlet Size Counts by Outlet Type",
       x = "Outlet Type",
       y = "Count")
train %>%
    drop_na(Outlet_Size) %>%
    group_by(Outlet_Type, Outlet_Size) %>%
    summarise(count = n(), .groups = "drop") 


test %>%
    drop_na(Outlet_Size) %>%
    group_by(Outlet_Type, Outlet_Size) %>%
    summarise(count = n(), .groups = "drop") %>%
    ggplot(aes(x = Outlet_Type, y = count, fill = Outlet_Size, na.rm =TRUE)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Outlet Size Counts by Outlet Type",
       x = "Outlet Type",
       y = "Count")
test %>%
    drop_na(Outlet_Size) %>%
    group_by(Outlet_Type, Outlet_Size) %>%
    summarise(count = n(), .groups = "drop") 

# ---------------------------Checking Statistical Significance and assumptions (For Outlet_Size)-------------------------
## 1. MAR inspection (Whether the missingness of outlet size is associated with other variable)

train$Outlet_Size_missing <- is.na(train$Outlet_Size)

#### 1.1 Chisq test (categorical)
numerical_var <- c("Item_Weight","Item_Visibility","Item_MRP","Outlet_Establishment_Year")
categorical_var <- c("Outlet_Size","Outlet_Type","Item_Fat_Content","Outlet_Location_Type")

chisq_result <- data.frame()
for (i in categorical_var){
    if (i != "Outlet_Size") {
        freq_table <- table(train$Outlet_Size_missing,train[[i]])
        result<- chisq.test(freq_table)
        chisq_result <- rbind(chisq_result, data.frame(variables = i, p_value = result$p.value))
    }
}

names(result)
print(chisq_result)
print(chisq_result[chisq_result$p_value <=0.05,])

#### 1.2 T-test (numerical)
#### T-test
generalized_t_func <- function(variable_name){
                        formula <- as.formula(paste(variable_name,"~ Outlet_Size_missing"))
                        result <- t.test(formula, data = train)
                        p_val <- result$p.value
                        return(data.frame(variable = variable_name, p_value = p_val))
                        }

t_result <- map_dfr(numerical_var,generalized_t_func)
print(t_result)
print(subset(t_result,t_result$p_value <= 0.05))

#### Conclude MAR for Outlet_Size

# ------------------------------------------------------------------------------------------




# ---------------------------Checking Statistical Significance and assumptions (For Item_Weight)-------------------------
## 1. MAR inspection 
train$Item_Weight_missing <- is.na(train$Item_Weight)

## 1.1 Chisq test

chisq_result <- data.frame()
for (i in categorical_var){
    freq_table <- table(train$Item_Weight_missing,train[[i]])
    result<- chisq.test(freq_table)
    chisq_result <- rbind(chisq_result, data.frame(variables = i, p_value = result$p.value))

}

names(result)
print(chisq_result)
print(chisq_result[chisq_result$p_value <=0.05,])

#### 1.2 T-test (numerical)
#### T-test
generalized_t_func <- function(variable_name){
                        formula <- as.formula(paste(variable_name,"~ Item_Weight_missing"))
                        result <- t.test(formula, data = train)
                        p_val <- result$p.value
                        return(data.frame(variable = variable_name, p_value = p_val))
                        }

t_result <- map_dfr(numerical_var,generalized_t_func)
print(t_result)
print(subset(t_result,t_result$p_value <= 0.05))

#### Conclude MAR for Item_Weight


## 2. Check Multicollinearity (for numerical)
numeric <- train[,numerical_var]
correlation_mat <- rcorr(as.matrix(numeric, use = "pairwise.complete.obs"))
names(correlation_mat)
cor <- as.data.frame(correlation_mat$P)
print(cor["Item_Weight",])


model <- lm(Item_Weight ~ Item_Visibility + Item_MRP + Outlet_Establishment_Year, data = train)
vif(model)
### VIF score <5 indicates low multicollinearity


## 3. Check Linearity
plot(train$Item_Weight,train$Item_Visibility)
plot(train$Item_Weight,train$Item_MRP)
plot(train$Item_Weight,train$Outlet_Establishment_Year)

## Linearity Failed, might not proceed with pmm imputation

# ----------------------------------End Checking----------------------------------------------------------

# Imputation
#### MICE

all_features <- c("Item_Weight","Item_Visibility","Item_MRP","Outlet_Establishment_Year","Outlet_Type","Item_Fat_Content", "Outlet_Location_Type")

# Initialize mice
init <- mice(train,maxit=0)

# Inspect default imputation method by mice
# Default for Item_Weight is pnm, Outlet_Size is polyreg
meth <- init$method

#### Try to impute using random forest
meth["Item_Weight"] <- "rf"

# Exclude identifiers
pred <- init$predictorMatrix
pred["Item_Identifier","Outlet_Identifier"] <-0
pred
impute <- mice(train, method = meth, predictorMatrix = pred, m=5, seed = 123)

imputed_train <- complete(impute,1)
imputed_train
imputed_train_necessary <- imputed_train %>% select(-c("Outlet_Size_missing","Item_Weight_missing"))
colnames(imputed_train_necessary)

write_csv(imputed_train_necessary,"Imputed_train.csv")

#---------------------------------------------------------------------------

##### Samething for test
init1 <- mice(test,maxit = 0)
meth <- init1$method
meth["Item_Weight"] <- "rf"
pred <- init1$predictorMatrix
pred["Item_Identifier","Outlet_Identifier"] <-0
pred

impute1 <- mice(test, method = meth, predictorMatrix = pred, m = 5, seed = 124)
imputed1_test <- complete(impute1,1)

write_csv(imputed1_test,"Imputed_test.csv")

# -------------------------------------------------------------------------
## Check distribution of imputed value

# imputed <- mice(data, m = 5, method = "pmm", seed = 123)

# # Check density plots
densityplot(impute, ~Item_Weight)

# # Or stripplot (dot plot)
stripplot(impute, pch = 20, cex = 1.2)


























#### Check normality 

generalized_qqplot_func <- function(variable_name){
                            result <- ggplot(train, aes(sample = .data[[variable_name]])) + 
                                        stat_qq(na.rm = TRUE) + 
                                        stat_qq_line(color = "blue", na.rm = TRUE)+
                                        labs(title = paste("Q-Q Plot of",variable_name))
                                        }

plots <- map(numerical_var, generalized_qqplot_func)
length(plots)
plots[1]
plots[2]
plots[3]
plots[4]

# Out of the 4 variables, Outlet_Establishment_Year and Item_Visibility are not considered normally distributed
## Item Visibility is right-skewed and deviates significantly from normality
## Outlet Establishment Year is discrete

#### ANOVA-test --> Change to t-test
generalized_aov_func <- function(variable_name){
                        formula <- as.formula(paste(variable_name,"~ Outlet_Size"))
                        result <- aov(formula, data = train)
                        p_val <- summary(result)[[1]][["Pr(>F)"]][[1]]
                        return(data.frame(variable = variable_name, p_value = p_val))
                        }

aov_result <- map_dfr(numerical_var,generalized_aov_func)
print(aov_result)
print(subset(aov_result,aov_result$p_value <= 0.05))