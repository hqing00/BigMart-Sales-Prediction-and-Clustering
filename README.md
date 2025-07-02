# BigMart Sales Prediction and Clustering
This project was developed as part of a coursework assignment in Programming for Data Science (WQD7004)

## Description
Analyzed BigMart sales data to predict product-level sales and classify outlet performance. The project aimed to uncover sales patterns and build predictive models to support pricing and promotional strategy optimization.

## Problem
BigMart struggles with forecasting product sales and evaluating outlet performance across locations. This project focuses on building predictive models and uncovering sales trends to enhance pricing and promotion strategies through data-driven insights.

## Objectives  
- To identify key drivers of item sales in BigMart outlets and build a predictive model for future sales forecasting.
- To classify outlet performance levels and provide insights on which outlets are more likely to succeed.

## Data
The data is obtained from https://www.kaggle.com/datasets/brijbhushannanda1979/bigmart-sales-data/data

## Contents
**Modeling**
- Linear Regression
- Ridge Regression
- Lasso Regression
- Elastic-net regularized generalized linear model
- Decision Tree
- Random Forest
- GBM
- SVR
- XGBoost

**Clustering**  
- K-Means Clustering
- Hierarchical clustering

## Conclusion
This project identified key drivers of item sales in BigMart outlets through exploratory data analysis (EDA), imputation, modeling, and clustering. EDA revealed missing values in Item_Weight and Outlet_Size, with distribution checks, skewness, and others to validate imputation assumptions. Statistical tests confirmed that the missingness was not random, and MICE was applied to impute values.  

Predictive modeling showed that GBM and SVR achieved the best performance, while Ridge and ElasticNet provided consistent results. Polynomial regression has the poor performance among all models.  

K-means clustering divided the product into two groups. In addition, hierarchical clustering segmented outlets into three performance groups: top performers (e.g., OUT027), steady performers, and underperformers (e.g., OUT010, OUT019). These insights support strategic recommendations such as introduce volume discounts, boost visibility, scaling successful practices, optimizing mid-tier stores, and intervening in low-performing outlets to drive overall sales improvement.  

## Team Members
1. Chew Hong Ern
2. Yap Hui Qing
3. Yeoh Li Ting
4. Lau Shi Jie
5. Nirvar Roy Vaskar

## Published Link
https://rpubs.com/yhq22/wqd7004 
