Frito Lay: Customer Attrition - Multivariate Analysis
================

``` r
library(tidyverse)
library(caret)
library(e1071)
library(class)
library(naivebayes)
library(scales)
library(ggthemes)
theme_set(theme_economist())
title_format <- function(x) labs(title = x, x = NULL, y = NULL)
```

------------------------------------------------------------------------

## ———— Data Loading ————-

``` r
# getwd()
data <- read.csv("../data/CaseStudy1-data.csv")
```

## ———— Data Preparation ————-

------------------------------------------------------------------------

``` r
str(data)
```

    ## 'data.frame':    870 obs. of  36 variables:
    ##  $ ID                      : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Age                     : int  32 40 35 32 24 27 41 37 34 34 ...
    ##  $ Attrition               : chr  "No" "No" "No" "No" ...
    ##  $ BusinessTravel          : chr  "Travel_Rarely" "Travel_Rarely" "Travel_Frequently" "Travel_Rarely" ...
    ##  $ DailyRate               : int  117 1308 200 801 567 294 1283 309 1333 653 ...
    ##  $ Department              : chr  "Sales" "Research & Development" "Research & Development" "Sales" ...
    ##  $ DistanceFromHome        : int  13 14 18 1 2 10 5 10 10 10 ...
    ##  $ Education               : int  4 3 2 4 1 2 5 4 4 4 ...
    ##  $ EducationField          : chr  "Life Sciences" "Medical" "Life Sciences" "Marketing" ...
    ##  $ EmployeeCount           : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ EmployeeNumber          : int  859 1128 1412 2016 1646 733 1448 1105 1055 1597 ...
    ##  $ EnvironmentSatisfaction : int  2 3 3 3 1 4 2 4 3 4 ...
    ##  $ Gender                  : chr  "Male" "Male" "Male" "Female" ...
    ##  $ HourlyRate              : int  73 44 60 48 32 32 90 88 87 92 ...
    ##  $ JobInvolvement          : int  3 2 3 3 3 3 4 2 3 2 ...
    ##  $ JobLevel                : int  2 5 3 3 1 3 1 2 1 2 ...
    ##  $ JobRole                 : chr  "Sales Executive" "Research Director" "Manufacturing Director" "Sales Executive" ...
    ##  $ JobSatisfaction         : int  4 3 4 4 4 1 3 4 3 3 ...
    ##  $ MaritalStatus           : chr  "Divorced" "Single" "Single" "Married" ...
    ##  $ MonthlyIncome           : int  4403 19626 9362 10422 3760 8793 2127 6694 2220 5063 ...
    ##  $ MonthlyRate             : int  9250 17544 19944 24032 17218 4809 5561 24223 18410 15332 ...
    ##  $ NumCompaniesWorked      : int  2 1 2 1 1 1 2 2 1 1 ...
    ##  $ Over18                  : chr  "Y" "Y" "Y" "Y" ...
    ##  $ OverTime                : chr  "No" "No" "No" "No" ...
    ##  $ PercentSalaryHike       : int  11 14 11 19 13 21 12 14 19 14 ...
    ##  $ PerformanceRating       : int  3 3 3 3 3 4 3 3 3 3 ...
    ##  $ RelationshipSatisfaction: int  3 1 3 3 3 3 1 3 4 2 ...
    ##  $ StandardHours           : int  80 80 80 80 80 80 80 80 80 80 ...
    ##  $ StockOptionLevel        : int  1 0 0 2 0 2 0 3 1 1 ...
    ##  $ TotalWorkingYears       : int  8 21 10 14 6 9 7 8 1 8 ...
    ##  $ TrainingTimesLastYear   : int  3 2 2 3 2 4 5 5 2 3 ...
    ##  $ WorkLifeBalance         : int  2 4 3 3 3 2 2 3 3 2 ...
    ##  $ YearsAtCompany          : int  5 20 2 14 6 9 4 1 1 8 ...
    ##  $ YearsInCurrentRole      : int  2 7 2 10 3 7 2 0 1 2 ...
    ##  $ YearsSinceLastPromotion : int  0 4 2 5 1 1 0 0 0 7 ...
    ##  $ YearsWithCurrManager    : int  3 9 2 7 3 7 3 0 0 7 ...

------------------------------------------------------------------------

### ————- Remove Low Variance and Constant Features ————-

``` r
# Check if each column contains only unique values
unique_check <- sapply(data, function(x) length(unique(x)) == nrow(data))

# Check if each column contains only one unique value
constant_check <- sapply(data, function(x) length(unique(x)) == 1)

col_check <- list(
  constant = names(data)[constant_check],
  unique = names(data)[unique_check]
)

col_check
```

    ## $constant
    ## [1] "EmployeeCount" "Over18"        "StandardHours"
    ## 
    ## $unique
    ## [1] "ID"             "EmployeeNumber"

``` r
dim(data)
```

    ## [1] 870  36

``` r
data_v2 <- data %>%
  select(-one_of(col_check$constant)) %>%
  select(-one_of(col_check$unique))

dim(data_v2)
```

    ## [1] 870  31

------------------------------------------------------------------------

### ————- Convert Categorical Variables to Factors ————-

``` r
data_v3 <- data_v2 %>%
  mutate_if(is.character, as.factor)
str(data_v3)
```

    ## 'data.frame':    870 obs. of  31 variables:
    ##  $ Age                     : int  32 40 35 32 24 27 41 37 34 34 ...
    ##  $ Attrition               : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ BusinessTravel          : Factor w/ 3 levels "Non-Travel","Travel_Frequently",..: 3 3 2 3 2 2 3 3 3 2 ...
    ##  $ DailyRate               : int  117 1308 200 801 567 294 1283 309 1333 653 ...
    ##  $ Department              : Factor w/ 3 levels "Human Resources",..: 3 2 2 3 2 2 2 3 3 2 ...
    ##  $ DistanceFromHome        : int  13 14 18 1 2 10 5 10 10 10 ...
    ##  $ Education               : int  4 3 2 4 1 2 5 4 4 4 ...
    ##  $ EducationField          : Factor w/ 6 levels "Human Resources",..: 2 4 2 3 6 2 4 2 2 6 ...
    ##  $ EnvironmentSatisfaction : int  2 3 3 3 1 4 2 4 3 4 ...
    ##  $ Gender                  : Factor w/ 2 levels "Female","Male": 2 2 2 1 1 2 2 1 1 2 ...
    ##  $ HourlyRate              : int  73 44 60 48 32 32 90 88 87 92 ...
    ##  $ JobInvolvement          : int  3 2 3 3 3 3 4 2 3 2 ...
    ##  $ JobLevel                : int  2 5 3 3 1 3 1 2 1 2 ...
    ##  $ JobRole                 : Factor w/ 9 levels "Healthcare Representative",..: 8 6 5 8 7 5 7 8 9 1 ...
    ##  $ JobSatisfaction         : int  4 3 4 4 4 1 3 4 3 3 ...
    ##  $ MaritalStatus           : Factor w/ 3 levels "Divorced","Married",..: 1 3 3 2 3 1 2 1 2 2 ...
    ##  $ MonthlyIncome           : int  4403 19626 9362 10422 3760 8793 2127 6694 2220 5063 ...
    ##  $ MonthlyRate             : int  9250 17544 19944 24032 17218 4809 5561 24223 18410 15332 ...
    ##  $ NumCompaniesWorked      : int  2 1 2 1 1 1 2 2 1 1 ...
    ##  $ OverTime                : Factor w/ 2 levels "No","Yes": 1 1 1 1 2 1 2 2 2 1 ...
    ##  $ PercentSalaryHike       : int  11 14 11 19 13 21 12 14 19 14 ...
    ##  $ PerformanceRating       : int  3 3 3 3 3 4 3 3 3 3 ...
    ##  $ RelationshipSatisfaction: int  3 1 3 3 3 3 1 3 4 2 ...
    ##  $ StockOptionLevel        : int  1 0 0 2 0 2 0 3 1 1 ...
    ##  $ TotalWorkingYears       : int  8 21 10 14 6 9 7 8 1 8 ...
    ##  $ TrainingTimesLastYear   : int  3 2 2 3 2 4 5 5 2 3 ...
    ##  $ WorkLifeBalance         : int  2 4 3 3 3 2 2 3 3 2 ...
    ##  $ YearsAtCompany          : int  5 20 2 14 6 9 4 1 1 8 ...
    ##  $ YearsInCurrentRole      : int  2 7 2 10 3 7 2 0 1 2 ...
    ##  $ YearsSinceLastPromotion : int  0 4 2 5 1 1 0 0 0 7 ...
    ##  $ YearsWithCurrManager    : int  3 9 2 7 3 7 3 0 0 7 ...

------------------------------------------------------------------------

### ————- Modeling ————-

##### ————- Naive Bayes with 10-fold Cross-Validation ————-

``` r
set.seed(123)
ctrl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE
)

data_v3$Attrition <- relevel(data_v3$Attrition, ref = "Yes")

nb_cv <- train(
  Attrition ~ .,
  data = data_v3,
  method = "naive_bayes",
  trControl = ctrl,
  metric = "ROC",
  tuneGrid = data.frame(
    laplace   = 0,
    usekernel = FALSE,
    adjust    = 1
  )
)
nb_cv
```

    ## Naive Bayes 
    ## 
    ## 870 samples
    ##  30 predictor
    ##   2 classes: 'Yes', 'No' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 783, 783, 783, 783, 783, 783, ... 
    ## Resampling results:
    ## 
    ##   ROC        Sens       Spec     
    ##   0.7746575  0.7857143  0.6041096
    ## 
    ## Tuning parameter 'laplace' was held constant at a value of 0
    ## Tuning
    ##  parameter 'usekernel' was held constant at a value of FALSE
    ## Tuning
    ##  parameter 'adjust' was held constant at a value of 1

------------------------------------------------------------------------

##### Testing Prediction

``` r
set.seed(123)
idx <- createDataPartition(data_v3$Attrition, p = 0.7, list = FALSE)
train <- data_v3[idx, ]
test  <- data_v3[-idx, ]

pred_nb <- predict(nb_cv, newdata = test)

pred_prob <- predict(nb_cv, newdata = test, type = "prob")

cm_nb <- confusionMatrix(pred_nb, test$Attrition, positive = "Yes")
cm_nb
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction Yes  No
    ##        Yes  34  78
    ##        No    8 141
    ##                                           
    ##                Accuracy : 0.6705          
    ##                  95% CI : (0.6099, 0.7272)
    ##     No Information Rate : 0.8391          
    ##     P-Value [Acc > NIR] : 1               
    ##                                           
    ##                   Kappa : 0.2709          
    ##                                           
    ##  Mcnemar's Test P-Value : 1.003e-13       
    ##                                           
    ##             Sensitivity : 0.8095          
    ##             Specificity : 0.6438          
    ##          Pos Pred Value : 0.3036          
    ##          Neg Pred Value : 0.9463          
    ##              Prevalence : 0.1609          
    ##          Detection Rate : 0.1303          
    ##    Detection Prevalence : 0.4291          
    ##       Balanced Accuracy : 0.7267          
    ##                                           
    ##        'Positive' Class : Yes             
    ## 

------------------------------------------------------------------------

##### ————- KNN with 10-fold Cross-Validation ————-

``` r
set.seed(123)

data_v3$Attrition <- relevel(data_v3$Attrition, ref = "Yes")
ctrl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE
)

knn_cv <- train(
  Attrition ~ .,
  data = data_v3,
  method = "knn",
  trControl = ctrl,
  preProcess = c("center", "scale"),
  metric = "Sens",                  
  tuneGrid = data.frame(k = seq(3, 31, 2))
)

knn_cv
```

    ## k-Nearest Neighbors 
    ## 
    ## 870 samples
    ##  30 predictor
    ##   2 classes: 'Yes', 'No' 
    ## 
    ## Pre-processing: centered (44), scaled (44) 
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 783, 783, 783, 783, 783, 783, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   k   ROC        Sens        Spec     
    ##    3  0.6295499  0.16428571  0.9712329
    ##    5  0.6771037  0.16428571  0.9863014
    ##    7  0.7238748  0.16428571  0.9863014
    ##    9  0.7185910  0.14285714  0.9917808
    ##   11  0.7310665  0.12142857  0.9945205
    ##   13  0.7382094  0.10714286  0.9958904
    ##   15  0.7390900  0.10000000  0.9945205
    ##   17  0.7363503  0.10000000  0.9972603
    ##   19  0.7388943  0.08571429  0.9972603
    ##   21  0.7386008  0.07857143  0.9972603
    ##   23  0.7397750  0.07857143  0.9986301
    ##   25  0.7411448  0.07857143  0.9972603
    ##   27  0.7430039  0.04285714  0.9986301
    ##   29  0.7434932  0.03571429  0.9986301
    ##   31  0.7538650  0.03571429  1.0000000
    ## 
    ## Sens was used to select the optimal model using the largest value.
    ## The final value used for the model was k = 7.

------------------------------------------------------------------------

##### Testing Prediction

``` r
set.seed(6)

idx  <- createDataPartition(data_v3$Attrition, p = 0.7, list = FALSE)
train <- data_v3[idx, ]
test  <- data_v3[-idx, ]

pred_knn_cls  <- predict(knn_cv, newdata = test)              # class labels
pred_knn_prob <- predict(knn_cv, newdata = test, type = "prob")  # probs (optional)

confusionMatrix(pred_knn_cls, test$Attrition, positive = "Yes")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction Yes  No
    ##        Yes   7   0
    ##        No   35 219
    ##                                           
    ##                Accuracy : 0.8659          
    ##                  95% CI : (0.8185, 0.9048)
    ##     No Information Rate : 0.8391          
    ##     P-Value [Acc > NIR] : 0.1358          
    ##                                           
    ##                   Kappa : 0.2513          
    ##                                           
    ##  Mcnemar's Test P-Value : 9.081e-09       
    ##                                           
    ##             Sensitivity : 0.16667         
    ##             Specificity : 1.00000         
    ##          Pos Pred Value : 1.00000         
    ##          Neg Pred Value : 0.86220         
    ##              Prevalence : 0.16092         
    ##          Detection Rate : 0.02682         
    ##    Detection Prevalence : 0.02682         
    ##       Balanced Accuracy : 0.58333         
    ##                                           
    ##        'Positive' Class : Yes             
    ## 
