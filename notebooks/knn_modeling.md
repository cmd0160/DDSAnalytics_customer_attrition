Frito Lay: Customer Attrition - Multivariate Analysis
================

``` r
library(tidyverse)
library(tidymodels)  
library(themis)
library(kknn)
library(naivebayes) 
library(caret)
set.seed(123)
```

------------------------------------------------------------------------

### ———— Data Loading ————-

``` r
data <- read.csv("../data/CaseStudy1-data.csv")
```

------------------------------------------------------------------------

### ———— KNN Modeling ————-

``` r
set.seed(123)

data$Attrition <- relevel(factor(data$Attrition), ref = "Yes")

idx   <- createDataPartition(data$Attrition, p = 0.7, list = FALSE)
train <- data[idx, ]
test  <- data[-idx, ]

attrition_rec <- recipe(Attrition ~ ., data = train) %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_nzv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  # step_corr(all_numeric_predictors(), threshold = 0.9) %>%
  # step_normalize(all_numeric_predictors(), -all_outcomes()) %>%
  step_range(all_numeric_predictors(), -all_outcomes(), min = 0, max = 1) %>% 
  step_tomek(Attrition) %>%
  step_upsample(Attrition, over_ratio = 1)

f1_summary <- function(data, lev = NULL, model = NULL) {
  precision <- posPredValue(data$pred, data$obs, positive = lev[1])
  recall    <- sensitivity(data$pred, data$obs, positive = lev[1])
  f1        <- (2 * precision * recall) / (precision + recall)

  c(F1 = f1)
}

ctrl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = f1_summary,
  classProbs = FALSE,
  savePredictions = "final"
)

grid <- expand.grid(k = seq(3, 100, by = 3))

set.seed(42)

knn_model <- train(
  attrition_rec,
  data = train,
  method = "knn",
  trControl = ctrl,
  tuneGrid = grid,
  metric = "F1"
)

pred_test <- predict(knn_model, newdata = test)
confusionMatrix(pred_test, test$Attrition, positive = "Yes")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction Yes  No
    ##        Yes  27  66
    ##        No   15 153
    ##                                           
    ##                Accuracy : 0.6897          
    ##                  95% CI : (0.6297, 0.7453)
    ##     No Information Rate : 0.8391          
    ##     P-Value [Acc > NIR] : 1               
    ##                                           
    ##                   Kappa : 0.2291          
    ##                                           
    ##  Mcnemar's Test P-Value : 2.767e-08       
    ##                                           
    ##             Sensitivity : 0.6429          
    ##             Specificity : 0.6986          
    ##          Pos Pred Value : 0.2903          
    ##          Neg Pred Value : 0.9107          
    ##              Prevalence : 0.1609          
    ##          Detection Rate : 0.1034          
    ##    Detection Prevalence : 0.3563          
    ##       Balanced Accuracy : 0.6707          
    ##                                           
    ##        'Positive' Class : Yes             
    ## 

------------------------------------------------------------------------

### ———— KNN Feature Importance ————-

``` r
importance_nb <- varImp(knn_model)
plot(importance_nb, top = 10, main = "Top 10 Feature Importances - K-Nearest Neighbors")
```

![](knn_modeling_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

------------------------------------------------------------------------
