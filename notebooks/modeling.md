Frito Lay: Customer Attrition - Multivariate Analysis
================

``` r
library(tidyverse)
library(caret)
library(e1071)
library(class)
library(scales)
library(ggthemes)
theme_set(theme_economist())
title_format <- function(x) labs(title = x, x = NULL, y = NULL)
```

## ———— Data Loading ————-

``` r
# getwd()
data <- read.csv("../data/CaseStudy1-data.csv")
```

## ———— Data Preparation ————-

``` r
# glimpse(data)
```

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
