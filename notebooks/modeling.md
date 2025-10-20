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
