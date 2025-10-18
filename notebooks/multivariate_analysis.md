Frito Lay: Customer Attrition - Multivariate Analysis
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.2
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(caret)
```

    ## Loading required package: lattice
    ## 
    ## Attaching package: 'caret'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(e1071)
library(class)
library(scales)
```

    ## 
    ## Attaching package: 'scales'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
library(ggthemes)
theme_set(theme_economist())
title_format <- function(x) labs(title = x, x = NULL, y = NULL)
```

## ———— Data Loading ————-

``` r
getwd()
```

    ## [1] "/Users/cory/Projects/DDSAnalytics_customer_attrition/notebooks"

``` r
data <- read.csv("../data/CaseStudy1-data.csv")
```

## ———— Multivariate Analysis ————-

Multivariate analysis explores how multiple factors interact to
influence employee attrition. By examining combinations of demographic,
compensation, and work-related variables together, it reveals deeper
relationships—such as how overtime, tenure, and income jointly affect
turnover—providing a more complete view of the underlying drivers of
employee loss.

### Attrition by Department and OverTime

Question:

How does the Attrition rate vary across different Departments and
OverTime status? The following heatmap visualizes this relationship:

``` r
# Calculating attrition rate by Department and OverTime group
df1 <- data %>%
  group_by(Department, OverTime) %>%
  summarise(attr_rate = mean(Attrition == "Yes"), .groups = "drop")

# Creating a heatmap to visualize attrition patterns by Department and OverTime
ggplot(df1, aes(Department, OverTime, fill = attr_rate)) +
  geom_tile(color = "white") +  
  scale_fill_gradient(
    low = "#d8e6f3", 
    high = "#b2182b", 
    labels = percent  
  ) +
  labs(
    title = "Attrition Rate by Department and OverTime",
    fill = "Attrition Rate Percentage"
  ) +
  theme(
    # Center and bold the plot title with slight spacing below
    plot.title = element_text(
      hjust = 0.5,
      face = "bold", 
      margin = margin(b = 10)
    ),
    # Formating legend text and size
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.4, "cm"),
    # Adding padding around axis titles for readability
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12)),
  )
```

![](multivariate_analysis_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

This heatmap illustrates how attrition rates vary across departments and
overtime status. The darkest red shading represents the highest
attrition percentages. Employees in the Sales department who work
overtime experience the highest attrition rate, exceeding 40%. In
contrast, Research & Development employees with no overtime show the
lowest attrition, suggesting that work-life balance plays a key role in
retention. Across all departments, overtime consistently correlates with
higher attrition, but the impact is most pronounced in Sales—indicating
that targeted workload management and incentive strategies in Sales
could potentially yield meaningful reductions in turnover.
