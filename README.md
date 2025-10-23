# Frito Lay Employee Attrition Analysis  

**Author:** Cory Davis  
**Tools:** R | tidyverse | caret | ggplot2 | ggthemes | e1071 | knitr  

---

## 📘 Project Overview  

DDS Analytics was contracted by **Frito Lay** to investigate patterns of **employee attrition** and identify actionable drivers of turnover.  
This project uses statistical and machine-learning methods in R to explore the data, visualize key trends, and develop predictive insights that can help Frito Lay reduce voluntary turnover and associated replacement costs.

The analysis is divided into two complementary notebooks:  

- 🧩 **[Univariate Analysis](./notebooks/univariate_analysis.md)** — Exploratory analysis of individual features (demographics, compensation, engagement).  
- 🔗 **[Multivariate Analysis](./notebooks/multivariate_analysis.md)** — Relationships among multiple features and modeling of attrition risk.  

Together they form the full **exploratory-to-modeling pipeline**, including:
- Exploratory Data Analysis (EDA)
- Feature engineering
- Model training and evaluation
- Business cost-savings estimation

---

## 🎯 Business Context  

Replacing an employee costs between **50 % – 400 % of their annual salary**, depending on role and seniority.  
By predicting which employees are most likely to leave and intervening proactively, Frito Lay can allocate retention incentives (≈ $200 per employee) more effectively, achieving measurable financial savings.

---

## 🔍 Analytical Objectives  

1. **Understand attrition patterns** — visualize demographic, role-based, and compensation factors.  
2. **Identify top predictors** of voluntary turnover.  
3. **Quantify potential cost savings** if high-risk employees are retained through targeted incentives.  

---

## 🧠 Data Summary  

| Item | Description |
|------|--------------|
| **Source** | `CaseStudy1-data.csv` (Frito Lay HR records) |
| **Rows** | ≈ 1 000 employees |
| **Columns** | 36 features (demographic, compensation, engagement, performance metrics) |
| **Target Variable** | `Attrition` (Yes / No) |

---

## 📊 Key Insights  

| Factor | Observation | Insight |
|--------|--------------|----------|
| **OverTime** | Employees working overtime show roughly 2× higher attrition rates | Work-life imbalance is a strong turnover driver |
| **Age** | Majority of leavers < 35 years old | Younger workers may seek faster career progression |
| **Monthly Income** | Lower income correlates with higher attrition | Compensation adjustments could improve retention |
| **Tenure (YearsAtCompany)** | Attrition declines sharply after 5 years | Investing in early-tenure engagement pays off |

Visual summaries and distributions are available in both notebooks.

---

## ⚙️ Modeling Approach  

- **Models Evaluated:** Logistic Regression | Naive Bayes | K-Nearest Neighbors  
- **Validation:** 70 / 30 train-test split × 100 random seeds  
- **Metrics:** Accuracy, Sensitivity, Specificity, Precision, F1  
- **Tools:** `caret` for model training & tuning, `ggplot2` for visualization  

> The Logistic Regression model achieved the best balance between interpretability and predictive performance.

---

## 💰 Business Impact  

Using model probabilities and incentive cost assumptions:
- Baseline attrition ≈ 16 %  
- Targeting the top 10 % at-risk employees could **save $350 000 – $500 000 annually** in avoided replacement costs.  

---

## 📂 Repository Structure  

