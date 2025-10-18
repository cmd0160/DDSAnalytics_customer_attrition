# Frito Lay Employee Attrition Analysis  

**Author:** Cory Davis  
**Tools:** R | tidyverse | caret | ggplot2 | ggthemes | e1071 | knitr  

---

## 📘 Project Overview  

DDS Analytics was contracted by **Frito Lay** to investigate patterns of **employee attrition** and identify actionable drivers of turnover.  
This project uses statistical and machine-learning methods in R to explore the data, visualize key trends, and develop predictive insights that can help Frito Lay reduce voluntary turnover and associated replacement costs.

The accompanying notebook walks through the **full exploratory-to-modeling pipeline**, including:
- Exploratory Data Analysis (EDA)
- Feature engineering
- Model training and evaluation
- Business cost-savings estimation

You can **view the full analysis notebook directly on GitHub** here:  
👉 [Open `customer_attrition.md`](./notebooks/customer_attrition.md)

---

## 🎯 Business Context  

Replacing an employee costs between **50 % – 400 % of their annual salary**g on role and seniority.  
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
| **Columns** | 36 features (demographic, compensation, engagement, and performance metrics) |
| **Target Variable** | `Attrition` (Yes / No) |

---

## 📊 Exploratory Insights  

Key relationships discovered in the analysis:

| Factor | Observation | Insight |
|--------|--------------|----------|
| **Oty of leavers < 35 years old | Younger workers may seek faster career progression |
| **MonthlyIncome** | Lower income correlates with higher attrition | Compensation adjustments could improve retention |
| **Tenure (YearsAtCompany)** | Attrition declines sharply after 5 years | Investing in early-tenure engagement pays off |

Visual summaries and distributions are available in the notebook.

---

## ⚙️ Modeling Approach  

- **Models Evaluated:** Logistic Regression | Naive Bayes | K-Nearest Neighbors  
- **Validation:** 70 / 30 train-test split × 100 random seeds  
- **Metrics:** Accuracy, Sensitivity, Specificity, Precision, F1  
- **Tools:** `caret` for model training & tuning, `ggplot2` for visualization  

> The Logistic Regression model achieved the best overall balance between interpretability and predictive performance.

---

## 💰 Business Impact  

Using model probabilities and incentive cost assumptions:
- Baseline attrition ≈ 16 %  
- Targeting top 10 % risk employees could **save \$35 000 annually** in avoided replacement costs.  

---

## 📂 Repository Structure  


