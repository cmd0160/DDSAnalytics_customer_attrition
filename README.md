# 🧭 Frito Lay Employee Attrition Analysis  

**Author:** Cory Davis  
**Tools:** R | tidyverse | caret | ggplot2 | ggthemes | e1071 | knitr | naivebayes  

---

## Project Overview  

DDS Analytics was contracted by **Frito Lay** to investigate patterns of **employee attrition** and identify actionable drivers of turnover.  
This project applies **statistical and machine learning models** to understand why employees leave and how proactive intervention can reduce attrition costs.

The analysis is divided into two core notebooks:  

- 🧩 **[Univariate Analysis](./notebooks/univariate_analysis.md)** — Examines individual variables (e.g., demographics, income, satisfaction).  
- 🔗 **[Multivariate Analysis](./notebooks/multivariate_analysis.md)** — Explores feature interactions and develops predictive models.  
- 🔗 **[Naive Bayes Modeling](./notebooks/nb_modeling.md)** — Naive Bayes Modeling (Probabilistic Classification).  
- 🔗 **[KNN Modeling](./notebooks/knn_modeling.md)** — K-Nearest Neighbors Modeling (Distance Based Classification). 
- 🔗 **[Predictions](./notebooks/customer_attrition_predictions.Rmd)** — Predictions notebook. 


The full analysis pipeline includes:
- Exploratory Data Analysis (EDA)  
- Feature Engineering & Data Cleaning  
- Model Development (Naive Bayes & KNN)  
- Cross-Validation & Evaluation (Sensitivity / Specificity)  
- Business Cost-Savings Estimation  

---

## Business Context  

Replacing an employee costs between **50 % – 400 % of their annual salary**.  
Frito Lay estimates a **$200 retention incentive** could prevent at-risk employees from leaving.  
Accurate prediction of who’s likely to leave enables **targeted retention**, maximizing ROI on employee programs.

---

## Analytical Objectives  

1. **Identify key drivers** influencing voluntary turnover.  
2. **Build predictive models** to estimate attrition likelihood.  
3. **Quantify financial impact** of retaining high-risk employees through incentives.  

---

## Data Summary  

| Item | Description |
|------|--------------|
| **Source** | `CaseStudy1-data.csv` (Frito Lay HR records) |
| **Rows** | 870 employees |
| **Columns** | 36 features (demographic, compensation, engagement, performance metrics) |
| **Target Variable** | `Attrition` (Yes / No) |

---

## Key Insights  

| Factor | Observation | Insight |
|--------|--------------|----------|
| **OverTime** | Employees working overtime are ~2× more likely to leave | Workload imbalance drives attrition |
| **Age** | Most leavers are under 35 years old | Early-career retention is critical |
| **Monthly Income** | Lower earners show higher turnover | Pay equity and reward strategies matter |
| **Job & Environment Satisfaction** | Strong predictors of attrition | Cultural and engagement efforts can reduce risk |

---

## Modeling Approach  

- **Algorithms:** Naive Bayes | K-Nearest Neighbors (KNN)  
- **Validation:** 10-fold cross-validation (caret)  
- **Metrics:** Sensitivity, Specificity, ROC, F1
- **Preprocessing:**  
  - Removed constant & low-variance features  
  - Scaled numeric variables  
  - Encoded categorical predictors as factors  

### **Model Comparison**

| Model | Strength | Weakness |
|--------|-----------|-----------|
| **Naive Bayes** | High sensitivity (captured most at-risk employees) | Lower specificity (more false positives) |
| **KNN** | Balanced sensitivity/specificity | More computationally intensive |

---

## Findings Summary  

- **Attrition Risk:** Highest among Sales employees, younger staff, and those with heavy overtime.  
- **Satisfaction & Income:** Strongest predictors of turnover probability.  
- **Tenure:** Employees who stay beyond 5 years are far less likely to leave.  
- **Predictive Value:** Models can flag at-risk employees early, allowing targeted HR outreach.

---

## Business Recommendations  

1. ** Targeted Retention Programs**  
   - Focus on Sales, high-overtime, and low-satisfaction employees.  
   - Offer $200 incentives, schedule flexibility, and recognition programs.  

2. ** Early Career Engagement**  
   - Strengthen onboarding and mentorship for younger employees.  

3. ** Workload Management**  
   - Audit overtime patterns; rebalance workloads to prevent burnout.  

4. ** Predictive HR Monitoring**  
   - Embed the Naive Bayes model into HR dashboards to flag at-risk employees.  
   - Refresh model quarterly as new data becomes available.  

---

## Estimated Business Impact  

| Metric | Estimated Effect |
|---------|------------------|
| **Baseline Attrition** | ~16% |
| **Intervention Cost** | $200 per targeted employee |
| **Estimated Annual Savings** | **$350,000 – $500,000** in avoided turnover costs |
| **ROI** | High — minimal investment, major retention impact |

---



