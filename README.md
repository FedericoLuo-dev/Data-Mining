# Linear and Logistic Regression Analysis on Loan Default Dataset

## Project Description
This project focuses on modeling and analyzing a loan default dataset using various regression techniques. The primary objectives are:
1.  **Linear Regression:** To model and understand the factors influencing the actual funded amount (`funded_amnt`).
2.  **Logistic Regression:** To model the probability of a borrower failing to repay their loan (`repay_fail`).
The workflow incorporates data imputation, multicollinearity checks, optimal grouping of categorical variables, handling of heteroscedasticity and influential observations, and robust inference methods.

## Dataset
The dataset `Anonymize_Loan_Default_data.csv` is used, containing information about loan applicants.
*   **Key Features Used:** `int_rate`, `term`, `emp_length`, `home_ownership`, `annual_inc`, `verification_status`, `dti`, `purpose`, `total_acc`, `revol_bal`, `installment`, `loan_amnt`.
*   **Target Variables:** 
    *   Continuous: `funded_amnt` (for Linear Regression)
    *   Binary: `repay_fail` (for Logistic Regression)

---

## Methodology and Project Phases

### 1. Initial Exploration and Missing Value Handling
*   Analyzed the dataset for missing values using the `VIM` package (`aggr` plot).
*   Removed a highly problematic row (row 55) containing numerous NAs.
*   **Imputation:** Utilized the `mice` package with Predictive Mean Matching (PMM) to impute missing data specifically for the covariates involved in the starting models.

### 2. Linear Regression for `funded_amnt`
*   **Starting Model:** Built an initial Ordinary Least Squares (OLS) model and evaluated it using `summary()`, `forest_model()`, and diagnostic plots.
*   **Multicollinearity:** Calculated the Variance Inflation Factor (VIF) and partial correlations (`parcor`). Dropped `installment` and identified `loan_amnt` as nearly identical to the dependent variable, refining the model features.
*   **Optimal Grouping:** Used the `factorMerger` package to optimally group categorical variables (`purpose` and `emp_length`), creating new merged factors (`purposeC`, `emp_lengthCC`) to improve model performance and stability.
*   **Box-Cox Transformation:** Applied the Box-Cox transformation to the dependent variable (resulting in a square root transformation `funded_amnt^0.5`) to address non-normality and stabilize variance.
*   **GAM (Generalized Additive Model):** Explored non-linear relationships using `gam()` to identify the need for logarithmic transformations on `total_acc` and `revol_bal`.
*   **Model Selection:** Performed stepwise selection (`stepAIC`) and evaluated the model specification using the RESET test (`resettest`).
*   **Diagnostics:** 
    *   Checked for heteroscedasticity using the Breusch-Pagan test (`bptest`).
    *   Identified and removed influential observations using Cook's Distance (`cooks.distance`) and DFFITS (`dffits`).
*   **Robust Inference:** 
    *   Calculated White's Heteroscedasticity-Consistent Standard Errors (`vcovHC`).
    *   Fitted a Robust Regression model using `lmrob()` to mitigate the impact of outliers.
    *   Performed Bootstrap inference (`Boot()` from the `car` package) to obtain robust confidence intervals for the model coefficients.

### 3. Logistic Regression for `repay_fail`
*   **Target Definition:** Modeled the binary outcome `repay_fail` (1 = default, 0 = repaid).
*   **Data Preparation:** Imputed missing values specific to the logistic regression covariates using `mice` (PMM).
*   **Optimal Grouping:** Applied `factorMerger` again to optimally bin the `purpose` variable (`purposeC`) specifically for the binary target.
*   **Model Fitting:** Trained a logistic regression model (`glm` with `family="binomial"`) using features like `funded_amnt`, `int_rate`, `term`, `home_ownership`, `purposeC`, `annual_inc`, and `dti`.
*   **Evaluation:** Evaluated the model using likelihood ratio tests (`drop1`) and computed predictions to generate a confusion matrix and calculate the overall accuracy. Interpreted the odds ratios (`exp(modlog$coefficients)`).
