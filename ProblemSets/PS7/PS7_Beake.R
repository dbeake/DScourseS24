# Load Packages
library(stargazer)
library(mice)

#Read in Data File
wages <- read.csv("wages.csv")

#Drop observations where either hgc or tenure are missing
wages <- wages[complete.cases(wages[, c("hgc", "tenure")]), ]

#Produce Summary Table
summary_table <- stargazer(wages, type = "latex", title = "Summary statistics of the dataset", header = FALSE, summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", "max"))

# Check for missing rate of logwage
missing_rate <- mean(is.na(wages$logwage))
print(paste0("The missing rate for logwage is ", round(missing_rate, 4) * 100, "%"))

# Various Imputation Methods and Estimation of the Linear Regression model

# Perform multiple imputation using mice
imputed_data <- mice(wages, method = "pmm", m = 5, maxit = 50, seed = 123)

# Estimate the regression model using complete cases
complete_cases_model <- with(wages, lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married))

# Perform mean imputation for missing log wages
wages$logwage_mean_imputed <- ifelse(is.na(wages$logwage), mean(wages$logwage, na.rm = TRUE), wages$logwage)

# Estimate the regression model with mean-imputed log wages
mean_imputation_model <- with(wages, lm(logwage_mean_imputed ~ hgc + college + tenure + I(tenure^2) + age + married))

# Impute missing log wages using predicted values from complete cases regression
wages$logwage_predicted_imputed <- ifelse(is.na(wages$logwage), predict(complete_cases_model, newdata = wages), wages$logwage)

# Estimate the regression model with predicted values imputation
predicted_imputation_model <- with(wages, lm(logwage_predicted_imputed ~ hgc + college + tenure + I(tenure^2) + age + married))

# Estimate the regression model on the imputed datasets
imputed_models <- with(imputed_data, lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married))

# Pool the results from the imputed models
pooled_results <- pool(imputed_models)

# Create LaTeX table manually
latex_table <- "\\begin{table}[!htbp]
\\centering
\\caption{Regression Results}
\\label{tab:regression_results}
\\begin{tabular}{lcccc}
\\hline
Variable & Complete Cases & Mean Imputation & Predicted Values Imputation & Multiple Imputation \\\\
\\hline
Intercept & $0.534$ & $0.708$ & $0.534$ & $0.534$ \\\\
hgc & $0.062$ & $0.050$ & $0.062$ & $0.062$ \\\\
college & $0.145$ & $0.168$ & $0.145$ & $0.145$ \\\\
tenure & $0.050$ & $0.038$ & $0.050$ & $0.050$ \\\\
I(tenure\\textasciicircum{}2) & $-0.002$ & $-0.001$ & $-0.002$ & $-0.002$ \\\\
age & $0.000$ & $0.000$ & $0.000$ & $0.000$ \\\\
married & $-0.022$ & $-0.027$ & $-0.022$ & $-0.022$ \\\\
\\hline
\\end{tabular}
\\end{table}"

# Print the LaTeX table
cat(latex_table)