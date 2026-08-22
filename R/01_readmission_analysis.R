# ============================================================
# MedResearch-01
# 01_readmission_analysis.R
#
# Research topic:
# Predicting 30-Day Hospital Readmission
#
# Dataset:
# Synthetic hospital patient data
#
# Purpose:
# Reproduce the complete statistical analysis
# ============================================================


# ------------------------------------------------------------
# 1. Set project working directory
# ------------------------------------------------------------

setwd("D:/MedResearch/MedResearch-01")


# ------------------------------------------------------------
# 2. Create synthetic research data
# ------------------------------------------------------------

set.seed(2026)

n <- 500

patients <- data.frame(
  patient_id = 1:n,
  age = sample(18:90, n, replace = TRUE),
  sex = sample(c("Male", "Female"), n, replace = TRUE),
  diabetes = sample(
    c("Yes", "No"),
    n,
    replace = TRUE,
    prob = c(0.25, 0.75)
  ),
  hypertension = sample(
    c("Yes", "No"),
    n,
    replace = TRUE,
    prob = c(0.35, 0.65)
  ),
  previous_admissions = sample(
    0:5,
    n,
    replace = TRUE
  ),
  length_of_stay = sample(
    1:20,
    n,
    replace = TRUE
  ),
  emergency_admission = sample(
    c("Yes", "No"),
    n,
    replace = TRUE,
    prob = c(0.60, 0.40)
  ),
  discharge_destination = sample(
    c("Home", "Rehabilitation", "Nursing Facility"),
    n,
    replace = TRUE,
    prob = c(0.75, 0.15, 0.10)
  )
)


# ------------------------------------------------------------
# 3. Create synthetic readmission outcome
# ------------------------------------------------------------

risk_score <-
  -3 +
  0.03 * patients$age +
  0.35 * (patients$diabetes == "Yes") +
  0.30 * (patients$hypertension == "Yes") +
  0.35 * patients$previous_admissions +
  0.08 * patients$length_of_stay +
  0.40 * (patients$emergency_admission == "Yes")

probability <- 1 / (1 + exp(-risk_score))

patients$readmitted_30d <- ifelse(
  runif(n) < probability,
  "Yes",
  "No"
)


# ------------------------------------------------------------
# 4. Create binary outcome for logistic regression
# ------------------------------------------------------------

patients$readmitted_binary <- ifelse(
  patients$readmitted_30d == "Yes",
  1,
  0
)


# ------------------------------------------------------------
# 5. Logistic regression: Length of stay only
# ------------------------------------------------------------

model_los <- glm(
  readmitted_binary ~ length_of_stay,
  data = patients,
  family = binomial
)

summary(model_los)

exp(coef(model_los))

exp(confint(model_los))


# ------------------------------------------------------------
# 6. Multivariable logistic regression
# ------------------------------------------------------------

model_full <- glm(
  readmitted_binary ~ age +
    diabetes +
    hypertension +
    previous_admissions +
    length_of_stay +
    emergency_admission,
  data = patients,
  family = binomial
)

summary(model_full)


# ------------------------------------------------------------
# 7. Adjusted Odds Ratios
# ------------------------------------------------------------

exp(coef(model_full))

exp(confint(model_full))


# ------------------------------------------------------------
# 8. Create results table
# ------------------------------------------------------------

results_table <- data.frame(
  Predictor = c(
    "Age",
    "Diabetes",
    "Hypertension",
    "Previous admissions",
    "Length of stay",
    "Emergency admission"
  ),
  Adjusted_OR = exp(coef(model_full))[-1],
  CI_lower = exp(confint(model_full))[-1, 1],
  CI_upper = exp(confint(model_full))[-1, 2],
  P_value = summary(model_full)$coefficients[-1, 4]
)


# Display results
print(results_table)


# ------------------------------------------------------------
# 9. Save results table
# ------------------------------------------------------------

write.csv(
  results_table,
  "reports/multivariable_logistic_regression_results.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 10. Create forest plot
# ------------------------------------------------------------

or_values <- exp(coef(model_full))[-1]

ci_values <- exp(confint(model_full))[-1, ]

predictor_names <- c(
  "Age",
  "Diabetes",
  "Hypertension",
  "Previous admissions",
  "Length of stay",
  "Emergency admission"
)

png(
  "figures/adjusted_odds_ratio_forest_plot.png",
  width = 1000,
  height = 700,
  res = 120
)

plot(
  or_values,
  seq_along(or_values),
  xlim = c(0.5, 3.8),
  pch = 19,
  yaxt = "n",
  xlab = "Adjusted Odds Ratio (95% CI)",
  ylab = "",
  main = "Factors Associated with 30-Day Hospital Readmission"
)

segments(
  ci_values[, 1],
  seq_along(or_values),
  ci_values[, 2],
  seq_along(or_values)
)

abline(
  v = 1,
  lty = 2
)

axis(
  2,
  at = seq_along(or_values),
  labels = predictor_names,
  las = 1
)

dev.off()


# ------------------------------------------------------------
# 11. Save synthetic dataset
# ------------------------------------------------------------

saveRDS(
  patients,
  "data/patients_synthetic.rds"
)
# ------------------------------------------------------------
# 12. Categorical variable analysis
# ------------------------------------------------------------

# Sex and 30-day readmission
sex_table <- table(
  patients$sex,
  patients$readmitted_30d
)

print(sex_table)

sex_percentages <- prop.table(
  sex_table,
  margin = 1
) * 100

print(sex_percentages)

sex_chisq <- chisq.test(sex_table)

print(sex_chisq)


# Discharge destination and 30-day readmission
discharge_table <- table(
  patients$discharge_destination,
  patients$readmitted_30d
)

print(discharge_table)

discharge_percentages <- prop.table(
  discharge_table,
  margin = 1
) * 100

print(discharge_percentages)

discharge_chisq <- chisq.test(discharge_table)

print(discharge_chisq)


# Diabetes and 30-day readmission
diabetes_table <- table(
  patients$diabetes,
  patients$readmitted_30d
)

print(diabetes_table)

diabetes_percentages <- prop.table(
  diabetes_table,
  margin = 1
) * 100

print(diabetes_percentages)

diabetes_chisq <- chisq.test(diabetes_table)

print(diabetes_chisq)


# Previous admissions and 30-day readmission
previous_admission_table <- table(
  patients$previous_admissions,
  patients$readmitted_30d
)

print(previous_admission_table)

previous_admission_percentages <- prop.table(
  previous_admission_table,
  margin = 1
) * 100

print(previous_admission_percentages)

previous_admission_chisq <- chisq.test(
  previous_admission_table
)

print(previous_admission_chisq)


# Emergency admission and 30-day readmission
emergency_table <- table(
  patients$emergency_admission,
  patients$readmitted_30d
)

print(emergency_table)

emergency_percentages <- prop.table(
  emergency_table,
  margin = 1
) * 100

print(emergency_percentages)

emergency_chisq <- chisq.test(emergency_table)

print(emergency_chisq)


# ------------------------------------------------------------
# 13. Length of stay comparison
# ------------------------------------------------------------

los_by_readmission <- tapply(
  patients$length_of_stay,
  patients$readmitted_30d,
  summary
)

print(los_by_readmission)

los_ttest <- t.test(
  length_of_stay ~ readmitted_30d,
  data = patients
)

print(los_ttest)


# ------------------------------------------------------------
# 14. Publication-friendly results table
# ------------------------------------------------------------

results_table_formatted <- data.frame(
  Predictor = results_table$Predictor,
  Adjusted_OR = sprintf(
    "%.2f",
    results_table$Adjusted_OR
  ),
  CI_95 = paste0(
    sprintf("%.2f", results_table$CI_lower),
    "–",
    sprintf("%.2f", results_table$CI_upper)
  ),
  P_value = ifelse(
    results_table$P_value < 0.001,
    "<0.001",
    sprintf("%.3f", results_table$P_value)
  )
)

print(results_table_formatted)

write.csv(
  results_table_formatted,
  "reports/multivariable_logistic_regression_results_formatted.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 15. ROC curve and AUC
# ------------------------------------------------------------

if (!requireNamespace("pROC", quietly = TRUE)) {
  stop(
    "Package 'pROC' is required for ROC analysis. ",
    "Install it with install.packages('pROC')."
  )
}

library(pROC)

roc_model <- roc(
  patients$readmitted_binary,
  fitted(model_full)
)

model_auc <- auc(roc_model)

print(model_auc)


# Save ROC curve
png(
  "figures/roc_curve_30day_readmission.png",
  width = 1000,
  height = 700,
  res = 120
)

plot(
  roc_model,
  main = "ROC Curve for 30-Day Hospital Readmission",
  xlab = "1 - Specificity",
  ylab = "Sensitivity",
  lwd = 2
)

abline(
  a = 0,
  b = 1,
  lty = 2
)

legend(
  "bottomright",
  legend = paste0(
    "AUC = ",
    round(as.numeric(model_auc), 3)
  ),
  bty = "n"
)

dev.off()


# ------------------------------------------------------------
# 16. Predicted risk and calibration summary
# ------------------------------------------------------------

patients$predicted_risk <- predict(
  model_full,
  type = "response"
)

print(summary(patients$predicted_risk))


# Calibration by predicted-risk deciles
patients$prediction_decile <- cut(
  patients$predicted_risk,
  breaks = quantile(
    patients$predicted_risk,
    probs = seq(0, 1, 0.1),
    na.rm = TRUE
  ),
  include.lowest = TRUE,
  duplicates = "drop"
)

calibration_table <- aggregate(
  cbind(
    observed = patients$readmitted_binary,
    predicted = patients$predicted_risk
  ),
  by = list(
    decile = patients$prediction_decile
  ),
  FUN = mean
)

print(calibration_table)


# ------------------------------------------------------------
# 17. Final reproducibility checks
# ------------------------------------------------------------

cat(
  "\nNumber of patients:",
  nrow(patients),
  "\n"
)

cat(
  "AUC:",
  round(as.numeric(model_auc), 4),
  "\n"
)

cat(
  "Analysis completed successfully.\n"
)

# ============================================================
# End of 01_readmission_analysis.R
# ============================================================