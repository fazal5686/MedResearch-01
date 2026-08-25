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

# ------------------------------------------------------------
# 10. Multicollinearity diagnostic: Variance Inflation Factor
# ------------------------------------------------------------

if (!requireNamespace("car", quietly = TRUE)) {
  stop("Package 'car' is required for VIF analysis.")
}

vif_values <- car::vif(model_full)

vif_results <- data.frame(
  Predictor = names(vif_values),
  VIF = as.numeric(vif_values)
)

print(vif_results)

write.csv(
  vif_results,
  "reports/multicollinearity_vif_results.csv",
  row.names = FALSE
)
# 11. Create forest plot
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
# 12. Save synthetic dataset
# ------------------------------------------------------------

saveRDS(
  patients,
  "data/patients_synthetic.rds"
)
# ------------------------------------------------------------
# 13. Categorical variable analysis
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
# 14. Length of stay comparison
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
# 15. Publication-friendly results table
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
# 16. ROC curve and AUC
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
# 17. Predicted risk and calibration summary
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

# ------------------------------------------------------------
# 18. Overfitting assessment and bootstrap internal validation
# ------------------------------------------------------------

# Basic model complexity assessment
n_total <- nrow(model.frame(model_full))

n_events <- sum(
  model.response(model.frame(model_full)) == 1
)

n_nonevents <- sum(
  model.response(model.frame(model_full)) == 0
)

n_predictors <- length(coef(model_full)) - 1

epp <- n_events / n_predictors

model_full_aic <- AIC(model_full)
model_full_bic <- BIC(model_full)
model_full_deviance <- deviance(model_full)
model_full_null_deviance <- model_full$null.deviance

model_full_pseudo_r2 <- 1 -
  (model_full_deviance / model_full_null_deviance)

cat("Total observations:", n_total, "\n")
cat("Readmission events:", n_events, "\n")
cat("Non-readmission observations:", n_nonevents, "\n")
cat("Number of predictors:", n_predictors, "\n")
cat("Events per predictor:", round(epp, 2), "\n")
cat("AIC:", model_full_aic, "\n")
cat("BIC:", model_full_bic, "\n")
cat("Null deviance:", model_full_null_deviance, "\n")
cat("Residual deviance:", model_full_deviance, "\n")
cat("McFadden-style pseudo-R2:", model_full_pseudo_r2, "\n")

# Bootstrap internal validation
if (!requireNamespace("pROC", quietly = TRUE)) {
  stop("Package 'pROC' is required for bootstrap AUC validation.")
}

set.seed(12345)
B <- 200
optimism_auc <- numeric(B)
bootstrap_auc <- numeric(B)

for (b in seq_len(B)) {

  boot_index <- sample(
    seq_len(nrow(patients)),
    size = nrow(patients),
    replace = TRUE
  )

  boot_data <- patients[boot_index, ]

  boot_model <- glm(
    readmitted_binary ~
      age +
      diabetes +
      hypertension +
      previous_admissions +
      length_of_stay +
      emergency_admission,
    family = binomial,
    data = boot_data
  )

  boot_pred_boot <- predict(
    boot_model,
    newdata = boot_data,
    type = "response"
  )

  boot_pred_original <- predict(
    boot_model,
    newdata = patients,
    type = "response"
  )

  auc_boot <- as.numeric(
    pROC::auc(
      boot_data$readmitted_binary,
      boot_pred_boot
    )
  )

  auc_original <- as.numeric(
    pROC::auc(
      patients$readmitted_binary,
      boot_pred_original
    )
  )

  bootstrap_auc[b] <- auc_boot
  optimism_auc[b] <- auc_boot - auc_original
}

mean_optimism <- mean(optimism_auc)

apparent_auc <- as.numeric(
  pROC::auc(
    patients$readmitted_binary,
    predict(model_full, type = "response")
  )
)

optimism_corrected_auc <- apparent_auc - mean_optimism

cat("Bootstrap repetitions:", B, "\n")
cat("Apparent AUC:", apparent_auc, "\n")
cat("Mean bootstrap optimism:", mean_optimism, "\n")
cat("Optimism-corrected AUC:", optimism_corrected_auc, "\n")

# Store overfitting assessment results
overfitting_results <- data.frame(
  Metric = c(
    "Total observations",
    "Readmission events",
    "Non-readmission observations",
    "Number of predictors",
    "Events per predictor",
    "AIC",
    "BIC",
    "Null deviance",
    "Residual deviance",
    "Pseudo-R2",
    "Bootstrap repetitions",
    "Apparent AUC",
    "Mean bootstrap optimism",
    "Optimism-corrected AUC"
  ),
  Value = c(
    n_total,
    n_events,
    n_nonevents,
    n_predictors,
    epp,
    model_full_aic,
    model_full_bic,
    model_full_null_deviance,
    model_full_deviance,
    model_full_pseudo_r2,
    B,
    apparent_auc,
    mean_optimism,
    optimism_corrected_auc
  )
)

print(overfitting_results)

write.csv(
  overfitting_results,
  "reports/overfitting_bootstrap_assessment.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 19. Bootstrap validation of model discrimination
# ------------------------------------------------------------

set.seed(2026)
B_auc <- 200

bootstrap_auc_validation <- numeric(B_auc)

for (b in seq_len(B_auc)) {

  boot_index <- sample(
    seq_len(nrow(patients)),
    size = nrow(patients),
    replace = TRUE
  )

  boot_data <- patients[boot_index, ]

  boot_model <- glm(
    readmitted_binary ~
      age +
      diabetes +
      hypertension +
      previous_admissions +
      length_of_stay +
      emergency_admission,
    family = binomial,
    data = boot_data
  )

  boot_predictions <- predict(
    boot_model,
    newdata = boot_data,
    type = "response"
  )

  bootstrap_auc_validation[b] <- as.numeric(
    pROC::auc(
      boot_data$readmitted_binary,
      boot_predictions
    )
  )
}

bootstrap_auc_mean <- mean(
  bootstrap_auc_validation
)

bootstrap_auc_ci <- quantile(
  bootstrap_auc_validation,
  probs = c(0.025, 0.975),
  na.rm = TRUE
)

cat(
  "Bootstrap validation repetitions:",
  B_auc,
  "\n"
)

cat(
  "Mean bootstrap AUC:",
  bootstrap_auc_mean,
  "\n"
)

cat(
  "Bootstrap AUC 95% CI:",
  bootstrap_auc_ci[1],
  "to",
  bootstrap_auc_ci[2],
  "\n"
)

bootstrap_validation_results <- data.frame(
  Metric = c(
    "Bootstrap repetitions",
    "Mean bootstrap AUC",
    "Bootstrap AUC 95% CI lower",
    "Bootstrap AUC 95% CI upper",
    "Apparent AUC",
    "Optimism-corrected AUC"
  ),
  Value = c(
    B_auc,
    bootstrap_auc_mean,
    bootstrap_auc_ci[1],
    bootstrap_auc_ci[2],
    apparent_auc,
    optimism_corrected_auc
  )
)

print(bootstrap_validation_results)

write.csv(
  bootstrap_validation_results,
  "reports/bootstrap_validation_auc.csv",
  row.names = FALSE
)

# 20. Final reproducibility checks
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
cat("\n========== FINAL INTEGRITY CHECK ==========\n")

main_file <- "R/01_readmission_analysis.R"

cat("Main script exists:", file.exists(main_file), "\n")
cat("Main script lines:",
    length(readLines(main_file, warn = FALSE)), "\n")
cat("Main script size:",
    file.info(main_file)$size, "bytes\n")

cat("\nMain script MD5:\n")
print(tools::md5sum(main_file))

cat("\n--- Reports ---\n")
cat("CSV reports:",
    length(list.files("reports", pattern = "\\.csv$")),
    "\n")

cat("\n--- Figures ---\n")
cat("Figures:",
    length(list.files(
      "figures",
      pattern = "\\.(png|jpg|jpeg)$"
    )),
    "\n")

cat("\n--- Git status ---\n")
system("git status --short")

cat("\n========== FINAL INTEGRITY CHECK COMPLETE ==========\n")
cat("\n========== CREATE MILESTONE 5 GIT CHECKPOINT ==========\n")

system("git add R/01_readmission_analysis.R")
system("git add R/01_readmission_analysis_2043_SAFE.R")
system("git add R/01_readmission_analysis_CLEAN_808.R")
system("git add R/01_readmission_analysis_CLEAN_808_SAFE.R")
system("git add R/01_readmission_analysis_PRE_808_PROMOTION_20260824_220204.R")
system("git add R/01_readmission_analysis_PRE_808_PROMOTION_20260824_220238.R")

cat("\n--- STAGED FILES ---\n")
system("git status --short")

cat("\n========== FILES STAGED FOR MILESTONE 5 ==========\n")
cat("\n========== COMMIT MILESTONE 5 ==========\n")

system(
  'git commit -m "Milestone 5: preserve clean 808-line readmission analysis and recovery backups"'
)

cat("\n========== MILESTONE 5 COMMIT COMPLETE ==========\n")
git_status <- system(
  'git commit -m "Milestone 5: preserve clean 808-line readmission analysis and recovery backups"',
  intern = TRUE
)

cat(git_status, sep = "\n")
cat("\n========== MILESTONE 5 GIT VERIFICATION ==========\n")

system("git status --short")

cat("\n--- LAST COMMIT ---\n")
system("git log -1 --oneline")

cat("\n========== GIT VERIFICATION COMPLETE ==========\n")
cat("\n========== CHECK GITHUB REMOTE ==========\n")
system("git remote -v")
cat("\n========== END REMOTE CHECK ==========\n")
cat("\n========== PUSH MILESTONE 5 TO GITHUB ==========\n")

system("git push origin main")

cat("\n========== GITHUB PUSH COMMAND FINISHED ==========\n")
cat("\n========== FINAL LOCAL/GITHUB SYNC CHECK ==========\n")

system("git fetch origin")
system("git status -sb")

cat("\nLocal commit:\n")
system("git log -1 --oneline")

cat("\nRemote main:\n")
system("git rev-parse origin/main")

cat("\nLocal main:\n")
system("git rev-parse HEAD")

cat("\n========== END SYNC CHECK ==========\n")
cat("\n========== INSPECT CHANGED REPORT ==========\n")

file <- "reports/table1_overall_continuous.csv"

cat("Exists:", file.exists(file), "\n")
cat("Size:", file.info(file)$size, "bytes\n")
cat("MD5:", unname(tools::md5sum(file)), "\n")

cat("\n--- FILE CONTENT ---\n")
print(read.csv(file))

cat("\n--- GIT DIFF ---\n")
system("git diff -- reports/table1_overall_continuous.csv")

cat("\n========== END CHANGED REPORT INSPECTION ==========\n")
cat("\n========== RESTORE FORMATTING-ONLY CSV CHANGE ==========\n")

system("git restore -- reports/table1_overall_continuous.csv")

cat("\n--- STATUS AFTER RESTORE ---\n")
system("git status -sb")

cat("\n========== CSV RESTORE COMPLETE ==========\n")
cat("\n========== PHASE 6: RESEARCH OUTPUT AUDIT ==========\n")

cat("\n--- PROJECT ---\n")
cat("Working directory:", getwd(), "\n")

cat("\n--- MAIN ANALYSIS ---\n")
analysis_file <- "R/01_readmission_analysis.R"

cat("Exists:", file.exists(analysis_file), "\n")
cat(
  "Lines:",
  length(readLines(analysis_file, warn = FALSE)),
  "\n"
)
cat(
  "MD5:",
  unname(tools::md5sum(analysis_file)),
  "\n"
)

cat("\n--- REPORT INVENTORY ---\n")

reports <- list.files(
  "reports",
  pattern = "\\.csv$",
  full.names = TRUE
)

cat("Total reports:", length(reports), "\n\n")

for (i in seq_along(reports)) {
  
  f <- reports[i]
  
  result <- tryCatch(
    {
      x <- read.csv(
        f,
        stringsAsFactors = FALSE
      )
      
      paste0(
        "OK | rows=",
        nrow(x),
        " | columns=",
        ncol(x)
      )
    },
    error = function(e) {
      paste0(
        "ERROR | ",
        conditionMessage(e)
      )
    }
  )
  
  cat(
    sprintf(
      "%02d. %-55s %s\n",
      i,
      basename(f),
      result
    )
  )
}

cat("\n--- FIGURE INVENTORY ---\n")

figures <- list.files(
  "figures",
  pattern = "\\.(png|jpg|jpeg)$",
  full.names = TRUE
)

cat("Total figures:", length(figures), "\n\n")

for (i in seq_along(figures)) {
  
  f <- figures[i]
  
  info <- file.info(f)
  
  cat(
    sprintf(
      "%02d. %-50s %d bytes\n",
      i,
      basename(f),
      info$size
    )
  )
}

cat("\n--- GIT STATUS ---\n")
system("git status -sb")

cat("\n========== PHASE 6 OUTPUT AUDIT COMPLETE ==========\n")
cat("\n============================================================\n")
cat("        PHASE 7 — STATISTICAL INTERPRETATION AUDIT\n")
cat("============================================================\n")

# ------------------------------------------------------------
# 1. Confirm current research script
# ------------------------------------------------------------

analysis_file <- "R/01_readmission_analysis.R"

cat("\n--- RESEARCH SCRIPT ---\n")
cat("Exists:", file.exists(analysis_file), "\n")
cat("Lines:", length(readLines(analysis_file, warn = FALSE)), "\n")
cat("MD5:", unname(tools::md5sum(analysis_file)), "\n")


# ------------------------------------------------------------
# 2. Model results
# ------------------------------------------------------------

model <- read.csv(
  "reports/multivariable_logistic_regression_results.csv"
)

cat("\n--- MULTIVARIABLE MODEL ---\n")
print(model)


# ------------------------------------------------------------
# 3. Identify statistically significant predictors
# ------------------------------------------------------------

significant <- model[
  model$P_value < 0.05,
]

cat("\n--- STATISTICALLY SIGNIFICANT PREDICTORS ---\n")
print(significant)


# ------------------------------------------------------------
# 4. Identify non-significant predictors
# ------------------------------------------------------------

non_significant <- model[
  model$P_value >= 0.05,
]

cat("\n--- NON-SIGNIFICANT PREDICTORS ---\n")
print(non_significant)


# ------------------------------------------------------------
# 5. Bootstrap validation
# ------------------------------------------------------------

bootstrap <- read.csv(
  "reports/bootstrap_validation_auc.csv"
)

cat("\n--- MODEL DISCRIMINATION ---\n")
print(bootstrap)


# ------------------------------------------------------------
# 6. Calculate optimism
# ------------------------------------------------------------

apparent_auc <- bootstrap$Value[
  bootstrap$Metric == "Apparent AUC"
]

corrected_auc <- bootstrap$Value[
  bootstrap$Metric == "Optimism-corrected AUC"
]

optimism <- apparent_auc - corrected_auc

cat("\nApparent AUC:",
    round(apparent_auc, 4), "\n")

cat("Optimism-corrected AUC:",
    round(corrected_auc, 4), "\n")

cat("Estimated optimism:",
    round(optimism, 4), "\n")


# ------------------------------------------------------------
# 7. Calibration
# ------------------------------------------------------------

calibration <- read.csv(
  "reports/model_calibration_results.csv"
)

cat("\n--- CALIBRATION ---\n")
print(calibration)

cal_intercept <- calibration$Value[
  calibration$Metric == "Calibration intercept"
]

cal_slope <- calibration$Value[
  calibration$Metric == "Calibration slope"
]

cat("\nCalibration intercept:",
    format(cal_intercept, scientific = FALSE),
    "\n")

cat("Calibration slope:",
    round(cal_slope, 4),
    "\n")


# ------------------------------------------------------------
# 8. Multicollinearity
# ------------------------------------------------------------

vif <- read.csv(
  "reports/multicollinearity_vif_results.csv"
)

cat("\n--- MULTICOLLINEARITY ---\n")
print(vif)

cat("\nMaximum VIF:",
    round(max(vif$VIF), 4),
    "\n")


# ------------------------------------------------------------
# 9. Overfitting assessment
# ------------------------------------------------------------

overfit <- read.csv(
  "reports/overfitting_bootstrap_assessment.csv"
)

cat("\n--- OVERFITTING ASSESSMENT ---\n")
print(overfit)


# ------------------------------------------------------------
# 10. Event rate
# ------------------------------------------------------------

total_n <- overfit$Value[
  overfit$Metric == "Total observations"
]

events <- overfit$Value[
  overfit$Metric == "Readmission events"
]

non_events <- overfit$Value[
  overfit$Metric == "Non-readmission observations"
]

event_rate <- events / total_n * 100

cat("\n--- OUTCOME DISTRIBUTION ---\n")
cat("Total observations:", total_n, "\n")
cat("Readmission events:", events, "\n")
cat("Non-readmission observations:", non_events, "\n")
cat("Readmission rate:",
    round(event_rate, 2),
    "%\n")


# ------------------------------------------------------------
# 11. Events per predictor
# ------------------------------------------------------------

predictors <- overfit$Value[
  overfit$Metric == "Number of predictors"
]

epp <- events / predictors

cat("\nEvents per predictor:",
    round(epp, 2),
    "\n")


# ------------------------------------------------------------
# 12. Influence diagnostics
# ------------------------------------------------------------

influence <- read.csv(
  "reports/influence_diagnostics_all_observations.csv"
)

max_cook <- max(influence$Cook_Distance)
max_dffits <- max(abs(influence$DFFITS))
max_hat <- max(influence$Hat_Value)

cat("\n--- INFLUENCE DIAGNOSTICS ---\n")
cat("Observations assessed:", nrow(influence), "\n")
cat("Maximum Cook's distance:",
    round(max_cook, 4),
    "\n")
cat("Maximum absolute DFFITS:",
    round(max_dffits, 4),
    "\n")
cat("Maximum hat value:",
    round(max_hat, 4),
    "\n")


# ------------------------------------------------------------
# 13. Sensitivity analysis
# ------------------------------------------------------------

sensitivity <- read.csv(
  "reports/influence_sensitivity_analysis.csv"
)

cat("\n--- INFLUENCE SENSITIVITY ANALYSIS ---\n")
print(sensitivity[
  c(
    "Predictor",
    "Full_Model_OR",
    "Sensitivity_OR",
    "OR_Percent_Change"
  )
])


# ------------------------------------------------------------
# 14. Output inventory
# ------------------------------------------------------------

reports <- list.files(
  "reports",
  pattern = "\\.csv$"
)

figures <- list.files(
  "figures",
  pattern = "\\.(png|jpg|jpeg)$"
)

cat("\n--- OUTPUT INVENTORY ---\n")
cat("CSV reports:", length(reports), "\n")
cat("Figures:", length(figures), "\n")


# ------------------------------------------------------------
# 15. Final Phase 7 summary
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("              PHASE 7 AUDIT SUMMARY\n")
cat("============================================================\n")

cat("Research script:", 
    ifelse(file.exists(analysis_file), "PRESENT", "MISSING"),
    "\n")

cat("Script lines:",
    length(readLines(analysis_file, warn = FALSE)),
    "\n")

cat("Readmission rate:",
    round(event_rate, 2),
    "%\n")

cat("Apparent AUC:",
    round(apparent_auc, 4),
    "\n")

cat("Optimism-corrected AUC:",
    round(corrected_auc, 4),
    "\n")

cat("Calibration slope:",
    round(cal_slope, 4),
    "\n")

cat("Maximum VIF:",
    round(max(vif$VIF), 4),
    "\n")

cat("Events per predictor:",
    round(epp, 2),
    "\n")

cat("Reports:",
    length(reports),
    "\n")

cat("Figures:",
    length(figures),
    "\n")

cat("\n============================================================\n")
cat("        PHASE 7 STATISTICAL AUDIT COMPLETE\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("        PHASE 8 — PUBLICATION RESULTS SUMMARY\n")
cat("============================================================\n")

model <- read.csv(
  "reports/multivariable_logistic_regression_results.csv"
)

bootstrap <- read.csv(
  "reports/bootstrap_validation_auc.csv"
)

calibration <- read.csv(
  "reports/model_calibration_results.csv"
)

overfit <- read.csv(
  "reports/overfitting_bootstrap_assessment.csv"
)

vif <- read.csv(
  "reports/multicollinearity_vif_results.csv"
)

cat("\n--- STUDY POPULATION ---\n")

total_n <- overfit$Value[
  overfit$Metric == "Total observations"
]

events <- overfit$Value[
  overfit$Metric == "Readmission events"
]

non_events <- overfit$Value[
  overfit$Metric == "Non-readmission observations"
]

cat("Total patients:", total_n, "\n")
cat("Readmission events:", events, "\n")
cat("Non-readmission observations:", non_events, "\n")
cat(
  "Readmission rate:",
  round(100 * events / total_n, 1),
  "%\n"
)


cat("\n--- MULTIVARIABLE ASSOCIATIONS ---\n")

for (i in seq_len(nrow(model))) {
  
  cat(
    model$Predictor[i],
    ": OR = ",
    round(model$Adjusted_OR[i], 3),
    " (95% CI ",
    round(model$CI_lower[i], 3),
    "–",
    round(model$CI_upper[i], 3),
    "), P = ",
    format.pval(model$P_value[i], digits = 3),
    "\n",
    sep = ""
  )
}


cat("\n--- MODEL DISCRIMINATION ---\n")

apparent_auc <- bootstrap$Value[
  bootstrap$Metric == "Apparent AUC"
]

corrected_auc <- bootstrap$Value[
  bootstrap$Metric == "Optimism-corrected AUC"
]

lower_auc <- bootstrap$Value[
  bootstrap$Metric == "Bootstrap AUC 95% CI lower"
]

upper_auc <- bootstrap$Value[
  bootstrap$Metric == "Bootstrap AUC 95% CI upper"
]

cat(
  "Apparent AUC:",
  round(apparent_auc, 3),
  "\n"
)

cat(
  "Mean bootstrap AUC:",
  round(
    bootstrap$Value[
      bootstrap$Metric == "Mean bootstrap AUC"
    ],
    3
  ),
  "\n"
)

cat(
  "Bootstrap AUC 95% CI:",
  round(lower_auc, 3),
  "–",
  round(upper_auc, 3),
  "\n"
)

cat(
  "Optimism-corrected AUC:",
  round(corrected_auc, 3),
  "\n"
)


cat("\n--- CALIBRATION ---\n")

cat(
  "Calibration intercept:",
  round(
    calibration$Value[
      calibration$Metric == "Calibration intercept"
    ],
    4
  ),
  "\n"
)

cat(
  "Calibration slope:",
  round(
    calibration$Value[
      calibration$Metric == "Calibration slope"
    ],
    4
  ),
  "\n"
)


cat("\n--- MULTICOLLINEARITY ---\n")

cat(
  "Maximum VIF:",
  round(max(vif$VIF), 3),
  "\n"
)


cat("\n--- MODEL COMPLEXITY ---\n")

cat(
  "Number of predictors:",
  overfit$Value[
    overfit$Metric == "Number of predictors"
  ],
  "\n"
)

cat(
  "Events per predictor:",
  round(
    overfit$Value[
      overfit$Metric == "Events per predictor"
    ],
    2
  ),
  "\n"
)


cat("\n============================================================\n")
cat("        PHASE 8 RESULTS SUMMARY COMPLETE\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("      PHASE 8B — CREATE PUBLICATION TABLE 2\n")
cat("============================================================\n")

model <- read.csv(
  "reports/multivariable_logistic_regression_results.csv"
)

table2 <- data.frame(
  Predictor = model$Predictor,
  Adjusted_OR = round(model$Adjusted_OR, 3),
  CI_95 = paste0(
    round(model$CI_lower, 3),
    "–",
    round(model$CI_upper, 3)
  ),
  P_value = ifelse(
    model$P_value < 0.001,
    "<0.001",
    format.pval(
      model$P_value,
      digits = 3,
      eps = 0.001
    )
  ),
  stringsAsFactors = FALSE
)

names(table2) <- c(
  "Predictor",
  "Adjusted OR",
  "95% CI",
  "P-value"
)

publication_file <-
  "reports/table2_multivariable_logistic_regression_publication.csv"

write.csv(
  table2,
  publication_file,
  row.names = FALSE,
  quote = TRUE
)

cat("\n--- TABLE 2 ---\n")
print(table2)

cat(
  "\nPublication table exists:",
  file.exists(publication_file),
  "\n"
)

cat(
  "Rows:",
  nrow(table2),
  "\n"
)

cat(
  "Columns:",
  ncol(table2),
  "\n"
)

cat(
  "Size:",
  file.info(publication_file)$size,
  "bytes\n"
)

cat(
  "MD5:",
  unname(tools::md5sum(publication_file)),
  "\n"
)

cat("\n============================================================\n")
cat("       PHASE 8B TABLE 2 CREATION COMPLETE\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       PHASE 8C — MODEL PERFORMANCE TABLE\n")
cat("============================================================\n")

bootstrap <- read.csv(
  "reports/bootstrap_validation_auc.csv"
)

calibration <- read.csv(
  "reports/model_calibration_results.csv"
)

overfit <- read.csv(
  "reports/overfitting_bootstrap_assessment.csv"
)

vif <- read.csv(
  "reports/multicollinearity_vif_results.csv"
)

model_performance <- data.frame(
  Metric = c(
    "Apparent AUC",
    "Mean bootstrap AUC",
    "Bootstrap AUC 95% CI",
    "Optimism-corrected AUC",
    "Calibration intercept",
    "Calibration slope",
    "Maximum VIF",
    "Events per predictor"
  ),
  
  Result = c(
    sprintf(
      "%.3f",
      bootstrap$Value[
        bootstrap$Metric == "Apparent AUC"
      ]
    ),
    
    sprintf(
      "%.3f",
      bootstrap$Value[
        bootstrap$Metric == "Mean bootstrap AUC"
      ]
    ),
    
    sprintf(
      "%.3f–%.3f",
      bootstrap$Value[
        bootstrap$Metric == "Bootstrap AUC 95% CI lower"
      ],
      bootstrap$Value[
        bootstrap$Metric == "Bootstrap AUC 95% CI upper"
      ]
    ),
    
    sprintf(
      "%.3f",
      bootstrap$Value[
        bootstrap$Metric == "Optimism-corrected AUC"
      ]
    ),
    
    sprintf(
      "%.3f",
      calibration$Value[
        calibration$Metric == "Calibration intercept"
      ]
    ),
    
    sprintf(
      "%.3f",
      calibration$Value[
        calibration$Metric == "Calibration slope"
      ]
    ),
    
    sprintf(
      "%.3f",
      max(vif$VIF)
    ),
    
    sprintf(
      "%.2f",
      overfit$Value[
        overfit$Metric == "Events per predictor"
      ]
    )
  ),
  
  stringsAsFactors = FALSE
)

publication_file <-
  "reports/table3_model_performance_publication.csv"

write.csv(
  model_performance,
  publication_file,
  row.names = FALSE,
  quote = TRUE
)

cat("\n--- TABLE 3 ---\n")
print(model_performance)

cat(
  "\nPublication table exists:",
  file.exists(publication_file),
  "\n"
)

cat(
  "Rows:",
  nrow(model_performance),
  "\n"
)

cat(
  "Columns:",
  ncol(model_performance),
  "\n"
)

cat(
  "Size:",
  file.info(publication_file)$size,
  "bytes\n"
)

cat(
  "MD5:",
  unname(tools::md5sum(publication_file)),
  "\n"
)

cat("\n============================================================\n")
cat("       PHASE 8C MODEL PERFORMANCE TABLE COMPLETE\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       PHASE 9 — CREATE RESULTS NARRATIVE\n")
cat("============================================================\n")

model <- read.csv(
  "reports/multivariable_logistic_regression_results.csv"
)

bootstrap <- read.csv(
  "reports/bootstrap_validation_auc.csv"
)

calibration <- read.csv(
  "reports/model_calibration_results.csv"
)

overfit <- read.csv(
  "reports/overfitting_bootstrap_assessment.csv"
)

vif <- read.csv(
  "reports/multicollinearity_vif_results.csv"
)

total_n <- overfit$Value[
  overfit$Metric == "Total observations"
]

events <- overfit$Value[
  overfit$Metric == "Readmission events"
]

non_events <- overfit$Value[
  overfit$Metric == "Non-readmission observations"
]

event_rate <- 100 * events / total_n

age <- model[model$Predictor == "Age", ]
diabetes <- model[model$Predictor == "Diabetes", ]
hypertension <- model[model$Predictor == "Hypertension", ]
previous <- model[model$Predictor == "Previous admissions", ]
los <- model[model$Predictor == "Length of stay", ]
emergency <- model[model$Predictor == "Emergency admission", ]

apparent_auc <- bootstrap$Value[
  bootstrap$Metric == "Apparent AUC"
]

corrected_auc <- bootstrap$Value[
  bootstrap$Metric == "Optimism-corrected AUC"
]

auc_lower <- bootstrap$Value[
  bootstrap$Metric == "Bootstrap AUC 95% CI lower"
]

auc_upper <- bootstrap$Value[
  bootstrap$Metric == "Bootstrap AUC 95% CI upper"
]

cal_intercept <- calibration$Value[
  calibration$Metric == "Calibration intercept"
]

cal_slope <- calibration$Value[
  calibration$Metric == "Calibration slope"
]

max_vif <- max(vif$VIF)

results_text <- paste0(
  
  "RESULTS\n\n",
  
  "Study population\n",
  "The analysis included ",
  total_n,
  " patients, of whom ",
  events,
  " experienced the study outcome and ",
  non_events,
  " did not. The overall readmission rate was ",
  sprintf("%.1f", event_rate),
  "%.\n\n",
  
  "Multivariable analysis\n",
  "In the multivariable logistic regression model, increasing age was associated with higher odds of readmission (adjusted OR ",
  sprintf("%.3f", age$Adjusted_OR),
  ", 95% CI ",
  sprintf("%.3f", age$CI_lower),
  "–",
  sprintf("%.3f", age$CI_upper),
  ", P < 0.001). Hypertension was also associated with increased odds of readmission (adjusted OR ",
  sprintf("%.3f", hypertension$Adjusted_OR),
  ", 95% CI ",
  sprintf("%.3f", hypertension$CI_lower),
  "–",
  sprintf("%.3f", hypertension$CI_upper),
  ", P = ",
  format.pval(hypertension$P_value, digits = 3),
  ").\n\n",
  
  "Previous admissions were associated with higher odds of readmission (adjusted OR ",
  sprintf("%.3f", previous$Adjusted_OR),
  ", 95% CI ",
  sprintf("%.3f", previous$CI_lower),
  "–",
  sprintf("%.3f", previous$CI_upper),
  ", P < 0.001). Longer length of stay was similarly associated with increased odds of readmission (adjusted OR ",
  sprintf("%.3f", los$Adjusted_OR),
  ", 95% CI ",
  sprintf("%.3f", los$CI_lower),
  "–",
  sprintf("%.3f", los$CI_upper),
  ", P < 0.001). Emergency admission was associated with increased odds of readmission (adjusted OR ",
  sprintf("%.3f", emergency$Adjusted_OR),
  ", 95% CI ",
  sprintf("%.3f", emergency$CI_lower),
  "–",
  sprintf("%.3f", emergency$CI_upper),
  ", P = ",
  format.pval(emergency$P_value, digits = 3),
  ").\n\n",
  
  "Diabetes was not independently associated with readmission after adjustment (adjusted OR ",
  sprintf("%.3f", diabetes$Adjusted_OR),
  ", 95% CI ",
  sprintf("%.3f", diabetes$CI_lower),
  "–",
  sprintf("%.3f", diabetes$CI_upper),
  ", P = ",
  format.pval(diabetes$P_value, digits = 3),
  ").\n\n",
  
  "Model performance\n",
  "The apparent area under the receiver operating characteristic curve (AUC) was ",
  sprintf("%.3f", apparent_auc),
  ". Internal bootstrap validation produced a mean AUC of ",
  sprintf("%.3f", bootstrap$Value[
    bootstrap$Metric == "Mean bootstrap AUC"
  ]),
  ", with a bootstrap 95% CI of ",
  sprintf("%.3f", auc_lower),
  "–",
  sprintf("%.3f", auc_upper),
  ". After correction for bootstrap-estimated optimism, the AUC was ",
  sprintf("%.3f", corrected_auc),
  ".\n\n",
  
  "Calibration and model diagnostics\n",
  "The calibration intercept was approximately ",
  sprintf("%.3f", cal_intercept),
  " and the calibration slope was ",
  sprintf("%.3f", cal_slope),
  ". Variance inflation factors were low, with a maximum VIF of ",
  sprintf("%.3f", max_vif),
  ", indicating no substantial multicollinearity among the model predictors. The model included six predictors and had ",
  sprintf("%.2f", overfit$Value[
    overfit$Metric == "Events per predictor"
  ]),
  " events per predictor.\n\n",
  
  "These findings indicate that the model demonstrated moderate discrimination after internal optimism correction, while the calibration and multicollinearity diagnostics were favorable within this dataset."
  
)

results_file <-
  "reports/manuscript_results_narrative.txt"

writeLines(
  results_text,
  results_file
)

cat("\n--- MANUSCRIPT RESULTS NARRATIVE ---\n")
cat(results_text)

cat("\n\nFile created:",
    file.exists(results_file),
    "\n")

cat("Lines:",
    length(readLines(results_file, warn = FALSE)),
    "\n")

cat("Size:",
    file.info(results_file)$size,
    "bytes\n")

cat("MD5:",
    unname(tools::md5sum(results_file)),
    "\n")

cat("\n============================================================\n")
cat("       PHASE 9 RESULTS NARRATIVE COMPLETE\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("      PHASE 10 — INTERPRETATION & LIMITATIONS AUDIT\n")
cat("============================================================\n")

# ------------------------------------------------------------
# 1. Load validated results
# ------------------------------------------------------------

model <- read.csv(
  "reports/multivariable_logistic_regression_results.csv"
)

bootstrap <- read.csv(
  "reports/bootstrap_validation_auc.csv"
)

calibration <- read.csv(
  "reports/model_calibration_results.csv"
)

overfit <- read.csv(
  "reports/overfitting_bootstrap_assessment.csv"
)

vif <- read.csv(
  "reports/multicollinearity_vif_results.csv"
)

sensitivity <- read.csv(
  "reports/influence_sensitivity_analysis.csv"
)

influence <- read.csv(
  "reports/influence_diagnostics_all_observations.csv"
)


# ------------------------------------------------------------
# 2. Study population
# ------------------------------------------------------------

total_n <- overfit$Value[
  overfit$Metric == "Total observations"
]

events <- overfit$Value[
  overfit$Metric == "Readmission events"
]

non_events <- overfit$Value[
  overfit$Metric == "Non-readmission observations"
]

event_rate <- 100 * events / total_n

cat("\n========== STUDY POPULATION ==========\n")
cat("Total observations:", total_n, "\n")
cat("Readmission events:", events, "\n")
cat("Non-readmission observations:", non_events, "\n")
cat("Readmission rate:", round(event_rate, 2), "%\n")


# ------------------------------------------------------------
# 3. Independent predictors
# ------------------------------------------------------------

cat("\n========== INDEPENDENT ASSOCIATIONS ==========\n")

model$Significance <- ifelse(
  model$P_value < 0.05,
  "Statistically significant",
  "Not statistically significant"
)

print(
  model[
    c(
      "Predictor",
      "Adjusted_OR",
      "CI_lower",
      "CI_upper",
      "P_value",
      "Significance"
    )
  ]
)


# ------------------------------------------------------------
# 4. Effect direction
# ------------------------------------------------------------

cat("\n========== EFFECT DIRECTIONS ==========\n")

for (i in seq_len(nrow(model))) {
  
  direction <- ifelse(
    model$Adjusted_OR > 1,
    "higher odds",
    "lower odds"
  )
  
  cat(
    model$Predictor[i],
    ": ",
    direction[i],
    " of readmission; OR = ",
    round(model$Adjusted_OR[i], 3),
    "\n",
    sep = ""
  )
}


# ------------------------------------------------------------
# 5. Strongest adjusted association
# ------------------------------------------------------------

largest_or <- which.max(model$Adjusted_OR)

cat("\n========== LARGEST ADJUSTED ASSOCIATION ==========\n")

cat(
  "Predictor:",
  model$Predictor[largest_or],
  "\n"
)

cat(
  "Adjusted OR:",
  round(model$Adjusted_OR[largest_or], 3),
  "\n"
)

cat(
  "95% CI:",
  round(model$CI_lower[largest_or], 3),
  "–",
  round(model$CI_upper[largest_or], 3),
  "\n"
)

cat(
  "P-value:",
  format.pval(
    model$P_value[largest_or],
    digits = 4
  ),
  "\n"
)


# ------------------------------------------------------------
# 6. Model discrimination
# ------------------------------------------------------------

apparent_auc <- bootstrap$Value[
  bootstrap$Metric == "Apparent AUC"
]

mean_bootstrap_auc <- bootstrap$Value[
  bootstrap$Metric == "Mean bootstrap AUC"
]

corrected_auc <- bootstrap$Value[
  bootstrap$Metric == "Optimism-corrected AUC"
]

auc_lower <- bootstrap$Value[
  bootstrap$Metric == "Bootstrap AUC 95% CI lower"
]

auc_upper <- bootstrap$Value[
  bootstrap$Metric == "Bootstrap AUC 95% CI upper"
]

optimism <- apparent_auc - corrected_auc

cat("\n========== DISCRIMINATION ==========\n")

cat("Apparent AUC:",
    round(apparent_auc, 4),
    "\n")

cat("Mean bootstrap AUC:",
    round(mean_bootstrap_auc, 4),
    "\n")

cat("Bootstrap 95% CI:",
    round(auc_lower, 4),
    "–",
    round(auc_upper, 4),
    "\n")

cat("Optimism-corrected AUC:",
    round(corrected_auc, 4),
    "\n")

cat("Estimated optimism:",
    round(optimism, 4),
    "\n")


# ------------------------------------------------------------
# 7. Calibration
# ------------------------------------------------------------

cal_intercept <- calibration$Value[
  calibration$Metric == "Calibration intercept"
]

cal_slope <- calibration$Value[
  calibration$Metric == "Calibration slope"
]

cat("\n========== CALIBRATION ==========\n")

cat(
  "Calibration intercept:",
  format(cal_intercept, scientific = FALSE),
  "\n"
)

cat(
  "Calibration slope:",
  round(cal_slope, 4),
  "\n"
)


# ------------------------------------------------------------
# 8. Multicollinearity
# ------------------------------------------------------------

max_vif <- max(vif$VIF)

cat("\n========== MULTICOLLINEARITY ==========\n")

cat("Maximum VIF:",
    round(max_vif, 4),
    "\n")

cat(
  "Multicollinearity assessment:",
  ifelse(
    max_vif < 5,
    "No substantial multicollinearity detected by VIF.",
    "Potential multicollinearity requires further assessment."
  ),
  "\n"
)


# ------------------------------------------------------------
# 9. Events per predictor
# ------------------------------------------------------------

epp <- overfit$Value[
  overfit$Metric == "Events per predictor"
]

cat("\n========== MODEL COMPLEXITY ==========\n")

cat(
  "Number of predictors:",
  overfit$Value[
    overfit$Metric == "Number of predictors"
  ],
  "\n"
)

cat(
  "Events per predictor:",
  round(epp, 2),
  "\n"
)


# ------------------------------------------------------------
# 10. Influence diagnostics
# ------------------------------------------------------------

cat("\n========== INFLUENCE DIAGNOSTICS ==========\n")

cat(
  "Observations assessed:",
  nrow(influence),
  "\n"
)

cat(
  "Maximum Cook's distance:",
  round(
    max(influence$Cook_Distance),
    4
  ),
  "\n"
)

cat(
  "Maximum absolute DFFITS:",
  round(
    max(abs(influence$DFFITS)),
    4
  ),
  "\n"
)

cat(
  "Maximum hat value:",
  round(
    max(influence$Hat_Value),
    4
  ),
  "\n"
)


# ------------------------------------------------------------
# 11. Sensitivity analysis
# ------------------------------------------------------------

cat("\n========== SENSITIVITY ANALYSIS ==========\n")

print(
  sensitivity[
    c(
      "Predictor",
      "Full_Model_OR",
      "Sensitivity_OR",
      "OR_Percent_Change"
    )
  ]
)


# ------------------------------------------------------------
# 12. Largest sensitivity change
# ------------------------------------------------------------

largest_change <- which.max(
  abs(sensitivity$OR_Percent_Change)
)

cat("\nLargest OR percentage change after sensitivity analysis:\n")

cat(
  "Predictor:",
  sensitivity$Predictor[largest_change],
  "\n"
)

cat(
  "Full model OR:",
  round(
    sensitivity$Full_Model_OR[largest_change],
    3
  ),
  "\n"
)

cat(
  "Sensitivity OR:",
  round(
    sensitivity$Sensitivity_OR[largest_change],
    3
  ),
  "\n"
)

cat(
  "Percentage change:",
  round(
    sensitivity$OR_Percent_Change[largest_change],
    2
  ),
  "%\n"
)


# ------------------------------------------------------------
# 13. Interpretation safeguards
# ------------------------------------------------------------

cat("\n========== INTERPRETATION SAFEGUARDS ==========\n")

cat(
  "1. Associations should not automatically be described as causal.\n"
)

cat(
  "2. Odds ratios describe associations with the modeled readmission outcome.\n"
)

cat(
  "3. AUC describes discrimination, not clinical usefulness by itself.\n"
)

cat(
  "4. Calibration results should be interpreted within this dataset.\n"
)

cat(
  "5. Bootstrap validation provides internal validation, not external validation.\n"
)

cat(
  "6. Findings should not be generalized beyond the study population without external validation.\n"
)

cat(
  "7. Sensitivity analysis should be used to assess robustness, not to imply causality.\n"
)


# ------------------------------------------------------------
# 14. Preliminary limitations
# ------------------------------------------------------------

cat("\n========== PRELIMINARY LIMITATIONS ==========\n")

limitations <- c(
  "The dataset is synthetic and therefore does not establish clinical effectiveness in real-world patients.",
  "Internal bootstrap validation does not replace external validation in an independent population.",
  "The analysis evaluates associations and should not be interpreted as demonstrating causation.",
  "Model performance may differ in other hospitals, populations, or clinical settings.",
  "The observed outcome prevalence may differ from that of real-world clinical populations.",
  "Clinical utility, decision-curve performance, and implementation consequences have not yet been established."
)

for (i in seq_along(limitations)) {
  cat(i, ". ", limitations[i], "\n", sep = "")
}


# ------------------------------------------------------------
# 15. Final audit summary
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("       PHASE 10 INTERPRETATION AUDIT SUMMARY\n")
cat("============================================================\n")

cat("Sample size:", total_n, "\n")
cat("Events:", events, "\n")
cat("Event rate:", round(event_rate, 2), "%\n")
cat("Optimism-corrected AUC:", round(corrected_auc, 4), "\n")
cat("Calibration slope:", round(cal_slope, 4), "\n")
cat("Maximum VIF:", round(max_vif, 4), "\n")
cat("Events per predictor:", round(epp, 2), "\n")
cat(
  "Largest sensitivity change:",
  round(
    sensitivity$OR_Percent_Change[largest_change],
    2
  ),
  "%\n"
)

cat("\n============================================================\n")
cat("       PHASE 10 INTERPRETATION AUDIT COMPLETE\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       PHASE 11 PUBLICATION PACKAGE AUDIT\n")
cat("============================================================\n")

cat("\n--- Publication files ---\n")

publication_files <- list.files(
  "reports",
  pattern = "\\.(csv|txt|md|docx|pdf)$",
  full.names = FALSE
)

print(publication_files)

cat("\nPublication file count:",
    length(publication_files),
    "\n")

cat("\n--- Core statistical outputs ---\n")

core_files <- c(
  "multivariable_logistic_regression_results.csv",
  "univariable_logistic_regression_results.csv",
  "bootstrap_validation_auc.csv",
  "model_calibration_results.csv",
  "multicollinearity_vif_results.csv",
  "overfitting_bootstrap_assessment.csv",
  "influence_sensitivity_analysis.csv"
)

for (f in core_files) {
  cat(
    sprintf(
      "%-55s exists=%s\n",
      f,
      file.exists(file.path("reports", f))
    )
  )
}

cat("\n--- Figures ---\n")

figures <- list.files(
  "figures",
  pattern = "\\.(png|jpg|jpeg)$",
  full.names = FALSE
)

for (i in seq_along(figures)) {
  f <- file.path("figures", figures[i])
  
  cat(
    sprintf(
      "%02d. %-50s %d bytes\n",
      i,
      figures[i],
      file.info(f)$size
    )
  )
}

cat("\nFigure count:", length(figures), "\n")

cat("\n--- Final research script ---\n")

analysis_file <- "R/01_readmission_analysis.R"

cat(
  "Exists:",
  file.exists(analysis_file),
  "\n"
)

cat(
  "Lines:",
  length(readLines(analysis_file, warn = FALSE)),
  "\n"
)

cat(
  "MD5:",
  unname(tools::md5sum(analysis_file)),
  "\n"
)

cat("\n============================================================\n")
cat("       PHASE 11 PUBLICATION PACKAGE AUDIT COMPLETE\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       PHASE 12 FINAL REPRODUCIBILITY CHECK\n")
cat("============================================================\n")

cat("\n--- MAIN ANALYSIS SCRIPT ---\n")

analysis_file <- "R/01_readmission_analysis.R"

cat("Exists:", file.exists(analysis_file), "\n")
cat(
  "Lines:",
  length(readLines(analysis_file, warn = FALSE)),
  "\n"
)
cat(
  "MD5:",
  unname(tools::md5sum(analysis_file)),
  "\n"
)

cat("\n--- SAFETY COPY ---\n")

safe_file <- "R/01_readmission_analysis_CLEAN_808_SAFE.R"

cat("Exists:", file.exists(safe_file), "\n")
cat(
  "Lines:",
  length(readLines(safe_file, warn = FALSE)),
  "\n"
)
cat(
  "MD5:",
  unname(tools::md5sum(safe_file)),
  "\n"
)

cat("\n--- REPORTS ---\n")

reports <- list.files(
  "reports",
  pattern = "\\.csv$",
  full.names = FALSE
)

cat("CSV reports:", length(reports), "\n")

cat("\n--- FIGURES ---\n")

figures <- list.files(
  "figures",
  pattern = "\\.(png|jpg|jpeg)$",
  full.names = FALSE
)

cat("Figures:", length(figures), "\n")

cat("\n--- GIT STATUS ---\n")

system("git status -sb")

cat("\n--- LOCAL COMMIT ---\n")

system("git rev-parse HEAD")

cat("\n--- GITHUB MAIN ---\n")

system("git rev-parse origin/main")

cat("\n============================================================\n")
cat("       PHASE 12 FINAL REPRODUCIBILITY CHECK COMPLETE\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       PHASE 13 FINAL PROJECT INVENTORY\n")
cat("============================================================\n")

cat("\n--- PROJECT ROOT ---\n")
cat(normalizePath(".", winslash = "/", mustWork = TRUE), "\n")

cat("\n--- RESEARCH SCRIPT ---\n")

analysis_file <- "R/01_readmission_analysis.R"

cat(
  "File:",
  normalizePath(analysis_file, winslash = "/", mustWork = TRUE),
  "\n"
)

cat(
  "Lines:",
  length(readLines(analysis_file, warn = FALSE)),
  "\n"
)

cat(
  "Size:",
  file.info(analysis_file)$size,
  "bytes\n"
)

cat(
  "MD5:",
  unname(tools::md5sum(analysis_file)),
  "\n"
)

cat("\n--- REPORT INVENTORY ---\n")

reports <- list.files(
  "reports",
  pattern = "\\.csv$",
  full.names = FALSE
)

for (i in seq_along(reports)) {
  cat(
    sprintf(
      "%02d. %s\n",
      i,
      reports[i]
    )
  )
}

cat(
  "\nTotal reports:",
  length(reports),
  "\n"
)

cat("\n--- FIGURE INVENTORY ---\n")

figures <- list.files(
  "figures",
  pattern = "\\.(png|jpg|jpeg)$",
  full.names = FALSE
)

for (i in seq_along(figures)) {
  f <- file.path("figures", figures[i])
  
  cat(
    sprintf(
      "%02d. %-50s %d bytes\n",
      i,
      figures[i],
      file.info(f)$size
    )
  )
}

cat(
  "\nTotal figures:",
  length(figures),
  "\n"
)

cat("\n--- GIT STATUS ---\n")
system("git status -sb")

cat("\n--- LAST COMMIT ---\n")
system("git log -1 --oneline")

cat("\n============================================================\n")
cat("       PHASE 13 FINAL PROJECT INVENTORY COMPLETE\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       PHASE 14 FINAL MILESTONE 5 BACKUP\n")
cat("============================================================\n")

project <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)

backup_root <- "D:/MedResearch/Milestone5_Backups"

timestamp <- format(
  Sys.time(),
  "%Y%m%d_%H%M%S"
)

final_backup <- file.path(
  backup_root,
  paste0(
    "MedResearch-01_Milestone5_FINAL_",
    timestamp
  )
)

dir.create(
  final_backup,
  recursive = TRUE,
  showWarnings = FALSE
)

files <- list.files(
  project,
  recursive = TRUE,
  all.files = TRUE,
  full.names = TRUE,
  include.dirs = TRUE
)

project_norm <- normalizePath(
  project,
  winslash = "/",
  mustWork = TRUE
)

for (f in files) {
  
  f_norm <- normalizePath(
    f,
    winslash = "/",
    mustWork = FALSE
  )
  
  relative <- substring(
    f_norm,
    nchar(project_norm) + 2
  )
  
  destination <- file.path(
    final_backup,
    relative
  )
  
  if (dir.exists(f)) {
    
    dir.create(
      destination,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
  } else {
    
    dir.create(
      dirname(destination),
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    file.copy(
      f,
      destination,
      overwrite = TRUE
    )
  }
}

cat("\nBackup exists:",
    dir.exists(final_backup),
    "\n")

backup_analysis <- file.path(
  final_backup,
  "R",
  "01_readmission_analysis.R"
)

cat(
  "Backup analysis script exists:",
  file.exists(backup_analysis),
  "\n"
)

if (file.exists(backup_analysis)) {
  
  cat(
    "Backup script lines:",
    length(
      readLines(
        backup_analysis,
        warn = FALSE
      )
    ),
    "\n"
  )
  
  cat(
    "Backup script MD5:",
    unname(
      tools::md5sum(
        backup_analysis
      )
    ),
    "\n"
  )
}

cat(
  "\nFINAL BACKUP LOCATION:\n",
  final_backup,
  "\n"
)

cat("\n============================================================\n")
cat("       PHASE 14 FINAL BACKUP COMPLETE\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       PHASE 15 FINAL MILESTONE 5 CLOSURE CHECK\n")
cat("============================================================\n")

cat("\n--- MAIN SCRIPT ---\n")

main <- "R/01_readmission_analysis.R"

cat("Exists:", file.exists(main), "\n")
cat("Lines:", length(readLines(main, warn = FALSE)), "\n")
cat("MD5:", unname(tools::md5sum(main)), "\n")

cat("\n--- REPORTS ---\n")

reports <- list.files(
  "reports",
  pattern = "\\.csv$",
  full.names = FALSE
)

cat("CSV reports:", length(reports), "\n")

cat("\n--- FIGURES ---\n")

figures <- list.files(
  "figures",
  pattern = "\\.(png|jpg|jpeg)$",
  full.names = FALSE
)

cat("Figures:", length(figures), "\n")

cat("\n--- GIT STATUS ---\n")
system("git status -sb")

cat("\n--- LOCAL HEAD ---\n")
system("git rev-parse HEAD")

cat("\n--- GITHUB ORIGIN/MAIN ---\n")
system("git rev-parse origin/main")

cat("\n--- LAST COMMIT ---\n")
system("git log -1 --oneline")

cat("\n============================================================\n")
cat("       PHASE 15 FINAL CLOSURE CHECK COMPLETE\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       PHASE 16 COMMIT FINAL PUBLICATION OUTPUTS\n")
cat("============================================================\n")

system("git add reports/manuscript_results_narrative.txt")
system("git add reports/table2_multivariable_logistic_regression_publication.csv")
system("git add reports/table3_model_performance_publication.csv")

cat("\n--- STAGED PUBLICATION FILES ---\n")
system("git status --short")

cat("\n============================================================\n")
cat("       PHASE 16 FILES STAGED\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       PHASE 17 COMMIT FINAL PUBLICATION OUTPUTS\n")
cat("============================================================\n")

commit_result <- system(
  'git commit -m "Phase 8-10: add publication tables and results narrative"',
  intern = TRUE
)

cat(commit_result, sep = "\n")

cat("\n--- GIT STATUS AFTER COMMIT ---\n")
system("git status -sb")

cat("\n--- NEW HEAD ---\n")
system("git log -1 --oneline")

cat("\n============================================================\n")
cat("       PHASE 17 COMMIT COMPLETE\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       PHASE 18 PUSH FINAL PUBLICATION COMMIT\n")
cat("============================================================\n")

push_result <- system(
  "git push origin main",
  intern = TRUE
)

cat(push_result, sep = "\n")

cat("\n============================================================\n")
cat("       PHASE 18 GITHUB PUSH COMPLETE\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       PHASE 19 FINAL GITHUB SYNCHRONIZATION CHECK\n")
cat("============================================================\n")

system("git fetch origin")

cat("\n--- GIT STATUS ---\n")
system("git status -sb")

cat("\n--- LOCAL HEAD ---\n")
local_head <- system(
  "git rev-parse HEAD",
  intern = TRUE
)
cat(local_head, "\n")

cat("\n--- GITHUB ORIGIN/MAIN ---\n")
remote_head <- system(
  "git rev-parse origin/main",
  intern = TRUE
)
cat(remote_head, "\n")

cat("\n--- HEADS IDENTICAL ---\n")
cat(
  identical(
    local_head,
    remote_head
  ),
  "\n"
)

cat("\n--- LAST COMMIT ---\n")
system("git log -1 --oneline")

cat("\n============================================================\n")
cat("       PHASE 19 FINAL GITHUB CHECK COMPLETE\n")
cat("============================================================\n")