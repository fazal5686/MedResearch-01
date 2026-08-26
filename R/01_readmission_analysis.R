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
cat("\n============================================================\n")
cat("       POST-RESTORATION INTEGRITY CHECK\n")
cat("============================================================\n")

cat("\n--- ACTIVE FILE ---\n")
cat("Path:", main, "\n")
cat("Lines:", length(readLines(main, warn = FALSE)), "\n")
cat("MD5:", unname(tools::md5sum(main)), "\n")

cat("\n--- AUTHORITATIVE CLEAN SOURCE ---\n")
cat("Path:", source_808, "\n")
cat("Lines:", length(readLines(source_808, warn = FALSE)), "\n")
cat("MD5:", unname(tools::md5sum(source_808)), "\n")

cat("\n--- PRE-RESTORATION BACKUP ---\n")
cat("Path:", backup, "\n")
cat("Exists:", file.exists(backup), "\n")
cat("Lines:", length(readLines(backup, warn = FALSE)), "\n")
cat("MD5:", unname(tools::md5sum(backup)), "\n")

active_md5 <- unname(tools::md5sum(main))
source_md5 <- unname(tools::md5sum(source_808))
backup_md5 <- unname(tools::md5sum(backup))

active_lines <- length(readLines(main, warn = FALSE))
source_lines <- length(readLines(source_808, warn = FALSE))
backup_lines <- length(readLines(backup, warn = FALSE))

if (
  active_lines != 808 ||
  source_lines != 808 ||
  active_md5 != "52b6fe170f6de4b8b32b726c3f66cb2f" ||
  source_md5 != "52b6fe170f6de4b8b32b726c3f66cb2f" ||
  !file.exists(backup) ||
  backup_lines != 1033 ||
  backup_md5 != "0e2c404d2b5beb2087573d9e5118aebe"
) {
  stop("SAFETY STOP: Integrity check failed.")
}

cat("\n============================================================\n")
cat("       ALL INTEGRITY CHECKS PASSED\n")
cat("============================================================\n")
cat("ACTIVE:       808 lines / MD5 VERIFIED\n")
cat("CLEAN SOURCE: 808 lines / MD5 VERIFIED\n")
cat("BACKUP:       1033 lines / MD5 VERIFIED\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       STEP 3 — NON-DESTRUCTIVE R SYNTAX CHECK\n")
cat("============================================================\n")

# Confirm active file identity again
active_lines <- length(readLines(main, warn = FALSE))
active_md5 <- unname(tools::md5sum(main))

cat("Active file:", main, "\n")
cat("Lines:", active_lines, "\n")
cat("MD5:", active_md5, "\n")

if (
  active_lines != 808 ||
  active_md5 != "52b6fe170f6de4b8b32b726c3f66cb2f"
) {
  stop(
    "SAFETY STOP: Active file is no longer the verified 808-line version."
  )
}

# Parse only — does NOT execute the script
parsed <- tryCatch(
  {
    parse(file = main)
  },
  error = function(e) {
    e
  }
)

if (inherits(parsed, "error")) {
  cat("\n============================================================\n")
  cat("       SYNTAX CHECK FAILED\n")
  cat("============================================================\n")
  cat(conditionMessage(parsed), "\n")
  stop("SAFETY STOP: Do not execute the research script.")
}

cat("\n============================================================\n")
cat("       SYNTAX CHECK PASSED\n")
cat("============================================================\n")
cat("R successfully parsed the entire 808-line script.\n")
cat("No code was executed.\n")
cat("No research outputs were modified by this check.\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       STEP 4 — PRE-EXECUTION SAFETY INSPECTION\n")
cat("============================================================\n")

# Re-confirm identity
if (
  length(readLines(main, warn = FALSE)) != 808 ||
  unname(tools::md5sum(main)) !=
  "52b6fe170f6de4b8b32b726c3f66cb2f"
) {
  stop("SAFETY STOP: Active script identity changed.")
}

lines <- readLines(main, warn = FALSE)

cat("\n--- FILE IDENTITY ---\n")
cat("Lines:", length(lines), "\n")
cat("MD5:", unname(tools::md5sum(main)), "\n")

cat("\n--- LIBRARY / PACKAGE CALLS ---\n")
pkg_lines <- grep(
  "^\\s*(library|require)\\s*\\(",
  lines,
  value = TRUE
)
if (length(pkg_lines) == 0) {
  cat("No library()/require() calls detected.\n")
} else {
  cat(paste(pkg_lines, collapse = "\n"), "\n")
}

cat("\n--- FILE/OUTPUT OPERATIONS ---\n")
file_patterns <- c(
  "file\\.remove",
  "unlink",
  "file\\.copy",
  "file\\.rename",
  "write\\.csv",
  "write\\.table",
  "writeLines",
  "saveRDS",
  "save\\(",
  "ggsave",
  "pdf\\(",
  "png\\(",
  "jpeg\\(",
  "rds",
  "csv"
)

matches <- unique(unlist(
  lapply(
    file_patterns,
    function(p) grep(p, lines, ignore.case = TRUE, value = TRUE)
  )
))

if (length(matches) == 0) {
  cat("No matching file/output operations detected.\n")
} else {
  cat(paste(matches, collapse = "\n"), "\n")
}

cat("\n--- SYSTEM / SHELL OPERATIONS ---\n")
system_patterns <- c(
  "system\\(",
  "system2\\(",
  "shell\\(",
  "shell\\.exec",
  "setwd\\(",
  "unlink\\("
)

system_matches <- unique(unlist(
  lapply(
    system_patterns,
    function(p) grep(p, lines, ignore.case = TRUE, value = TRUE)
  )
))

if (length(system_matches) == 0) {
  cat("No system/shell operations detected.\n")
} else {
  cat(paste(system_matches, collapse = "\n"), "\n")
}

cat("\n--- SCRIPT END ---\n")
cat(paste(
  sprintf("%4d: %s", seq(max(1, length(lines) - 19), length(lines)),
          lines[max(1, length(lines) - 19):length(lines)]),
  collapse = "\n"
), "\n")

cat("\n============================================================\n")
cat("       STEP 4 COMPLETE — INSPECTION ONLY\n")
cat("============================================================\n")
cat("NO CODE FROM THE RESEARCH SCRIPT WAS EXECUTED.\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       STEP 4A — DISPLAY SAFETY FINDINGS\n")
cat("============================================================\n")

cat("\n--- FILE IDENTITY ---\n")
cat("Lines:", length(lines), "\n")
cat("MD5:", unname(tools::md5sum(main)), "\n")

cat("\n--- LIBRARY / PACKAGE CALLS ---\n")
pkg_lines <- grep(
  "^\\s*(library|require)\\s*\\(",
  lines,
  value = TRUE
)

if (length(pkg_lines) == 0) {
  cat("NONE DETECTED\n")
} else {
  cat(paste(pkg_lines, collapse = "\n"), "\n")
}

cat("\n--- FILE / OUTPUT OPERATIONS ---\n")
file_patterns <- c(
  "file\\.remove",
  "unlink",
  "file\\.copy",
  "file\\.rename",
  "write\\.csv",
  "write\\.table",
  "writeLines",
  "saveRDS",
  "save\\(",
  "ggsave",
  "pdf\\(",
  "png\\(",
  "jpeg\\("
)

matches <- unique(unlist(
  lapply(
    file_patterns,
    function(p) grep(p, lines, ignore.case = TRUE, value = TRUE)
  )
))

if (length(matches) == 0) {
  cat("NONE DETECTED\n")
} else {
  cat(paste(matches, collapse = "\n"), "\n")
}

cat("\n--- SYSTEM / SHELL OPERATIONS ---\n")
system_patterns <- c(
  "system\\(",
  "system2\\(",
  "shell\\(",
  "shell\\.exec",
  "setwd\\(",
  "unlink\\("
)

system_matches <- unique(unlist(
  lapply(
    system_patterns,
    function(p) grep(p, lines, ignore.case = TRUE, value = TRUE)
  )
))

if (length(system_matches) == 0) {
  cat("NONE DETECTED\n")
} else {
  cat(paste(system_matches, collapse = "\n"), "\n")
}

cat("\n--- FIRST 20 LINES ---\n")
cat(paste(
  sprintf("%4d: %s", 1:min(20, length(lines)),
          lines[1:min(20, length(lines))]),
  collapse = "\n"
), "\n")

cat("\n--- LAST 20 LINES ---\n")
start <- max(1, length(lines) - 19)
cat(paste(
  sprintf("%4d: %s", start:length(lines),
          lines[start:length(lines)]),
  collapse = "\n"
), "\n")

cat("\n============================================================\n")
cat("       STEP 4A COMPLETE — NO RESEARCH CODE EXECUTED\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       STEP 4B — CRITICAL SAFETY SCAN\n")
cat("============================================================\n")

cat("\nFILE IDENTITY\n")
cat("Lines:", length(lines), "\n")
cat("MD5:", unname(tools::md5sum(main)), "\n")

cat("\nDESTRUCTIVE OPERATIONS\n")

danger_patterns <- c(
  "file\\.remove\\s*\\(",
  "unlink\\s*\\(",
  "file\\.rename\\s*\\(",
  "system\\s*\\(",
  "system2\\s*\\(",
  "shell\\s*\\(",
  "shell\\.exec\\s*\\("
)

danger_hits <- unique(unlist(
  lapply(
    danger_patterns,
    function(p) {
      idx <- grep(p, lines, ignore.case = TRUE)
      if (length(idx) == 0) {
        return(character(0))
      }
      sprintf("%4d: %s", idx, lines[idx])
    }
  )
))

if (length(danger_hits) == 0) {
  cat("NONE DETECTED\n")
} else {
  cat(paste(danger_hits, collapse = "\n"), "\n")
}

cat("\nOUTPUT-WRITING OPERATIONS\n")

output_patterns <- c(
  "write\\.csv\\s*\\(",
  "write\\.table\\s*\\(",
  "writeLines\\s*\\(",
  "saveRDS\\s*\\(",
  "save\\s*\\(",
  "ggsave\\s*\\(",
  "pdf\\s*\\(",
  "png\\s*\\(",
  "jpeg\\s*\\("
)

output_hits <- unique(unlist(
  lapply(
    output_patterns,
    function(p) {
      idx <- grep(p, lines, ignore.case = TRUE)
      if (length(idx) == 0) {
        return(character(0))
      }
      sprintf("%4d: %s", idx, lines[idx])
    }
  )
))

if (length(output_hits) == 0) {
  cat("NONE DETECTED\n")
} else {
  cat(paste(output_hits, collapse = "\n"), "\n")
}

cat("\nPACKAGE REQUIREMENTS\n")

pkg_hits <- grep(
  "^\\s*(library|require)\\s*\\(",
  lines,
  ignore.case = TRUE,
  value = TRUE
)

if (length(pkg_hits) == 0) {
  cat("NONE DETECTED\n")
} else {
  cat(paste(pkg_hits, collapse = "\n"), "\n")
}

cat("\n============================================================\n")
cat("       STEP 4B COMPLETE — SCAN ONLY\n")
cat("============================================================\n")
cat("NO RESEARCH CODE WAS EXECUTED.\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       STEP 4C — COMPACT SAFETY SUMMARY\n")
cat("============================================================\n")

# Re-confirm file identity
active_lines <- length(readLines(main, warn = FALSE))
active_md5 <- unname(tools::md5sum(main))

# Search for potentially destructive commands
danger_patterns <- c(
  "file\\.remove\\s*\\(",
  "unlink\\s*\\(",
  "file\\.rename\\s*\\(",
  "system\\s*\\(",
  "system2\\s*\\(",
  "shell\\s*\\(",
  "shell\\.exec\\s*\\("
)

danger_count <- sum(
  sapply(
    danger_patterns,
    function(p) length(grep(p, lines, ignore.case = TRUE))
  )
)

# Search for output-writing commands
output_patterns <- c(
  "write\\.csv\\s*\\(",
  "write\\.table\\s*\\(",
  "writeLines\\s*\\(",
  "saveRDS\\s*\\(",
  "save\\s*\\(",
  "ggsave\\s*\\(",
  "pdf\\s*\\(",
  "png\\s*\\(",
  "jpeg\\s*\\("
)

output_count <- sum(
  sapply(
    output_patterns,
    function(p) length(grep(p, lines, ignore.case = TRUE))
  )
)

# Package calls
pkg_count <- length(
  grep(
    "^\\s*(library|require)\\s*\\(",
    lines,
    ignore.case = TRUE
  )
)

cat("Active lines: ", active_lines, "\n", sep = "")
cat("Active MD5:   ", active_md5, "\n", sep = "")
cat("Dangerous/system calls detected: ", danger_count, "\n", sep = "")
cat("Output-writing calls detected:    ", output_count, "\n", sep = "")
cat("Package calls detected:            ", pkg_count, "\n", sep = "")

cat("\nIdentity verified: ",
    active_lines == 808 &&
      active_md5 == "52b6fe170f6de4b8b32b726c3f66cb2f",
    "\n", sep = "")

cat("\n============================================================\n")
cat("       STEP 4C COMPLETE — NO RESEARCH CODE EXECUTED\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       STEP 4D — THREE SAFETY COUNTS\n")
cat("============================================================\n")

cat("Dangerous/system calls: ", danger_count, "\n", sep = "")
cat("Output-writing calls:   ", output_count, "\n", sep = "")
cat("Package calls:          ", pkg_count, "\n", sep = "")

cat("============================================================\n")
cat("\n============================================================\n")
cat("       STEP 5 — IDENTIFY OUTPUT OPERATIONS\n")
cat("============================================================\n")

output_patterns <- c(
  "write\\.csv\\s*\\(",
  "write\\.table\\s*\\(",
  "writeLines\\s*\\(",
  "saveRDS\\s*\\(",
  "save\\s*\\(",
  "ggsave\\s*\\(",
  "pdf\\s*\\(",
  "png\\s*\\(",
  "jpeg\\s*\\("
)

output_hits <- unique(unlist(
  lapply(
    output_patterns,
    function(p) {
      idx <- grep(p, lines, ignore.case = TRUE)
      if (length(idx) == 0) {
        return(character(0))
      }
      sprintf("%4d: %s", idx, lines[idx])
    }
  )
))

cat(paste(output_hits, collapse = "\n"), "\n")

cat("\n============================================================\n")
cat("       STEP 5 COMPLETE — INSPECTION ONLY\n")
cat("============================================================\n")
cat("NO RESEARCH CODE WAS EXECUTED.\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       STEP 6 — INSPECT OUTPUT DESTINATIONS\n")
cat("============================================================\n")

target_lines <- c(177, 203, 224, 268, 422, 453, 680, 782)

for (ln in target_lines) {
  start <- max(1, ln - 3)
  end <- min(length(lines), ln + 5)
  
  cat("\n------------------------------------------------------------\n")
  cat("Around line", ln, "\n")
  cat("------------------------------------------------------------\n")
  
  cat(
    paste(
      sprintf(
        "%4d: %s",
        start:end,
        lines[start:end]
      ),
      collapse = "\n"
    ),
    "\n"
  )
}

cat("\n============================================================\n")
cat("       STEP 6 COMPLETE — INSPECTION ONLY\n")
cat("============================================================\n")
cat("NO RESEARCH CODE WAS EXECUTED.\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       STEP 6A — OUTPUT DESTINATIONS ONLY\n")
cat("============================================================\n")

for (ln in c(177, 203, 224, 268, 422, 453, 680, 782)) {
  cat("\n--- line ", ln, " ---\n", sep = "")
  cat(
    paste(
      lines[max(1, ln):min(length(lines), ln + 3)],
      collapse = "\n"
    ),
    "\n"
  )
}

cat("\n============================================================\n")
cat("       STEP 6A COMPLETE\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       STEP 6C — COMPACT OUTPUT DESTINATIONS\n")
cat("============================================================\n")

for (ln in c(177, 203, 224, 268, 422, 453, 680, 782)) {
  
  block <- paste(
    lines[ln:min(length(lines), ln + 6)],
    collapse = " "
  )
  
  cat("\n", ln, ": ", block, "\n", sep = "")
}

cat("\n============================================================\n")
cat("       STEP 6C COMPLETE\n")
cat("============================================================\n")
output_inspection_file <- tempfile(fileext = ".txt")

con <- file(output_inspection_file, open = "wt")

for (ln in c(177, 203, 224, 268, 422, 453, 680, 782)) {
  writeLines(
    c(
      paste0("===== OUTPUT LINE ", ln, " ====="),
      lines[ln:min(length(lines), ln + 6)]
    ),
    con
  )
}

close(con)

cat("\n============================================================\n")
cat("       OUTPUT INSPECTION FILE CREATED\n")
cat("============================================================\n")
cat(readLines(output_inspection_file, warn = FALSE), sep = "\n")
cat("\n============================================================\n")
cat("       INSPECTION COMPLETE — NO RESEARCH CODE EXECUTED\n")
cat("============================================================\n")
cat(readLines(output_inspection_file, warn = FALSE), sep = "\n")
cat("\n============================================================\n")
cat("       STEP 7 — CURRENT OUTPUT INVENTORY\n")
cat("============================================================\n")

cat("\n--- REPORTS ---\n")
if (dir.exists("reports")) {
  reports_now <- list.files(
    "reports",
    recursive = TRUE,
    full.names = FALSE
  )
  print(reports_now)
} else {
  cat("reports directory does not exist.\n")
}

cat("\n--- FIGURES ---\n")
if (dir.exists("figures")) {
  figures_now <- list.files(
    "figures",
    recursive = TRUE,
    full.names = FALSE
  )
  print(figures_now)
} else {
  cat("figures directory does not exist.\n")
}

cat("\n============================================================\n")
cat("       STEP 7 COMPLETE — INVENTORY ONLY\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       STEP 7A — REPORTS INVENTORY\n")
cat("============================================================\n")

if (!dir.exists("reports")) {
  cat("REPORTS DIRECTORY DOES NOT EXIST\n")
} else {
  reports_now <- list.files(
    "reports",
    recursive = TRUE,
    full.names = FALSE
  )
  
  cat("Number of report files:", length(reports_now), "\n\n")
  
  if (length(reports_now) > 0) {
    cat(paste(reports_now, collapse = "\n"), "\n")
  } else {
    cat("REPORTS DIRECTORY IS EMPTY\n")
  }
}

cat("\n============================================================\n")
cat("       STEP 7A COMPLETE — INVENTORY ONLY\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       STEP 8 — PROTECT CURRENT RESEARCH OUTPUTS\n")
cat("============================================================\n")

# Create timestamped protection directory
output_backup <- file.path(
  "reports",
  paste0(
    "PRE_808_EXECUTION_OUTPUT_BACKUP_",
    format(Sys.time(), "%Y%m%d_%H%M%S")
  )
)

if (dir.exists(output_backup)) {
  stop("SAFETY STOP: Backup directory already exists.")
}

dir.create(output_backup, recursive = TRUE)

reports_backup <- file.path(output_backup, "reports")
figures_backup <- file.path(output_backup, "figures")

dir.create(reports_backup)
dir.create(figures_backup)

# Current files
reports_files <- list.files(
  "reports",
  full.names = TRUE,
  recursive = FALSE
)

figures_files <- list.files(
  "figures",
  full.names = TRUE,
  recursive = FALSE
)

# Do not accidentally copy the new backup directory itself
reports_files <- reports_files[
  basename(reports_files) != basename(output_backup)
]

# Copy reports
report_copy_ok <- TRUE

if (length(reports_files) > 0) {
  report_copy_ok <- all(
    file.copy(
      reports_files,
      reports_backup,
      overwrite = FALSE
    )
  )
}

# Copy figures
figure_copy_ok <- TRUE

if (length(figures_files) > 0) {
  figure_copy_ok <- all(
    file.copy(
      figures_files,
      figures_backup,
      overwrite = FALSE
    )
  )
}

cat("\nBackup directory:\n")
cat(output_backup, "\n")

cat("\nOriginal report files:", length(reports_files), "\n")
cat("Original figure files:", length(figures_files), "\n")

cat("\nReport backup successful:", report_copy_ok, "\n")
cat("Figure backup successful:", figure_copy_ok, "\n")

# Verify counts
reports_backed_up <- list.files(
  reports_backup,
  full.names = FALSE
)

figures_backed_up <- list.files(
  figures_backup,
  full.names = FALSE
)

cat("\nBacked-up report files:", length(reports_backed_up), "\n")
cat("Backed-up figure files:", length(figures_backed_up), "\n")

if (
  !report_copy_ok ||
  !figure_copy_ok ||
  length(reports_backed_up) != length(reports_files) ||
  length(figures_backed_up) != length(figures_files)
) {
  stop(
    "SAFETY STOP: Output backup verification failed."
  )
}

cat("\n============================================================\n")
cat("       OUTPUT BACKUP VERIFIED SUCCESSFULLY\n")
cat("============================================================\n")
cat("Reports protected:", length(reports_backed_up), "\n")
cat("Figures protected:", length(figures_backed_up), "\n")
cat("Total protected:", length(reports_backed_up) +
      length(figures_backed_up), "\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       STEP 8A — FINAL BACKUP CHECKPOINT\n")
cat("============================================================\n")

cat("Output backup directory:\n")
cat(output_backup, "\n\n")

cat("Backup directory exists:",
    dir.exists(output_backup), "\n")

cat("Reports protected:",
    length(list.files(reports_backup, full.names = FALSE)),
    "\n")

cat("Figures protected:",
    length(list.files(figures_backup, full.names = FALSE)),
    "\n")

cat("\nActive script lines:",
    length(readLines(main, warn = FALSE)),
    "\n")

cat("Active script MD5:",
    unname(tools::md5sum(main)),
    "\n")

if (
  !dir.exists(output_backup) ||
  length(list.files(reports_backup, full.names = FALSE)) != 24 ||
  length(list.files(figures_backup, full.names = FALSE)) != 5 ||
  length(readLines(main, warn = FALSE)) != 808 ||
  unname(tools::md5sum(main)) !=
  "52b6fe170f6de4b8b32b726c3f66cb2f"
) {
  stop("SAFETY STOP: Final checkpoint failed.")
}

cat("\n============================================================\n")
cat("       FINAL PRE-EXECUTION CHECKPOINT PASSED\n")
cat("============================================================\n")
cat("808-line script VERIFIED.\n")
cat("1033-line previous script PROTECTED.\n")
cat("24 reports PROTECTED.\n")
cat("5 figures PROTECTED.\n")
cat("29 existing outputs PROTECTED.\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       STEP 9 — EXECUTE VERIFIED 808-LINE ANALYSIS\n")
cat("============================================================\n")

if (
  length(readLines(main, warn = FALSE)) != 808 ||
  unname(tools::md5sum(main)) !=
  "52b6fe170f6de4b8b32b726c3f66cb2f"
) {
  stop(
    "SAFETY STOP: Active script is not the verified 808-line version."
  )
}

cat("Script verified: 808 lines\n")
cat("MD5 verified: 52b6fe170f6de4b8b32b726c3f66cb2f\n")
cat("Beginning analysis execution...\n\n")

source(main, echo = TRUE)

cat("\n============================================================\n")
cat("       STEP 9 — ANALYSIS EXECUTION FINISHED\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       STEP 10 — POST-EXECUTION VERIFICATION\n")
cat("============================================================\n")

# ------------------------------------------------------------
# 1. Verify active script identity
# ------------------------------------------------------------

active_lines <- length(readLines(main, warn = FALSE))
active_md5 <- unname(tools::md5sum(main))

cat("\n--- ACTIVE SCRIPT ---\n")
cat("Lines:", active_lines, "\n")
cat("MD5:", active_md5, "\n")

if (
  active_lines != 808 ||
  active_md5 != "52b6fe170f6de4b8b32b726c3f66cb2f"
) {
  stop("SAFETY STOP: Active script changed during execution.")
}

# ------------------------------------------------------------
# 2. Verify expected outputs
# ------------------------------------------------------------

expected_reports <- c(
  "bootstrap_validation_auc.csv",
  "calibration_plot_data.csv",
  "model_calibration_results.csv",
  "overfitting_bootstrap_assessment.csv",
  "table2_multivariable_logistic_regression_publication.csv",
  "table3_model_performance_publication.csv"
)

expected_figures <- c(
  "adjusted_odds_ratio_forest_plot.png",
  "calibration_plot.png",
  "length_of_stay_readmission.png",
  "previous_admissions_readmission.png",
  "roc_curve_30day_readmission.png"
)

missing_reports <- expected_reports[
  !file.exists(file.path("reports", expected_reports))
]

missing_figures <- expected_figures[
  !file.exists(file.path("figures", expected_figures))
]

cat("\n--- EXPECTED REPORTS ---\n")
cat("Expected:", length(expected_reports), "\n")
cat("Missing:", length(missing_reports), "\n")

if (length(missing_reports) > 0) {
  cat(paste(missing_reports, collapse = "\n"), "\n")
}

cat("\n--- EXPECTED FIGURES ---\n")
cat("Expected:", length(expected_figures), "\n")
cat("Missing:", length(missing_figures), "\n")

if (length(missing_figures) > 0) {
  cat(paste(missing_figures, collapse = "\n"), "\n")
}

# ------------------------------------------------------------
# 3. Current output counts
# ------------------------------------------------------------

current_reports <- list.files(
  "reports",
  recursive = FALSE,
  full.names = FALSE
)

current_figures <- list.files(
  "figures",
  recursive = FALSE,
  full.names = FALSE
)

cat("\n--- CURRENT OUTPUT COUNTS ---\n")
cat("Reports:", length(current_reports), "\n")
cat("Figures:", length(current_figures), "\n")

if (
  length(missing_reports) > 0 ||
  length(missing_figures) > 0
) {
  stop("POST-EXECUTION CHECK FAILED: Expected outputs are missing.")
}

cat("\n============================================================\n")
cat("       STEP 10 — POST-EXECUTION CHECK PASSED\n")
cat("============================================================\n")
cat("Active script remains VERIFIED.\n")
cat("Expected reports are present.\n")
cat("Expected figures are present.\n")
cat("Analysis completed without reported error.\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       STEP 11 — COMPARE PRE/POST EXECUTION OUTPUTS\n")
cat("============================================================\n")

backup_reports <- reports_backup
backup_figures <- figures_backup

compare_dir <- function(before_dir, after_dir) {
  
  before <- list.files(
    before_dir,
    full.names = FALSE
  )
  
  after <- list.files(
    after_dir,
    full.names = FALSE
  )
  
  all_files <- sort(unique(c(before, after)))
  
  result <- data.frame(
    file = all_files,
    before_exists = file.exists(
      file.path(before_dir, all_files)
    ),
    after_exists = file.exists(
      file.path(after_dir, all_files)
    ),
    before_md5 = NA_character_,
    after_md5 = NA_character_,
    stringsAsFactors = FALSE
  )
  
  for (i in seq_len(nrow(result))) {
    
    if (result$before_exists[i]) {
      result$before_md5[i] <- unname(
        tools::md5sum(
          file.path(before_dir, result$file[i])
        )
      )
    }
    
    if (result$after_exists[i]) {
      result$after_md5[i] <- unname(
        tools::md5sum(
          file.path(after_dir, result$file[i])
        )
      )
    }
  }
  
  result$status <- ifelse(
    !result$before_exists,
    "NEW",
    ifelse(
      !result$after_exists,
      "REMOVED",
      ifelse(
        result$before_md5 == result$after_md5,
        "UNCHANGED",
        "CHANGED"
      )
    )
  )
  
  result
}

report_comparison <- compare_dir(
  backup_reports,
  "reports"
)

figure_comparison <- compare_dir(
  backup_figures,
  "figures"
)

cat("\n--- REPORT COMPARISON ---\n")
print(
  report_comparison[
    report_comparison$status != "UNCHANGED",
  ],
  row.names = FALSE
)

cat("\n--- FIGURE COMPARISON ---\n")
print(
  figure_comparison[
    figure_comparison$status != "UNCHANGED",
  ],
  row.names = FALSE
)

cat("\n--- SUMMARY ---\n")

cat(
  "Reports unchanged:",
  sum(report_comparison$status == "UNCHANGED"),
  "\n"
)

cat(
  "Reports changed:",
  sum(report_comparison$status == "CHANGED"),
  "\n"
)

cat(
  "Reports new:",
  sum(report_comparison$status == "NEW"),
  "\n"
)

cat(
  "Reports removed:",
  sum(report_comparison$status == "REMOVED"),
  "\n"
)

cat(
  "Figures unchanged:",
  sum(figure_comparison$status == "UNCHANGED"),
  "\n"
)

cat(
  "Figures changed:",
  sum(figure_comparison$status == "CHANGED"),
  "\n"
)

cat(
  "Figures new:",
  sum(figure_comparison$status == "NEW"),
  "\n"
)

cat(
  "Figures removed:",
  sum(figure_comparison$status == "REMOVED"),
  "\n"
)

cat("\n============================================================\n")
cat("       STEP 11 COMPLETE — COMPARISON ONLY\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       STEP 11A — OUTPUT COMPARISON SUMMARY\n")
cat("============================================================\n")

cat("Reports unchanged:",
    sum(report_comparison$status == "UNCHANGED"), "\n")

cat("Reports changed:",
    sum(report_comparison$status == "CHANGED"), "\n")

cat("Reports new:",
    sum(report_comparison$status == "NEW"), "\n")

cat("Reports removed:",
    sum(report_comparison$status == "REMOVED"), "\n")

cat("Figures unchanged:",
    sum(figure_comparison$status == "UNCHANGED"), "\n")

cat("Figures changed:",
    sum(figure_comparison$status == "CHANGED"), "\n")

cat("Figures new:",
    sum(figure_comparison$status == "NEW"), "\n")

cat("Figures removed:",
    sum(figure_comparison$status == "REMOVED"), "\n")

cat("\n--- CHANGED REPORTS ---\n")
changed_reports <- report_comparison$file[
  report_comparison$status == "CHANGED"
]
if (length(changed_reports) == 0) {
  cat("NONE\n")
} else {
  cat(paste(changed_reports, collapse = "\n"), "\n")
}

cat("\n--- CHANGED FIGURES ---\n")
changed_figures <- figure_comparison$file[
  figure_comparison$status == "CHANGED"
]
if (length(changed_figures) == 0) {
  cat("NONE\n")
} else {
  cat(paste(changed_figures, collapse = "\n"), "\n")
}

cat("\n============================================================\n")
cat("       STEP 11A COMPLETE\n")
cat("============================================================\n")
cat("\n============================================================\n")
cat("       STEP 11B — REPORT COMPARISON\n")
cat("============================================================\n")

cat("Reports unchanged:",
    sum(report_comparison$status == "UNCHANGED"), "\n")

cat("Reports changed:",
    sum(report_comparison$status == "CHANGED"), "\n")

cat("Reports new:",
    sum(report_comparison$status == "NEW"), "\n")

cat("Reports removed:",
    sum(report_comparison$status == "REMOVED"), "\n")

cat("\nCHANGED REPORT FILES:\n")

changed_reports <- report_comparison$file[
  report_comparison$status == "CHANGED"
]

if (length(changed_reports) == 0) {
  cat("NONE\n")
} else {
  cat(paste(changed_reports, collapse = "\n"), "\n")
}

cat("\n============================================================\n")
cat("       STEP 11B COMPLETE\n")
cat("============================================================\n")
