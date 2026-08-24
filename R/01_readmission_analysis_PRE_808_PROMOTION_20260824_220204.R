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
# ============================================================
getwd()
# Current R folder
current_R <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

# Current project root = one folder above R/
current_project <- dirname(current_R)

# Desktop backup location
desktop <- file.path(path.expand("~"), "Desktop")
backup_name <- paste0(
  "MedResearch-01_BACKUP_",
  format(Sys.time(), "%Y%m%d_%H%M%S")
)
backup_project <- file.path(desktop, backup_name)

# Copy the complete project
dir.create(backup_project, recursive = TRUE, showWarnings = FALSE)

files_to_copy <- list.files(
  current_project,
  all.files = TRUE,
  full.names = TRUE,
  recursive = TRUE,
  include.dirs = TRUE
)

# Copy files while preserving the folder structure
for (f in files_to_copy) {
  relative <- substring(
    f,
    nchar(current_project) + 2
  )
  
  destination <- file.path(backup_project, relative)
  
  if (dir.exists(f)) {
    dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  } else {
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    file.copy(f, destination, overwrite = TRUE)
  }
}

cat("\nBACKUP COMPLETE\n")
cat("Current project:\n", current_project, "\n\n")
cat("Desktop backup:\n", backup_project, "\n")
list.files(
  backup_project,
  recursive = TRUE,
  full.names = FALSE
)
list.files(
  current_project,
  all.files = TRUE,
  recursive = FALSE,
  full.names = FALSE
)
list.files(
  current_R,
  all.files = TRUE,
  full.names = FALSE
)
candidates <- c(
  "C:/Users/Windows/Desktop",
  "C:/Users/Windows/OneDrive/Desktop",
  "C:/Users/Windows/Documents",
  "C:/Users/Windows/OneDrive/Documents",
  "C:/Users/Windows/Downloads"
)

for (p in candidates) {
  if (dir.exists(p)) {
    cat("\n========== ", p, " ==========\n", sep = "")
    print(list.files(p, full.names = FALSE))
  }
}
real_project <- "C:/Users/Windows/OneDrive/Documents/Desktop/MedResearch"

cat("PROJECT EXISTS:", dir.exists(real_project), "\n\n")

cat("TOP-LEVEL CONTENTS:\n")
print(list.files(
  real_project,
  all.files = TRUE,
  recursive = FALSE,
  full.names = FALSE
))
cat("\nALL PROJECT FILES:\n")
print(list.files(
  real_project,
  all.files = TRUE,
  recursive = TRUE,
  full.names = FALSE
))
roots <- c(
  "C:/Users/Windows/OneDrive/Documents/Desktop",
  "C:/Users/Windows/OneDrive/Desktop",
  "C:/Users/Windows/Desktop",
  "C:/Users/Windows/Downloads"
)

for (root in roots) {
  if (dir.exists(root)) {
    hits <- list.files(
      root,
      pattern = "^MedResearch$",
      recursive = FALSE,
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    if (length(hits) > 0) {
      cat("\nFOUND:\n")
      print(hits)
    }
  }
}
desktop_path <- "C:/Users/Windows/OneDrive/Documents/Desktop"

info <- file.info(
  file.path(desktop_path, "MedResearch")
)

print(info)
roots <- c(
  "C:/Users/Windows/OneDrive/Documents/Desktop",
  "C:/Users/Windows/OneDrive/Desktop",
  "C:/Users/Windows/Desktop",
  "C:/Users/Windows/Downloads"
)

for (root in roots) {
  if (dir.exists(root)) {
    hits <- list.files(
      root,
      pattern = "^MedResearch$",
      recursive = FALSE,
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    if (length(hits) > 0) {
      cat("\nFOUND:\n")
      print(hits)
    }
  }
}
desktop_path <- "C:/Users/Windows/OneDrive/Documents/Desktop"

info <- file.info(
  file.path(desktop_path, "MedResearch")
)

print(info)

list.files(
  "C:/Users/Windows/OneDrive/Documents/Desktop",
  full.names = TRUE
)
list.files("C:/Users/Windows/OneDrive/Documents/Desktop", full.names = TRUE)
file.exists("C:/Users/Windows/OneDrive/Documents/Desktop/MedResearch")
1 + 1
list.files("C:/Users/Windows/OneDrive/Documents/Desktop", full.names = TRUE)
 1 + 1
 list.files(
   "C:/Users/Windows/OneDrive/Documents/Desktop",
   full.names = TRUE
 )
 list.files(
   c(
     "C:/Users/Windows/OneDrive/Documents/Desktop",
     "C:/Users/Windows/OneDrive/Desktop",
     "C:/Users/Windows/Desktop",
     "C:/Users/Windows/Downloads"
   ),
   full.names = TRUE
 )
 real_project <- "C:/Users/Windows/OneDrive/Desktop/MedResearch"
 
 list.files(
   real_project,
   all.files = TRUE,
   recursive = FALSE,
   full.names = FALSE
 )
 project <- "C:/Users/Windows/OneDrive/Desktop/MedResearch/MedResearch-01"
 
 list.files(
   project,
   all.files = TRUE,
   recursive = FALSE,
   full.names = FALSE
 )
 backup_project <- "C:/Users/Windows/OneDrive/Documents/Desktop/MedResearch-01_REAL_BACKUP"
 
 if (dir.exists(backup_project)) {
   unlink(backup_project, recursive = TRUE, force = TRUE)
 }
 
 ok <- file.copy(
   project,
   backup_project,
   recursive = TRUE
 )
 
 cat("Backup result:", ok, "\n")
 cat("Backup location:", backup_project, "\n")
 backup_project <- "C:/Users/Windows/OneDrive/Documents/Desktop/MedResearch-01_REAL_BACKUP"
 
 if (dir.exists(backup_project)) {
   unlink(backup_project, recursive = TRUE, force = TRUE)
 }
 
 dir.create(backup_project, recursive = TRUE)
 
 items <- list.files(
   project,
   all.files = TRUE,
   full.names = TRUE,
   no.. = TRUE
 )
 
 for (item in items) {
   file.copy(
     item,
     backup_project,
     recursive = TRUE,
     copy.date = TRUE
   )
 }
 
 cat("BACKUP CREATED\n")
 cat("Location:", backup_project, "\n")
 cat("Items copied:", length(items), "\n")
 list.files(
   backup_project,
   all.files = TRUE,
   recursive = FALSE,
   full.names = FALSE
 )
 zip_file <- "C:/Users/Windows/Downloads/MedResearch-01-f9e9cbcb47a505c334d7bcebfbe19ffd1127b623.zip"
 
 recovery_root <- "C:/Users/Windows/OneDrive/Documents/Desktop/MedResearch_GITHUB_RECOVERY"
 
 if (dir.exists(recovery_root)) {
   unlink(recovery_root, recursive = TRUE, force = TRUE)
 }
 
 dir.create(recovery_root, recursive = TRUE)
 
 unzip(
   zip_file,
   exdir = recovery_root
 )
 
 cat("GITHUB RECOVERY COPY EXTRACTED\n")
 list.files(
   recovery_root,
   all.files = TRUE,
   recursive = FALSE,
   full.names = FALSE
 )
 github_project <- file.path(
   recovery_root,
   "MedResearch-01-f9e9cbcb47a505c334d7bcebfbe19ffd1127b623"
 )
 
 list.files(
   github_project,
   all.files = TRUE,
   recursive = FALSE,
   full.names = FALSE
 )
 current_files <- list.files(
   project,
   recursive = TRUE,
   all.files = TRUE,
   full.names = FALSE
 )
 
 github_files <- list.files(
   github_project,
   recursive = TRUE,
   all.files = TRUE,
   full.names = FALSE
 )
 
 cat("===== FILES IN CURRENT PROJECT BUT NOT GITHUB =====\n")
 print(setdiff(current_files, github_files))
 
 cat("\n===== FILES IN GITHUB BUT NOT CURRENT PROJECT =====\n")
 print(setdiff(github_files, current_files))
 compare_files <- function(folder1, folder2) {
   f1 <- list.files(
     folder1,
     recursive = TRUE,
     all.files = FALSE,
     full.names = FALSE
   )
   
   f2 <- list.files(
     folder2,
     recursive = TRUE,
     all.files = FALSE,
     full.names = FALSE
   )
   
   common <- intersect(f1, f2)
   
   # Ignore Git metadata and R history files
   common <- common[
     !grepl(
       "(^\\.git/|^\\.Rhistory$|/\\.Rhistory$|^\\.RData$)",
       common
     )
   ]
   
   result <- data.frame(
     file = common,
     current_size = NA_real_,
     github_size = NA_real_,
     current_hash = NA_character_,
     github_hash = NA_character_,
     stringsAsFactors = FALSE
   )
   
   for (i in seq_along(common)) {
     cf <- file.path(folder1, common[i])
     gf <- file.path(folder2, common[i])
     
     result$current_size[i] <- file.info(cf)$size
     result$github_size[i] <- file.info(gf)$size
     
     result$current_hash[i] <- tools::md5sum(cf)
     result$github_hash[i] <- tools::md5sum(gf)
   }
   
   result$same <- (
     result$current_hash == result$github_hash
   )
   
   result
 }
 
 comparison <- compare_files(
   project,
   github_project
 )
 
 cat("===== FILES WITH DIFFERENT CONTENT =====\n")
 print(comparison[!comparison$same, ])
 
 cat("\n===== NUMBER OF COMMON FILES =====\n")
 cat(nrow(comparison), "\n")
 
 cat("\n===== NUMBER OF DIFFERENT FILES =====\n")
 cat(sum(!comparison$same), "\n")
 cat("Location:", recovery_root, "\n")
 cat("========== CURRENT MedResearch-01.R ==========\n")
 cat(readLines(
   file.path(project, "MedResearch-01.R"),
   warn = FALSE
 ), sep = "\n")
 
 cat("\n\n========== GITHUB MedResearch-01.R ==========\n")
 cat(readLines(
   file.path(github_project, "MedResearch-01.R"),
   warn = FALSE
 ), sep = "\n")
 
 cat("\n\n========== LINE COUNTS ==========\n")
 
 cat(
   "Current MedResearch-01.R:",
   length(readLines(file.path(project, "MedResearch-01.R"), warn = FALSE)),
   "\n"
 )
 current_file <- file.path(project, "R/01_readmission_analysis.R")
 github_file  <- file.path(github_project, "R/01_readmission_analysis.R")
 
 current_text <- readLines(current_file, warn = FALSE)
 github_text  <- readLines(github_file, warn = FALSE)
 
 cat("Current lines:", length(current_text), "\n")
 cat("GitHub lines:", length(github_text), "\n")
 
 cat(
   "Files identical:",
   identical(current_text, github_text),
   "\n"
 )
 
 cat(
   "GitHub MedResearch-01.R:",
   length(readLines(file.path(github_project, "MedResearch-01.R"), warn = FALSE)),
   "\n"
 )
 
 cat(
   "Current R/01_readmission_analysis.R:",
   length(readLines(
     file.path(project, "R/01_readmission_analysis.R"),
     warn = FALSE
   )),
   "\n"
 )
 
 cat(
   "GitHub R/01_readmission_analysis.R:",
   length(readLines(
     file.path(github_project, "R/01_readmission_analysis.R"),
     warn = FALSE
   )),
   "\n"
 )
 cat("\n========== ALL R FILES IN CURRENT PROJECT ==========\n")
 
 r_files <- list.files(
   project,
   pattern = "\\.R$",
   recursive = TRUE,
   full.names = TRUE
 )
 
 for (f in r_files) {
   cat(
     length(readLines(f, warn = FALSE)),
     "lines  |  ",
     normalizePath(f, winslash = "/", mustWork = FALSE),
     "\n"
   )
 }
 cat("\n========== FILE IDENTITY CHECK ==========\n")
 
 main_file <- file.path(project, "MedResearch-01.R")
 analysis_file <- file.path(project, "R/01_readmission_analysis.R")
 backup_file <- file.path(project, "R/01_readmission_analysis_RECOVERY_BACKUP.R")
 
 main_text <- readLines(main_file, warn = FALSE)
 analysis_text <- readLines(analysis_file, warn = FALSE)
 backup_text <- readLines(backup_file, warn = FALSE)
 
 cat(
   "MedResearch-01.R vs 01_readmission_analysis.R:",
   identical(main_text, analysis_text),
   "\n"
 )
 
 cat(
   "01_readmission_analysis.R vs RECOVERY_BACKUP:",
   identical(analysis_text, backup_text),
   "\n"
 )
 
 cat(
   "MedResearch-01.R vs RECOVERY_BACKUP:",
   identical(main_text, backup_text),
   "\n"
 )
 cat("\n========== CURRENT MedResearch-01.R ==========\n\n")
 
 cat(
   readLines(
     file.path(project, "MedResearch-01.R"),
     warn = FALSE
   ),
   sep = "\n"
 )
 cat("\n========== HISTORICAL LINE COUNTS ==========\n")
 
 commits <- c(
   "0ee837c",
   "8a40328",
   "38ec944",
   "b10772e",
   "f9e9cbc"
 )
 
 for (commit in commits) {
   txt <- tryCatch(
     system(
       paste(
         "git show",
         commit,
         ":R/01_readmission_analysis.R"
       ),
       intern = TRUE
     ),
     error = function(e) character()
   )
   
   cat(
     commit,
     ":",
     length(txt),
     "lines\n"
   )
 }
 system("git status --short")
 system("git log --oneline --all -- R/01_readmission_analysis.R")
 system("git show 0ee837c:R/01_readmission_analysis.R") 
 txt <- system(
   "git show 0ee837c:R/01_readmission_analysis.R",
   intern = TRUE
 )
 
 cat("Lines:", length(txt), "\n") 
 getwd()
 project 
 setwd(project) 
 cat("Working directory:\n")
 getwd()
 
 cat("\nGit status:\n")
 system("git status --short")
 
 cat("\nGit repository check:\n")
 system("git rev-parse --show-toplevel") 
 
 cat("\n========== GIT HISTORY ==========\n")
 
 system("git log --oneline --all --decorate -- R/01_readmission_analysis.R")
 cat("\n========== HISTORICAL FILE TEST ==========\n")
 
 txt <- system(
   "git show 0ee837c:R/01_readmission_analysis.R",
   intern = TRUE
 )
 
 cat("0ee837c lines:", length(txt), "\n")
 cat("\n========== ALL HISTORICAL VERSIONS ==========\n")
 
 commits <- c(
   "0ee837c",
   "8a40328",
   "b10772e",
   "f9e9cbc"
 )
 
 for (commit in commits) {
   
   txt <- system(
     paste0(
       "git show ",
       commit,
       ":R/01_readmission_analysis.R"
     ),
     intern = TRUE
   )
   
   cat(
     commit,
     "=>",
     length(txt),
     "lines\n"
   )
 }
 cat("\n========== FILES IN MILESTONE 5 ==========\n")
 
 system("git ls-tree -r --name-only f9e9cbc")
 cat("\n========== MedResearch-01.R HISTORY ==========\n")
 
 system("git log --oneline --all -- MedResearch-01.R")
 cat("\n========== MedResearch-01.R AT MILESTONE 5 ==========\n")
 
 txt <- system(
   "git show f9e9cbc:MedResearch-01.R",
   intern = TRUE
 )
 
 cat("Lines:", length(txt), "\n")
 cat(txt, sep = "\n")
 cat("\n========== FILE CHANGE HISTORY ==========\n")
 
 system(
   "git log --stat --oneline --all -- R/01_readmission_analysis.R"
 )
 cat("\n========== UNREACHABLE GIT OBJECTS ==========\n")
 
 system("git fsck --full --no-reflogs --unreachable")
 cat("\n========== TEMPORARY MEDRESEARCH COPIES ==========\n")
 
 temp_root <- tempdir()
 
 temp_files <- list.files(
   temp_root,
   recursive = TRUE,
   full.names = TRUE,
   all.files = TRUE
 )
 
 temp_matches <- temp_files[
   grepl(
     "MedResearch|01_readmission|readmission",
     basename(temp_files),
     ignore.case = TRUE
   )
 ]
 
 if (length(temp_matches) == 0) {
   cat("No matching files found in current R temp directory.\n")
 } else {
   info <- file.info(temp_matches)
   
   print(
     data.frame(
       file = temp_matches,
       size = info$size,
       modified = info$mtime,
       row.names = NULL
     )
   )
 }
 cat("\n========== WINDOWS TEMP MEDRESEARCH SEARCH ==========\n")
 
 temp_root_windows <- file.path(
   Sys.getenv("LOCALAPPDATA"),
   "Temp"
 )
 
 temp_files_windows <- list.files(
   temp_root_windows,
   recursive = TRUE,
   full.names = TRUE,
   all.files = TRUE
 )
 
 matches_windows <- temp_files_windows[
   grepl(
     "MedResearch|readmission|01_readmission",
     basename(temp_files_windows),
     ignore.case = TRUE
   )
 ]
 
 if (length(matches_windows) == 0) {
   cat("No matching files found.\n")
 } else {
   
   info <- file.info(matches_windows)
   
   result <- data.frame(
     file = matches_windows,
     size = info$size,
     modified = info$mtime,
     row.names = NULL
   )
   
   result <- result[
     order(result$size, decreasing = TRUE),
   ]
   
   print(result)
 }
 cat("\n========== FULL MEDRESEARCH DIRECTORY SEARCH ==========\n")
 
 search_root <- "C:/Users/Windows/OneDrive/Desktop/MedResearch"
 
 all_files <- list.files(
   search_root,
   recursive = TRUE,
   full.names = TRUE,
   all.files = TRUE
 )
 
 all_files <- all_files[
   !file.info(all_files)$isdir
 ]
 
 matches <- all_files[
   grepl(
     "MedResearch|readmission|research|analysis",
     basename(all_files),
     ignore.case = TRUE
   )
 ]
 
 if (length(matches) == 0) {
   
   cat("No matching files found.\n")
   
 } else {
   
   info <- file.info(matches)
   
   result <- data.frame(
     file = matches,
     size = info$size,
     modified = info$mtime,
     row.names = NULL
   )
   
   result <- result[
     order(result$size, decreasing = TRUE),
   ]
   
   print(result)
 }
 safe_backup <- file.path(
   dirname(project),
   paste0(
     "MedResearch_01_readmission_analysis_SAFE_",
     format(Sys.time(), "%Y%m%d_%H%M%S"),
     ".R"
   )
 )
 
 file.copy(
   file.path(project, "R/01_readmission_analysis.R"),
   safe_backup,
   overwrite = FALSE
 )
 
 cat("SAFE BACKUP CREATED:\n")
 cat(safe_backup, "\n")
 cat("\n========== RSTUDIO HISTORY FILES ==========\n")
 
 rstudio_root <- file.path(
   Sys.getenv("APPDATA"),
   "RStudio"
 )
 
 if (!dir.exists(rstudio_root)) {
   
   cat("RStudio AppData directory not found:\n")
   cat(rstudio_root, "\n")
   
 } else {
   
   rstudio_files <- list.files(
     rstudio_root,
     recursive = TRUE,
     full.names = TRUE,
     all.files = TRUE
   )
   
   rstudio_files <- rstudio_files[
     !file.info(rstudio_files)$isdir
   ]
   
   info <- file.info(rstudio_files)
   
   result <- data.frame(
     file = rstudio_files,
     size = info$size,
     modified = info$mtime,
     row.names = NULL
   )
   
   result <- result[
     order(result$size, decreasing = TRUE),
   ]
   
   print(head(result, 50))
 }
 cat("APPDATA =", Sys.getenv("APPDATA"), "\n")
 cat("LOCALAPPDATA =", Sys.getenv("LOCALAPPDATA"), "\n")
 cat("RStudio AppData exists =", dir.exists(
   file.path(Sys.getenv("APPDATA"), "RStudio")
 ), "\n")
 cat("\n========== RSTUDIO DIRECTORIES ==========\n")
 
 appdata <- Sys.getenv("APPDATA")
 
 dirs <- list.dirs(
   appdata,
   recursive = TRUE,
   full.names = TRUE
 )
 
 print(
   dirs[
     grepl(
       "RStudio|history|rstudio",
       dirs,
       ignore.case = TRUE
     )
   ]
 )
 cat("\n========== RSTUDIO HISTORY ENVIRONMENT ==========\n")
 
 print(Sys.getenv("RSTUDIO"))
 print(Sys.getenv("RSTUDIO_CONFIG_HOME"))
 print(Sys.getenv("RSTUDIO_DATA_HOME"))
 cat("\n========== RSTUDIO RECOVERY FILE SEARCH ==========\n")
 
 search_roots <- c(
   file.path(Sys.getenv("APPDATA"), "RStudio"),
   file.path(Sys.getenv("LOCALAPPDATA"), "RStudio"),
   file.path(Sys.getenv("USERPROFILE"), "Documents")
 )
 
 for (root in search_roots) {
   
   cat("\nROOT:", root, "\n")
   
   if (!dir.exists(root)) {
     cat("Not found\n")
     next
   }
   
   files <- list.files(
     root,
     recursive = TRUE,
     full.names = TRUE,
     all.files = TRUE
   )
   
   files <- files[
     !file.info(files)$isdir
   ]
   
   matches <- files[
     grepl(
       "\\.(Rhistory|RData|Rproj|R|txt|log|json|db)$",
       files,
       ignore.case = TRUE
     ) |
       grepl(
         "history|source|session|console",
         basename(files),
         ignore.case = TRUE
       )
   ]
   
   if (length(matches) > 0) {
     
     info <- file.info(matches)
     
     result <- data.frame(
       file = matches,
       size = info$size,
       modified = info$mtime,
       row.names = NULL
     )
     
     result <- result[
       order(result$size, decreasing = TRUE),
     ]
     
     print(head(result, 100))
     
   } else {
     cat("No likely recovery files found\n")
   }
 }
 cat("\n========== RSTUDIO RECOVERY FILE SEARCH ==========\n")

search_roots <- c(
  file.path(Sys.getenv("APPDATA"), "RStudio"),
  file.path(Sys.getenv("LOCALAPPDATA"), "RStudio"),
  file.path(Sys.getenv("USERPROFILE"), "Documents")
)

for (root in search_roots) {

  cat("\nROOT:", root, "\n")

  if (!dir.exists(root)) {
    cat("Not found\n")
    next
  }

  files <- list.files(
    root,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE
  )

  files <- files[
    !file.info(files)$isdir
  ]

  matches <- files[
    grepl(
      "\\.(Rhistory|RData|Rproj|R|txt|log|json|db)$",
      files,
      ignore.case = TRUE
    ) |
    grepl(
      "history|source|session|console",
      basename(files),
      ignore.case = TRUE
    )
  ]

  if (length(matches) > 0) {

    info <- file.info(matches)

    result <- data.frame(
      file = matches,
      size = info$size,
      modified = info$mtime,
      row.names = NULL
    )

    result <- result[
      order(result$size, decreasing = TRUE),
    ]

    print(head(result, 100))

  } else {
    cat("No likely recovery files found\n")
  }
}
cat("\n========== PROJECT HIDDEN RECOVERY FILES ==========\n")

project_files <- list.files(
  project,
  recursive = TRUE,
  full.names = TRUE,
  all.files = TRUE
)

project_hidden <- project_files[
  grepl(
    "Rhistory|RData|history|autosave|backup|recovery|session",
    basename(project_files),
    ignore.case = TRUE
  )
]

if (length(project_hidden) == 0) {
  cat("No matching hidden/recovery files found.\n")
} else {
  
  info <- file.info(project_hidden)
  
  print(
    data.frame(
      file = project_hidden,
      size = info$size,
      modified = info$mtime,
      row.names = NULL
    )
  )
}

cat("\n========== SAFE .RData INSPECTION ==========\n")

recovery_env <- new.env(parent = emptyenv())

loaded_objects <- load(
  file.path(project, ".RData"),
  envir = recovery_env
)

cat("\nObjects found:", length(loaded_objects), "\n")
print(loaded_objects)
cat("\n========== OBJECT SIZES ==========\n")

for (nm in loaded_objects) {
  obj <- recovery_env[[nm]]
  
  cat(
    nm,
    "| class =", paste(class(obj), collapse = ","),
    "| size =", format(object.size(obj), units = "auto"),
    "\n"
  )
}
cat("\n========== .RData HISTORICAL CODE LENGTHS ==========\n")

for (nm in c("v1", "v2", "v3", "v4", "v5", "git_file", "diff_lines")) {
  
  obj <- recovery_env[[nm]]
  
  cat(
    nm,
    "| class =", paste(class(obj), collapse = ","),
    "| length =", length(obj),
    "\n"
  )
}
cat("\n========== HISTORICAL CODE HEADINGS ==========\n")

for (nm in c("v1", "v2", "v3", "v4", "v5")) {
  
  obj <- recovery_env[[nm]]
  
  cat("\n\n-----", nm, "-----\n")
  
  headings <- grep(
    "^#|^##|^###",
    obj,
    value = TRUE
  )
  
  print(headings)
}
cat("\n========== DIFF_LINES INSPECTION ==========\n")

cat(
  recovery_env[["diff_lines"]],
  sep = "\n"
)
cat("\n========== DIFF_LINES SUMMARY ==========\n")

cat("Length:", length(recovery_env[["diff_lines"]]), "\n")

cat(
  "First 30 lines:\n\n",
  paste(
    head(recovery_env[["diff_lines"]], 30),
    collapse = "\n"
  ),
  "\n"
)
cat("\n========== DIFF KEYWORDS ==========\n")

diff <- recovery_env[["diff_lines"]]

keywords <- c(
  "2200",
  "240",
  "809",
  "missing",
  "recovery",
  "restore",
  "overfitting",
  "bootstrap",
  "univariable",
  "Table 1"
)

for (k in keywords) {
  
  hits <- grep(
    k,
    diff,
    ignore.case = TRUE,
    value = TRUE,
    fixed = TRUE
  )
  
  if (length(hits) > 0) {
    cat("\n---", k, "---\n")
    print(hits)
  }
}

cat("\n========== SEARCH RSTUDIO FILE CONTENT ==========\n")

rstudio_root <- file.path(
  Sys.getenv("APPDATA"),
  "RStudio"
)

rstudio_files <- list.files(
  rstudio_root,
  recursive = TRUE,
  full.names = TRUE,
  all.files = TRUE
)

rstudio_files <- rstudio_files[
  !file.info(rstudio_files)$isdir
]

found <- character()

for (f in rstudio_files) {
  
  txt <- tryCatch(
    readLines(
      f,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    error = function(e) character()
  )
  
  if (
    length(txt) > 0 &&
    any(
      grepl(
        "Predicting 30-Day Hospital Readmission",
        txt,
        fixed = TRUE
      )
    )
  ) {
    found <- c(found, f)
  }
}

cat("\nMATCHING FILES:\n")

if (length(found) == 0) {
  cat("No matching RStudio files found.\n")
} else {
  print(found)
}
cat("\n========== PROJECT RHISTORY ==========\n")

history_file <- file.path(
  project,
  ".Rhistory"
)

history_lines <- readLines(
  history_file,
  warn = FALSE
)

cat("History lines:", length(history_lines), "\n\n")

cat(
  paste(
    tail(history_lines, 80),
    collapse = "\n"
  )
)
setwd(project)
getwd()
analysis_file <- "R/01_readmission_analysis.R"

cat("File:", normalizePath(analysis_file), "\n")
cat("Lines:", length(readLines(analysis_file, warn = FALSE)), "\n")
cat("Size:", file.info(analysis_file)$size, "bytes\n")
rm(list = setdiff(ls(), c("project", "analysis_file")))

source(
  analysis_file,
  echo = TRUE
)
cat("\n========== GENERATED REPORTS ==========\n")

report_files <- list.files(
  "reports",
  full.names = FALSE
)

print(report_files)

cat(
  "\nReport count:",
  length(report_files),
  "\n"
)

cat("\n========== GENERATED FIGURES ==========\n")

figure_files <- list.files(
  "figures",
  full.names = FALSE
)

print(figure_files)

cat(
  "\nFigure count:",
  length(figure_files),
  "\n"
)
cat("\n========== CURRENT GIT STATUS ==========\n")

system("git status --short")
cat("\n========== TRACKED FILE DIFFERENCES ==========\n")

system("git diff --stat")

cat("\n========== MedResearch-01.R DIFFERENCE ==========\n")
system("git diff -- MedResearch-01.R")

cat("\n========== CHECKLIST DIFFERENCE ==========\n")
system("git diff -- docs/RESEARCH_MASTER_CHECKLIST.md")
system("git restore -- MedResearch-01.R")

cat("\n========== AFTER RESTORING MAIN FILE ==========\n")

cat(
  "MedResearch-01.R lines:",
  length(readLines("MedResearch-01.R", warn = FALSE)),
  "\n"
)

cat(
  "Analysis lines:",
  length(readLines("R/01_readmission_analysis.R", warn = FALSE)),
  "\n"
)
system("git status --short")
system("git add docs/RESEARCH_MASTER_CHECKLIST.md R/01_readmission_analysis_RECOVERY_BACKUP.R figures/calibration_plot.png reports/calibration_plot_data.csv reports/model_calibration_results.csv")

system("git commit -m \"Validate Milestone 5 analysis and add calibration outputs\"")
system("git status --short")
system("git rm R/01_readmission_analysis_RECOVERY_BACKUP.R && git commit -m \"Remove temporary recovery backup\"")
system("git rm R/01_readmission_analysis_RECOVERY_BACKUP.R")
system("git commit -m \"Remove temporary recovery backup\"")
system("git status --short")
safe_backup <- "C:/Users/Windows/OneDrive/Desktop/MedResearch/MedResearch_01_readmission_analysis_SAFE_20260824_113547.R"

if (file.exists(safe_backup)) {
  file.remove(safe_backup)
  cat("Safety backup deleted successfully.\n")
} else {
  cat("Safety backup was not found.\n")
}
file.exists(safe_backup)
cat("\n========== MILESTONE 5 RESEARCH OUTPUT SUMMARY ==========\n")

cat("\n--- Reports ---\n")
reports <- list.files("reports", pattern = "\\.csv$", full.names = TRUE)
print(basename(reports))

cat("\nReport count:", length(reports), "\n")

cat("\n--- Figures ---\n")
figures <- list.files("figures", pattern = "\\.(png|jpg|jpeg)$",
                      full.names = TRUE)
print(basename(figures))

cat("\nFigure count:", length(figures), "\n")

cat("\n--- Research script ---\n")
cat(
  "Lines:",
  length(readLines("R/01_readmission_analysis.R", warn = FALSE)),
  "\n"
)

cat(
  "Size:",
  file.info("R/01_readmission_analysis.R")$size,
  "bytes\n"
)

cat("\n--- Git status ---\n")
system("git status --short")
cat("\n========== MILESTONE 5 RESEARCH OUTPUT SUMMARY ==========")
