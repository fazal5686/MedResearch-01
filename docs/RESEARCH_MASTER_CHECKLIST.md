# MedResearch-01 — Research Master Checklist

## Completed
- [x] Synthetic hospital dataset created
- [x] Logistic regression completed
- [x] Adjusted odds ratios calculated
- [x] 95% confidence intervals calculated
- [x] Regression results exported
- [x] Forest plot created
- [x] ROC curve created
- [x] AUC calculated: 0.7534
- [x] Predicted readmission risk calculated
- [x] Git repository created
- [x] Research Milestone 1 committed
- [x] GitHub repository created
- [x] Project pushed to GitHub

## Next research steps
- [ ] Sensitivity and specificity analysis
- [ ] Probability threshold analysis
- [ ] Model calibration
- [ ] Calibration plot
- [ ] Model specification checks
- [x] Multicollinearity assessment — VIF 1.009–1.059; no problematic multicollinearity
- [x] Influential observation assessment — 19 observations screened using Cook's distance and DFFITS
- [x] Overfitting assessment — 6 predictors with 54.33 events per predictor; AIC 564.01; BIC 593.51; pseudo-R2 0.149
- [x] Internal validation — 200-bootstrap optimism correction; apparent AUC 0.7534 and optimism-corrected AUC 0.7418
- [x] Bootstrap validation — 200 bootstrap repetitions; mean AUC 0.7618; 95% CI 0.7182–0.7993
- [x] Univariable analysis — age, hypertension, previous admissions, length of stay, and emergency admission were significant; diabetes was not significant
- [ ] Subgroup analysis
- [x] Sensitivity analysis — exclusion of screened observations retained direction and significance of associations; hypertension showed the largest OR change
- [x] Table 1 — Patient characteristics — consolidated overall and readmission-stratified demographics, clinical characteristics, and P values
- [ ] Table 2 — Univariable analysis
- [ ] Table 3 — Multivariable regression
- [ ] Final publication-quality figures
- [ ] Methods section
- [ ] Results section
- [ ] Discussion
- [ ] Limitations
- [ ] Conclusion
- [ ] Final reproducibility check
- [ ] Final GitHub release
