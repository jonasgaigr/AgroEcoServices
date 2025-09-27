# ----------------------
# A) Infiltration (log-transformed)
# ----------------------
# Check distribution
hist(data_raw$infiltration_adjusted, breaks = 30)
hist(log1p(data_raw$infiltration_adjusted), breaks = 30)

m_infil <- lme4::lmer(
  log1p(infiltration_adjusted) ~ sample_place + (1 | site_id),
  data = data_raw
)

m_infil_null <- lme4::lmer(
  log1p(infiltration_adjusted) ~ 1 + (1 | site_id),
  data = data_raw
)

# Model comparison
anova(m_infil, m_infil_null)
AIC(m_infil, m_infil_null)

# Residual checks
plot(m_infil)                      # residuals vs fitted
qqnorm(resid(m_infil))
qqline(resid(m_infil))             # normal Q-Q

# 2. Tidy results
tab_infil <- broom.mixed::tidy(m_infil, effects = "fixed", conf.int = TRUE)

# 3. Make a nice regression table
write_tab_infil <- 
  sjPlot::tab_model(
  m_infil, m_infil_null,
  show.icc = TRUE,
  show.aic = TRUE,
  show.ci = 0.95,
  dv.labels = c("log(1+Infiltration) ~ Habitat", "Null model"),
  file = "Outputs/Tables/Infiltration_models.doc"
)

write_tab_infil

# 4. Optional: APA-style summary
library(report)
report_infil <- report::report(m_infil)

# Save the textual report
cat(report_infil, file = "Outputs/Tables/Infiltration_report.txt")


# ----------------------
# B) Water field capacity (WFC)
# ----------------------

# 1. Fit models
m_wfc <- lme4::lmer(
  WFC_adjusted ~ sample_place * depth_cm + (1 | site_id),
  data = data_raw
)

m_wfc_null <- lme4::lmer(
  WFC_adjusted ~ 1 + (1 | site_id),
  data = data_raw
)

# 2. Model comparison
anova(m_wfc, m_wfc_null)
AIC(m_wfc, m_wfc_null)

# 3. Diagnostics
plot(m_wfc)                  # residuals vs fitted
qqnorm(resid(m_wfc))         # Q-Q plot
qqline(resid(m_wfc))

# 4. Tidy results
tab_wfc <- broom.mixed::tidy(m_wfc, effects = "fixed", conf.int = TRUE)

# 5. Make regression tables and export
write_tab_wfc <- sjPlot::tab_model(
  m_wfc, m_wfc_null,
  show.icc = TRUE,
  show.aic = TRUE,
  show.ci = 0.95,
  dv.labels = c("WFC_adjusted ~ predictors", "Null model"),
  file = "Outputs/Tables/WFC_models.doc"
)

write_tab_wfc

# 6. Narrative APA-style summary
report_wfc <- report::report(m_wfc)

# Save the textual report
cat(report_wfc, file = "Outputs/Tables/WFC_report.txt")

# C) AWS
m_aws <- glmer.nb(AWS ~ sample_place * depth_cm +
                    SOC + WFC_adjusted + (1 | site_id),
                  data = data_raw)
m_aws_null <- glmer.nb(AWS ~ 1 + (1 | site_id),
                       data = data_raw)
anova(m_aws, m_aws) 
AIC(m_aws, m_aws_null)


