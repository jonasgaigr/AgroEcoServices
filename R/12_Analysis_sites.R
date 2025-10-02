# === Run all site-level GLMs and export tables/reports ===
# Creates Outputs/Tables and Outputs/Plots if missing
base::dir.create("Outputs/Tables", recursive = TRUE, showWarnings = FALSE)
base::dir.create("Outputs/Plots", recursive = TRUE, showWarnings = FALSE)

# ----------------------
# A) Infiltration (site effect) ----
# ----------------------
m_site_infil <- stats::glm(
  log1p(infiltration_adjusted) ~ site_id,
  data = data_raw,
  family = stats::Gamma(link = "log")
)

m_site_infil_null <- stats::glm(
  log1p(infiltration_adjusted) ~ 1,
  data = data_raw,
  family = stats::Gamma(link = "log")
)

# comparisons & diagnostics
stats::anova(m_site_infil, m_site_infil_null, test = "Chisq")
stats::AIC(m_site_infil, m_site_infil_null)

stats::qqnorm(stats::resid(m_site_infil)); stats::qqline(stats::resid(m_site_infil))

# tidy & save
tab_site_infil <- broom::tidy(m_site_infil, conf.int = TRUE, conf.level = 0.95)
utils::write.csv(tab_site_infil, file = "Outputs/Tables/Infiltration_site_tidy.csv", row.names = FALSE)

write_tab_site_infil <- sjPlot::tab_model(
  m_site_infil, m_site_infil_null,
  show.aic = TRUE,
  show.ci = 0.95,
  dv.labels = c("log(1+Infiltration) ~ Site", "Null model"),
  file = "Outputs/Tables/Infiltration_site_models.doc"
)

write_tab_site_infil

report_site_infil <- report::report(m_site_infil)
base::cat(report_site_infil, file = "Outputs/Tables/Infiltration_site_report.txt")

# ----------------------
# B) Water Field Capacity (site effect) ----
# ----------------------
m_site_wfc <- stats::glm(
  WFC_adjusted ~ site_id,
  data = data_raw,
  family = stats::gaussian()    # glm with identity link (equivalent to lm)
)

m_site_wfc_null <- stats::glm(
  WFC_adjusted ~ 1,
  data = data_raw,
  family = stats::gaussian()
)

stats::anova(m_site_wfc, m_site_wfc_null, test = "Chisq")
stats::AIC(m_site_wfc, m_site_wfc_null)

stats::qqnorm(stats::resid(m_site_wfc)); stats::qqline(stats::resid(m_site_wfc))

tab_site_wfc <- broom::tidy(m_site_wfc, conf.int = TRUE, conf.level = 0.95)
utils::write.csv(tab_site_wfc, file = "Outputs/Tables/WFC_site_tidy.csv", row.names = FALSE)

write_tab_site_wfc <- sjPlot::tab_model(
  m_site_wfc, m_site_wfc_null,
  show.aic = TRUE,
  show.ci = 0.95,
  dv.labels = c("WFC_adjusted ~ Site", "Null model"),
  file = "Outputs/Tables/WFC_site_models.doc"
)

write_tab_site_wfc

report_site_wfc <- report::report(m_site_wfc)
base::cat(report_site_wfc, file = "Outputs/Tables/WFC_site_report.txt")

# ----------------------
# C) Aggregate water stability (AWS) (site effect) ----
# ----------------------
m_site_aws <- glm(
  log1p(AWS) ~ site_id,
  data = data_raw,
  family = gaussian()
)

m_site_aws_null <- stats::glm(
  log1p(AWS) ~ 1,
  data = data_raw,
  family = stats::gaussian()
)

stats::anova(m_site_aws, m_site_aws_null, test = "Chisq")
stats::AIC(m_site_aws, m_site_aws_null)

stats::qqnorm(stats::resid(m_site_aws)); stats::qqline(stats::resid(m_site_aws))

tab_site_aws <- broom::tidy(m_site_aws, conf.int = TRUE, conf.level = 0.95)
utils::write.csv(tab_site_aws, file = "Outputs/Tables/AWS_site_tidy.csv", row.names = FALSE)

write_tab_site_aws <- sjPlot::tab_model(
  m_site_aws, m_site_aws_null,
  show.aic = TRUE,
  show.ci = 0.95,
  dv.labels = c("log(1+AWS) ~ Site", "Null model"),
  file = "Outputs/Tables/AWS_site_models.doc"
)

write_tab_site_aws

report_site_aws <- report::report(m_site_aws)
base::cat(report_site_aws, file = "Outputs/Tables/AWS_site_report.txt")

# ----------------------
# D) Soil Organic Carbon (SOC) (site effect) ----
# ----------------------
m_site_soc <- stats::glm(
  log1p(SOC) ~ site_id,
  data = data_raw,
  family = stats::Gamma(link = "log")
)

m_site_soc_null <- stats::glm(
  log1p(SOC) ~ 1,
  data = data_raw,
  family = stats::Gamma(link = "log")
)

stats::anova(m_site_soc, m_site_soc_null, test = "Chisq")
stats::AIC(m_site_soc, m_site_soc_null)

stats::qqnorm(stats::resid(m_site_soc)); stats::qqline(stats::resid(m_site_soc))

tab_site_soc <- broom::tidy(m_site_soc, conf.int = TRUE, conf.level = 0.95)
utils::write.csv(tab_site_soc, file = "Outputs/Tables/SOC_site_tidy.csv", row.names = FALSE)

write_tab_site_soc <- sjPlot::tab_model(
  m_site_soc, m_site_soc_null,
  show.aic = TRUE,
  show.ci = 0.95,
  dv.labels = c("log(1+SOC) ~ Site", "Null model"),
  file = "Outputs/Tables/SOC_site_models.doc"
)

write_tab_site_soc

report_site_soc <- report::report(m_site_soc)
base::cat(report_site_soc, file = "Outputs/Tables/SOC_site_report.txt")

# ----------------------
# E) Bare Soil (BS) (site effect) ----
# ----------------------
m_site_bs <- stats::glm(
  log1p(BS) ~ site_id,
  data = data_raw,
  family = stats::gaussian()
)

m_site_bs_null <- stats::glm(
  log1p(BS) ~ 1,
  data = data_raw,
  family = stats::gaussian()
)

stats::anova(m_site_bs, m_site_bs_null, test = "Chisq")
stats::AIC(m_site_bs, m_site_bs_null)

stats::qqnorm(stats::resid(m_site_bs)); stats::qqline(stats::resid(m_site_bs))

tab_site_bs <- broom::tidy(m_site_bs, conf.int = TRUE, conf.level = 0.95)
utils::write.csv(tab_site_bs, file = "Outputs/Tables/BS_site_tidy.csv", row.names = FALSE)

write_tab_site_bs <- sjPlot::tab_model(
  m_site_bs, m_site_bs_null,
  show.aic = TRUE,
  show.ci = 0.95,
  dv.labels = c("BS ~ Site", "Null model"),
  file = "Outputs/Tables/BS_site_models.doc"
)

write_tab_site_bs

report_site_bs <- report::report(m_site_bs)
base::cat(report_site_bs, file = "Outputs/Tables/BS_site_report.txt")
