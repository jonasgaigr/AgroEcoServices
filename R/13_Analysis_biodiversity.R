# ----------------------
# A) Species richness ----
# ----------------------
m_bd <- stats::glm(
  species_richness ~ sample_place,
  data = data_bd,
  family = stats::gaussian()
)

m_bd_null <- stats::glm(
  SOC_0_5 ~ 1,
  data = data_bd,
  family = stats::gaussian()
)

# comparisons & diagnostics
stats::anova(m_bd, m_bd_null)
stats::AIC(m_bd, m_bd_null)

stats::qqnorm(stats::resid(m_bd)); stats::qqline(stats::resid(m_bd))

# 4. Tidy results
tab_bd <- broom.mixed::tidy(m_bd, effects = "fixed", conf.int = TRUE)

# 5. Make regression tables and export
write_tab_bd <- sjPlot::tab_model(
  m_bd, m_bd_null,
  show.icc = TRUE,
  show.aic = TRUE,
  show.ci = 0.95,
  dv.labels = c("Species richness ~ SOC * Habitat", "Null model"),
  file = "Outputs/Tables/SpeciesRichness_models.doc"
)

write_tab_bd

# 6. Narrative APA-style summary
report_bd <- report::report(m_bd)

# Save the textual report
cat(report_bd, file = "Outputs/Tables/SpeciesRichness_report.txt")
