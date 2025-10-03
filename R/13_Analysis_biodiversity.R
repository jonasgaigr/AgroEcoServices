# ----------------------
# A) Species richness ----
# ----------------------
m_bd <- stats::glm(
  species_richness ~ SOC_0_5 * sample_place,
  data = data_bd,
  family = stats::poisson()
)

m_bd_null <- stats::glm(
  SOC_0_5 ~ 1,
  data = data_bd,
  family = stats::poisson()
)

# comparisons & diagnostics
stats::anova(m_bd, m_bd_null)
stats::AIC(m_bd, m_bd_null)

stats::qqnorm(stats::resid(m_bd)); stats::qqline(stats::resid(m_bd))
