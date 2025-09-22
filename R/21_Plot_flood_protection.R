# ----------------------
# A) Infiltration
# ----------------------
m_infil <- lmer(log1p(infiltration_adjusted) ~ sample_place + (1 | site_id),
                data = data_raw)
m_infil_null <- lmer(log1p(infiltration_adjusted) ~ 1 + (1 | site_id),
                     data = data_raw)

anova(m_infil, m_infil_null)
AIC(m_infil, m_infil_null)

pred_infil <- ggpredict(m_infil, terms = "sample_place")

p_infil <- ggplot(pred_infil, aes(x, predicted)) +
  geom_col(fill = "skyblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(x = "Habitat (sample_place)",
       y = "log(1+Infiltration)",
       title = "A) Infiltration ~ sample_place") +
  theme_minimal()

# ----------------------
# B) Water field capacity
# ----------------------
m_wfc <- lmer(WFC_adjusted ~ sample_place * depth_cm +
                infiltration_adjusted + AWS + (1 | site_id),
              data = data_raw)
m_wfc_null <- lmer(WFC_adjusted ~ 1 + (1 | site_id),
                   data = data_raw)

anova(m_wfc, m_wfc_null)
AIC(m_wfc, m_wfc_null)

pred_wfc <- ggpredict(m_wfc, terms = c("sample_place", "depth_cm"))

p_wfc <- ggplot(pred_wfc, aes(x = x, y = predicted, colour = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, colour = NA) +
  labs(x = "Habitat (sample_place)",
       y = "WFC_adjusted",
       colour = "Depth",
       fill = "Depth",
       title = "B) Water field capacity") +
  theme_minimal()

# ----------------------
# C) AWS
# ----------------------
m_aws <- glmer.nb(AWS ~ sample_place * depth_cm +
                    SOC + WFC_adjusted + (1 | site_id),
                  data = data_raw)
m_aws_null <- glmer.nb(AWS ~ 1 + (1 | site_id),
                       data = data_raw)

anova(m_aws, m_aws_null)
AIC(m_aws, m_aws_null)

pred_aws <- ggpredict(m_aws, terms = c("sample_place", "depth_cm"))

p_aws <- ggplot(pred_aws, aes(x = x, y = predicted, colour = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, colour = NA) +
  labs(x = "Habitat (sample_place)",
       y = "AWS (predicted)",
       colour = "Depth",
       fill = "Depth",
       title = "C) Available water storage") +
  theme_minimal()

# ----------------------
# Combine plots
# ----------------------
p_infil / p_wfc / p_aws
