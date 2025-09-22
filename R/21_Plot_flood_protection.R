# ----------------------
# A) Infiltration
# ----------------------
pred_infil <- ggpredict(m_infil, terms = "sample_place")

p_infil <- ggplot(pred_infil, aes(x, predicted)) +
  geom_violin(fill = "skyblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(x = "Habitat (sample_place)",
       y = "log(1+Infiltration)",
       title = "A) Infiltration ~ sample_place") +
  theme_minimal()

# ----------------------
# B) Water field capacity
# ----------------------
pred_wfc <- ggpredict(m_wfc, terms = c("sample_place", "depth_cm"))

p_wfc <- ggplot(pred_wfc, aes(x = x, y = predicted, colour = group)) +
  geom_point() +
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
