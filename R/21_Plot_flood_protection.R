# ----------------------
# A) Infiltration
# ----------------------
pred_infil <- ggpredict(m_infil, terms = "sample_place")

# raincloud plot
p_infil <- ggplot() +
  # half violin ("cloud")
  geom_violinhalf(
    data = data_raw,
    aes(x = sample_place, y = log1p(infiltration_adjusted)),
    side = "l", fill = "skyblue", alpha = 0.6, trim = FALSE
  ) +
  boxplot(
    data = data_raw,
    aes(x = sample_place, y = log1p(infiltration_adjusted)),
    side = "l", fill = "skyblue", alpha = 0.6, trim = FALSE
  ) +
  # raw points ("rain")
  geom_jitter(
    data = data_raw,
    aes(x = sample_place, y = log1p(infiltration_adjusted)),
    width = 0.1, alpha = 0.5, size = 1, color = "grey40"
  ) +
  # predicted means + CI
  labs(
    x = "Habitat (sample_place)",
    y = "log(1+Infiltration)",
    title = "A) Infiltration ~ sample_place"
  ) +
  coord_flip() +
  theme_minimal()


p_infil

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
