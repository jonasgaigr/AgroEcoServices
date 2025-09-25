# ----------------------
# A) Infiltration
# ----------------------
pred_infil <- ggpredict(m_infil, terms = "sample_place")

# raincloud plot
p_infil <- ggplot(
  data = data_raw,
  aes(x = reorder(sample_place, desc(log1p(infiltration_adjusted))), y = log1p(infiltration_adjusted)),
  ) +
  # half violin ("cloud")
  see::geom_violinhalf(
    position = position_nudge(x = 0.2, y = 0), 
    side = "l", fill = "skyblue", alpha = 0.6, trim = FALSE
  ) +
  geom_boxplot(
    width = 0.2, outlier.shape = NA, alpha = 0.8,
    side = "l", fill = "skyblue", alpha = 0.6, trim = FALSE
  ) +
  # raw points ("rain")
  geom_jitter(
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

ggsave(p_infil, filename = "Outputs/Plots/p_infil.png",
       height = 5, width = 5)

# ----------------------
# B) Water field capacity
# ----------------------
p_wfc <- ggplot(data = data_raw,
                aes(x = sample_place,
                    y = WFC_adjusted,
                    colour = as.factor(depth_cm),
                    group = as.factor(depth_cm))
                ) +
  geom_point(size = 1) +
  labs(x = "Habitat (sample_place)",
       y = "WFC_adjusted",
       colour = "Depth",
       fill = "Depth",
       title = "B) Water field capacity") +
  theme_minimal()

ggsave(p_wfc, filename = "Outputs/Plots/p_wfc.png",
       height = 5, width = 5)

library(ggplot2)
library(dplyr)
library(ggdist)

# drop NA just in case
data_wfc <- data_raw %>% drop_na(WFC_adjusted, sample_place, depth_cm)

p_wfc <- ggplot2::ggplot(data_wfc,
                         ggplot2::aes(x = sample_place,
                                      y = WFC_adjusted,
                                      fill = as.factor(depth_cm),
                                      colour = as.factor(depth_cm))) +
  # boxplot
  ggplot2::geom_boxplot(
    width = 0.15, outlier.shape = NA, alpha = 0.7,
    position = ggplot2::position_dodge(width = 0.8)
  ) +
  ggplot2::labs(
    x = "Habitat (sample_place)",
    y = "Water field capacity (WFC_adjusted)",
    colour = "Depth (cm)",
    fill = "Depth (cm)",
    title = "B) Water field capacity"
  ) +
  ggplot2::theme_minimal()


ggsave(p_wfc, filename = "Outputs/Plots/p_wfc.png",
       height = 5, width = 6)


# ----------------------
# C) AWS
# ----------------------


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
