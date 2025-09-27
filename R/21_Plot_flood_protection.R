# ----------------------
# A) Infiltration
# ----------------------
pred_infil <- ggpredict(m_infil, terms = "sample_place") %>%
  dplyr::filter(group != "1")

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

p_infil_pred <- p_infil +
  ggplot2::geom_point(
    data = pred_infil,
    ggplot2::aes(x = group, y = predicted),
    inherit.aes = FALSE,
    color = "red", size = 3
  ) +
  ggplot2::geom_errorbar(
    data = pred_infil,
    ggplot2::aes(x = group, ymin = conf.low, ymax = conf.high),
    inherit.aes = FALSE,
    color = "red", width = 0.2
  )

ggsave(p_infil, filename = "Outputs/Plots/p_infil.png",
       height = 5, width = 5)

# ----------------------
# B) Water field capacity
# ----------------------
# Prepare data and predictions, then build plot (explicit namespaces everywhere)
data_wfc <- dplyr::filter(data_raw, !is.na(WFC_adjusted), !is.na(sample_place), !is.na(depth_cm))

# ensure factors and consistent levels
data_wfc$sample_place <- factor(data_wfc$sample_place,
                                levels = unique(as.character(data_wfc$sample_place)))
data_wfc$depth_cm <- factor(data_wfc$depth_cm,
                            levels = unique(as.character(data_wfc$depth_cm)))

# predictions from model (ggeffects)
pred_wfc <- as.data.frame(ggeffects::ggpredict(m_wfc, terms = c("sample_place", "depth_cm")))

# ggpredict returns columns 'x' (sample_place) and 'group' (depth_cm)
# make them explicit and as factors matching the raw data
pred_wfc$sample_place <- factor(pred_wfc$x, levels = levels(data_wfc$sample_place))
pred_wfc$depth_cm     <- factor(pred_wfc$group, levels = levels(data_wfc$depth_cm))

# Build the plot with explicit namespaces
p_wfc <- ggplot2::ggplot(data_wfc, ggplot2::aes(x = sample_place, y = WFC_adjusted)) +
  # boxplots by depth (dodged)
  ggplot2::geom_boxplot(
    ggplot2::aes(fill = depth_cm, colour = depth_cm),
    width = 0.15, outlier.shape = NA, alpha = 0.7,
    position = ggplot2::position_dodge(width = 0.8)
  ) +
  # raw points (jitter + dodge so they sit in the correct depth group)
  ggplot2::geom_point(
    ggplot2::aes(colour = depth_cm),
    position = ggplot2::position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
    size = 1, alpha = 0.5
  ) +
  # predicted means (from pred_wfc) - note inherit.aes = FALSE so no accidental inheritance
  ggplot2::geom_point(
    data = pred_wfc,
    mapping = ggplot2::aes(x = sample_place, y = predicted, group = depth_cm, colour = depth_cm),
    position = ggplot2::position_dodge(width = 0.8),
    size = 3, shape = 18,
    inherit.aes = FALSE
  ) +
  # predicted 95% CI bars
  ggplot2::geom_errorbar(
    data = pred_wfc,
    mapping = ggplot2::aes(x = sample_place, ymin = conf.low, ymax = conf.high, group = depth_cm, colour = depth_cm),
    position = ggplot2::position_dodge(width = 0.8),
    width = 0.2,
    inherit.aes = FALSE
  ) +
  # labels + legend and flip coordinates
  ggplot2::labs(
    x = "Habitat (sample_place)",
    y = "Water field capacity (WFC_adjusted)",
    colour = "Depth (cm)",
    fill = "Depth (cm)",
    title = "B) Water field capacity"
  ) +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal()

# save
ggplot2::ggsave(p_wfc, filename = "Outputs/Plots/p_wfc.png", height = 5, width = 6)

# show
p_wfc

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
