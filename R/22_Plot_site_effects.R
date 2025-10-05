# ================================
# Site-level effect plots (A–E)
# ================================
# define a helper function
# ================================
# Site-level effect plots (A–E)
# ================================
# define a helper function
# modify plot_site_effect
plot_site_effect <- function(model, response, response_label, file_tag, transform = FALSE) {
  # raw data subset
  df <- dplyr::filter(data_raw, !is.na(.data[[response]]), !is.na(site_id))
  
  # predictions
  pred <- as.data.frame(ggeffects::ggpredict(model, terms = "site_id"))
  pred$site_id <- factor(pred$x, levels = unique(df$site_id))
  
  # optionally transform y-axis (log1p)
  y_lab <- if (transform) paste0("log(1 + ", response_label, ")") else response_label
  
  # build plot
  p <- ggplot(df, aes(x = forcats::fct_rev(forcats::fct_inorder(site_id)), y = .data[[response]])) +
    geom_boxplot(width = 0.25, outlier.shape = NA, alpha = 0.7,
                 colour = var_cols[[response_label]]) +
    geom_jitter(width = 0.1, alpha = 0.5, size = 1,
                colour = var_cols[[response_label]]) +
    #geom_point(
    #  data = pred,
    #  aes(x = site_id, y = predicted),
    #  inherit.aes = FALSE,
    #  size = 3, shape = 18, colour = var_cols[[response_label]]
    #) +
    #geom_errorbar(
    #  data = pred,
    #  aes(x = site_id, ymin = conf.low, ymax = conf.high),
    #  inherit.aes = FALSE,
    #  width = 0.2, colour = var_cols[[response_label]]
    #) +
    labs(
      x = "Site",
      y = y_lab,
      title = paste(file_tag, response_label, "~ site_id")
    ) +
    theme_minimal() +
    coord_flip() +
    theme(
      plot.title = element_text(hjust = 0.5)  # centers title
    )
  
  # save
  ggsave(filename = paste0("Outputs/Plots/", file_tag, "_site.png"),
         plot = p, width = 6, height = 5)
  
  return(p)
}


# ----------------------
# A) Infiltration (site)
# ----------------------
p_site_infil <- plot_site_effect(
  m_site_infil, "infiltration_adjusted",
  response_label = "infiltration",
  file_tag = "Infiltration",
  transform = TRUE
)

# B) WFC (site)
p_site_wfc <- plot_site_effect(
  m_site_wfc, "WFC_adjusted",
  response_label = "water field capacity (WFC)",
  file_tag = "WFC",
  transform = FALSE
)

# C) AWS (site)
p_site_aws <- plot_site_effect(
  m_site_aws, "AWS",
  response_label = "available water storage (AWS)",
  file_tag = "AWS",
  transform = TRUE
)

# D) SOC (site)
p_site_soc <- plot_site_effect(
  m_site_soc, "SOC",
  response_label = "soil organic carbon (SOC)",
  file_tag = "SOC",
  transform = TRUE
)

# E) Bare Soil (site)
p_site_bs <- plot_site_effect(
  m_site_bs, "BS",
  response_label = "bare soil (BS)",
  file_tag = "BS",
  transform = TRUE
)

# show one to check
p_site_infil


# ----------------------
# A) Infiltration (site)
# ----------------------
p_site_infil <- plot_site_effect(
  m_site_infil, "infiltration_adjusted",
  response_label = "infiltration",
  file_tag = "Infiltration",
  transform = TRUE
)

# B) WFC (site)
p_site_wfc <- plot_site_effect(
  m_site_wfc, "WFC_adjusted",
  response_label = "water field capacity (WFC)",
  file_tag = "WFC",
  transform = FALSE
)

# C) AWS (site)
p_site_aws <- plot_site_effect(
  m_site_aws, "AWS",
  response_label = "available water storage (AWS)",
  file_tag = "AWS",
  transform = TRUE
)

# D) SOC (site)
p_site_soc <- plot_site_effect(
  m_site_soc, "SOC",
  response_label = "soil organic carbon (SOC)",
  file_tag = "SOC",
  transform = TRUE
)

# E) Bare Soil (site)
p_site_bs <- plot_site_effect(
  m_site_bs, "BS",
  response_label = "bare soil (BS)",
  file_tag = "BS",
  transform = TRUE
)

# show one to check
p_site_infil

# Make a composite plot (2 x 3 grid, with one empty slot)
p_composite_site <- (
  p_site_infil + 
    p_site_wfc + 
    p_site_aws +
    p_site_soc + 
    p_site_bs +
    plot_spacer()
) +
  plot_layout(ncol = 1)

# Center titles globally
p_composite_site <- p_composite_site & theme(plot.title = element_text(hjust = 0.5))

# Save composite
ggsave("Outputs/Plots/Site_effects_composite.png",
       plot = p_composite, width = 12, height = 18)

# Save composite as PDF
ggsave("Outputs/Plots/Site_effects_composite.pdf",
       plot = p_composite, width = 12, height = 10, device = cairo_pdf)
