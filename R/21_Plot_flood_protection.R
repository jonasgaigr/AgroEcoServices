# A) Infiltration ----
data_infil <- dplyr::filter(
  data_raw,
  !is.na(infiltration_adjusted),
  !is.na(sample_place),
  !is.na(depth_cm)
)

pred_infil <- ggeffects::ggpredict(m_infil, terms = "sample_place") %>%
  dplyr::filter(group != "1")

# ensure correct factor order
data_infil$sample_place <- factor(
  data_infil$sample_place,
  levels = c("Field", "Bed", "Edible forest", "Woodland")
)
data_infil$depth_cm <- factor(
  data_infil$depth_cm,
  levels = c("0-5", "10-15", "25-30")
)

pred_infil$sample_place <- factor(
  pred_infil$sample_place,
  levels = c("Field", "Bed", "Edible forest", "Woodland")
)
pred_infil$depth_cm <- factor(
  pred_infil$depth_cm,
  levels = c("0-5", "10-15", "25-30")
)

# raincloud plot
p_infil <- ggplot(
  data = data_infil,
  aes(
    x = sample_place, 
    y = log1p(infiltration_adjusted),
    fill = sample_place,
    colour = sample_place),
  ) +
  # half violin ("cloud")
  see::geom_violinhalf(
    position = position_nudge(x = 0.2, y = 0), 
    side = "l", alpha = 0.6, trim = FALSE
  ) +
  geom_boxplot(
    width = 0.2, outlier.shape = NA, alpha = 0.8,
    side = "l",  alpha = 0.6, trim = FALSE
  ) +
  # raw points ("rain")
  geom_jitter(
    width = 0.1, alpha = 0.5, size = 1
  ) +
  scale_fill_manual(values = habitat_cols, guide = "none") +
  scale_colour_manual(values = habitat_cols, guide = "none") +
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

ggsave(p_infil_pred, filename = "Outputs/Plots/p_infil.png",
       height = 5, width = 5)


# B) Water field capacity ----

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

# ensure correct factor order
data_wfc$sample_place <- factor(
  data_wfc$sample_place,
  levels = c("Field", "Bed", "Edible forest", "Woodland")
)
data_wfc$depth_cm <- factor(
  data_wfc$depth_cm,
  levels = c("25-30", "10-15", "0-5")
)

pred_wfc$sample_place <- factor(
  pred_wfc$sample_place,
  levels = c("Field", "Bed", "Edible forest", "Woodland")
)
pred_wfc$depth_cm <- factor(
  pred_wfc$depth_cm,
  levels = c("25-30", "10-15", "0-5")
)

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
  scale_fill_manual(values = depth_cols) +
  scale_colour_manual(values = depth_cols) +
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


# C) Aggregate water stability (AWS) -----

# Manual prediction on the model (log1p) scale + WFC-style plot for AWS
# Run after m_aws is already fitted (your lmer model on log1p(AWS))

# 1) prepare data (raw + transformed) ----------------------------------------
data_aws <- dplyr::filter(data_raw, !is.na(AWS), !is.na(sample_place), !is.na(depth_cm))
data_aws <- dplyr::mutate(data_aws, AWS_log = log1p(AWS))

# ensure factor levels match model-fitting data
data_aws$sample_place <- factor(data_aws$sample_place, levels = unique(as.character(data_aws$sample_place)))
data_aws$depth_cm     <- factor(data_aws$depth_cm, levels = unique(as.character(data_aws$depth_cm)))

# make sure contrasts for newdata equal contrasts used when fitting the model
# (this preserves coding used in fixef/vcov)
contrasts(data_aws$sample_place) <- contrasts(data_aws$sample_place)
contrasts(data_aws$depth_cm)     <- contrasts(data_aws$depth_cm)


# 2) create prediction grid ---------------------------------------------------
newdata <- expand.grid(
  sample_place = levels(data_aws$sample_place),
  depth_cm     = levels(data_aws$depth_cm),
  stringsAsFactors = FALSE
)
newdata$sample_place <- factor(newdata$sample_place, levels = levels(data_aws$sample_place))
newdata$depth_cm     <- factor(newdata$depth_cm, levels = levels(data_aws$depth_cm))

# apply same contrasts to the grid
contrasts(newdata$sample_place) <- contrasts(data_aws$sample_place)
contrasts(newdata$depth_cm)     <- contrasts(data_aws$depth_cm)


# 3) build model matrix for fixed effects (matching formula: sample_place * depth_cm)
#    (we explicitly build X for "~ sample_place * depth_cm")
X <- stats::model.matrix(~ sample_place * depth_cm, data = newdata)

# 4) extract fixed effects and their covariance
beta  <- lme4::fixef(m_aws)        # fixed-effect coefficients (on log1p scale)
Vbeta <- stats::vcov(m_aws)        # covariance matrix of fixed effects

# Ensure column order in X matches names(beta); add zero columns if necessary
missing_coefs <- setdiff(names(beta), colnames(X))
if (length(missing_coefs) > 0) {
  # add zero columns for any missing (should be rare)
  X <- cbind(X, matrix(0, nrow = nrow(X), ncol = length(missing_coefs),
                       dimnames = list(NULL, missing_coefs)))
}
# reorder columns of X to match beta order
X <- X[, names(beta), drop = FALSE]

# 5) predicted mean on model scale and standard errors (Wald)
pred_lin <- as.numeric(X %*% beta)                      # predicted log1p(AWS)
se_pred  <- sqrt( rowSums((X %*% Vbeta) * X) )          # se on log1p scale

# 6) CI using t-approx (df = n - p)
n   <- length(stats::resid(m_aws))
p   <- length(beta)
df  <- max(1, n - p)
tq  <- stats::qt(0.975, df = df)
pred_df <- data.frame(newdata,
                      predicted = pred_lin,
                      se = se_pred,
                      conf.low = pred_lin - tq * se_pred,
                      conf.high = pred_lin + tq * se_pred,
                      stringsAsFactors = FALSE)

# 7) quick sanity checks (optional)
# print head and compare factor levels
print(pred_df)
# check match of names
# print(colnames(X))
# print(names(beta))

# ensure correct order in both datasets
data_aws$sample_place <- factor(
  data_aws$sample_place,
  levels = c("Field", "Bed", "Edible forest", "Woodland")
)
data_aws$depth_cm <- factor(
  data_aws$depth_cm,
  levels = c("25-30", "10-15", "0-5")
)

pred_df$sample_place <- factor(
  pred_df$sample_place,
  levels = c("Field", "Bed", "Edible forest", "Woodland")
)
pred_df$depth_cm <- factor(
  pred_df$depth_cm,
  levels = c("25-30", "10-15", "0-5")
)

# 8) Plot (boxplots + jitter + model predictions) on log1p scale --------------
p_aws <- ggplot2::ggplot(data_aws, ggplot2::aes(x = sample_place, y = AWS_log)) +
  # boxplots by depth (dodged)
  ggplot2::geom_boxplot(
    ggplot2::aes(fill = depth_cm, colour = depth_cm),
    width = 0.15, outlier.shape = NA, alpha = 0.7,
    position = ggplot2::position_dodge(width = 0.8)
  ) +
  # raw points (jittered & dodged)
  ggplot2::geom_point(
    ggplot2::aes(colour = depth_cm),
    position = ggplot2::position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
    size = 1, alpha = 0.5
  ) +
  # predicted means (on log1p scale) - inherit.aes = FALSE so we only use pred_df aesthetics
  ggplot2::geom_point(
    data = pred_df,
    mapping = ggplot2::aes(x = sample_place, y = predicted, group = depth_cm, colour = depth_cm),
    position = ggplot2::position_dodge(width = 0.8),
    size = 3, shape = 18,
    inherit.aes = FALSE
  ) +
  # predicted 95% CI bars (on log1p scale)
  ggplot2::geom_errorbar(
    data = pred_df,
    mapping = ggplot2::aes(x = sample_place, ymin = conf.low, ymax = conf.high, group = depth_cm, colour = depth_cm),
    position = ggplot2::position_dodge(width = 0.8),
    width = 0.2,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = depth_cols) +
  scale_colour_manual(values = depth_cols) +
  # labels + legend and flip coordinates
  ggplot2::labs(
    x = "Habitat (sample_place)",
    y = "log(1 + AWS)",
    colour = "Depth (cm)",
    fill = "Depth (cm)",
    title = "C) Aggregate water stability (AWS)"
  ) +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal()

# 9) save + show
ggplot2::ggsave(p_aws, filename = "Outputs/Plots/p_aws.png", height = 5, width = 6)
p_aws


# D) Soil organic carbon (SOC) ----
# ----------------------
# D) Soil organic carbon (SOC) ----
# ----------------------

# 1) Data prep
data_soc <- dplyr::filter(
  data_raw,
  !is.na(SOC),
  !is.na(sample_place),
  !is.na(depth_cm)
)

pred_soc <- ggeffects::ggpredict(m_soc, terms = "sample_place") %>%
  dplyr::filter(group != "1")

# back-transform predicted values and CI
pred_soc <- pred_soc %>%
  dplyr::mutate(
    predicted = exp(predicted) - 1,
    conf.low  = exp(conf.low) - 1,
    conf.high = exp(conf.high) - 1
  )

# 2) Ensure correct factor order
data_soc$sample_place <- factor(
  data_soc$sample_place,
  levels = c("Field", "Bed", "Edible forest", "Woodland")
)
data_soc$depth_cm <- factor(
  data_soc$depth_cm,
  levels = c("0-5", "10-15", "25-30")
)

pred_soc$sample_place <- factor(
  pred_soc$sample_place,
  levels = c("Field", "Bed", "Edible forest", "Woodland")
)
pred_soc$depth_cm <- factor(
  pred_soc$depth_cm,
  levels = c("0-5", "10-15", "25-30")
)

# 3) Raincloud plot
p_soc <- ggplot(
  data = data_soc,
  aes(
    x = sample_place, 
    y = SOC,
    fill = sample_place,
    colour = sample_place
  )
) +
  # half violin ("cloud")
  see::geom_violinhalf(
    position = position_nudge(x = 0.2, y = 0),
    side = "l", alpha = 0.6, trim = FALSE
  ) +
  geom_boxplot(
    width = 0.2, outlier.shape = NA, alpha = 0.8
  ) +
  # raw points ("rain")
  geom_jitter(
    width = 0.1, alpha = 0.5, size = 1
  ) +
  scale_fill_manual(values = habitat_cols, guide = "none") +
  scale_colour_manual(values = habitat_cols, guide = "none") +
  labs(
    x = "Habitat (sample_place)",
    y = "Soil Organic Carbon (%)",
    title = "D) Soil Organic Carbon (SOC) ~ sample_place"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),  # keeps 0 visible
    limits = c(0, NA),                       # clip below 0
    name = "Soil Organic Carbon (%)"
  ) +
  coord_flip() +
  theme_minimal()

# 4) Add predicted means + CI
p_soc_pred <- p_soc +
  ggplot2::geom_point(
    data = pred_soc,
    ggplot2::aes(x = group, y = predicted),
    inherit.aes = FALSE,
    color = "red", size = 3
  ) +
  ggplot2::geom_errorbar(
    data = pred_soc,
    ggplot2::aes(x = group, ymin = conf.low, ymax = conf.high),
    inherit.aes = FALSE,
    color = "red", width = 0.2
  )

# 5) Save
ggsave(p_soc_pred, filename = "Outputs/Plots/p_soc.png",
       height = 5, width = 5)

# E) Bare soil (BS) ----

# Prepare data and predictions, then build plot (explicit namespaces everywhere)
data_bs <- dplyr::filter(data_raw, !is.na(BS), !is.na(sample_place), !is.na(depth_cm))

# ensure factors and consistent levels
data_bs$sample_place <- factor(data_bs$sample_place,
                                levels = unique(as.character(data_bs$sample_place)))
data_bs$depth_cm <- factor(data_bs$depth_cm,
                            levels = unique(as.character(data_bs$depth_cm)))

# predictions from model (ggeffects)
pred_bs <- as.data.frame(ggeffects::ggpredict(m_bs, terms = c("sample_place")))

# ggpredict returns columns 'x' (sample_place) and 'group' (depth_cm)
# make them explicit and as factors matching the raw data
pred_bs$sample_place <- factor(pred_bs$x, levels = levels(data_bs$sample_place))

# ensure correct order in both datasets
data_bs$sample_place <- factor(
  data_bs$sample_place,
  levels = c("Field", "Bed", "Edible forest", "Woodland")
)

pred_bs$sample_place <- factor(
  pred_bs$sample_place,
  levels = c("Field", "Bed", "Edible forest", "Woodland")
)

## 8) Plot (WFC style) ----
p_bs <- ggplot2::ggplot(
  data_bs, 
  ggplot2::aes(
    x = sample_place, 
    y = BS, 
    fill = sample_place, 
    colour = sample_place
    )
  ) +
  #ggplot2::geom_errorbar(
  #  data = pred_bs,
  #  mapping = ggplot2::aes(
  #    x = sample_place, ymin = conf.low, ymax = conf.high
  #  ),
  #  position = ggplot2::position_dodge(width = 0.8),
  #  width = 0.2,
  #  alpha = 0.4,
  #  inherit.aes = FALSE
  #) + 
  #ggplot2::geom_point(
  #  data = pred_bs,
  #  mapping = ggplot2::aes(x = sample_place, y = predicted),
  #  position = ggplot2::position_dodge(width = 0.8),
  #  size = 3, shape = 18,
  #  alpha = 0.4,
  #  inherit.aes = FALSE
  #) +
  ggplot2::geom_boxplot(
    width = 0.15, outlier.shape = NA, alpha = 0.7,
    position = ggplot2::position_dodge(width = 0.8)
  ) +
  ggplot2::geom_point(
    position = ggplot2::position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
    size = 1, alpha = 0.5
  ) +
  ggplot2::scale_fill_manual(values = habitat_cols, name = "Habitat", guide = "none") +
  ggplot2::scale_colour_manual(values = habitat_cols, name = "Habitat", guide = "none") +
  ggplot2::labs(
    x = "Habitat (sample_place)",
    y = "Bare Soil (BS)",
    title = "D) Bare Soil (BS)"
  ) +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal()

## 9) Save and display ----
ggplot2::ggsave(p_bs, filename = "Outputs/Plots/p_bs.png", height = 5, width = 6)

p_bs

# ----------------------
# Combine plots
# ----------------------
p_infil / p_wfc / p_aws
