# Response matrix: numeric soil properties
soil_vars <- data_raw %>%
  dplyr::select(
    infiltration_adjusted,
    WFC_adjusted,
    AWS,
    SOC,
    BS
    )

# --- PCA (unconstrained ordination) ---
pca <- rda(soil_vars, scale = TRUE)

# Fit soil variables as vectors (arrows)
ef_pca <- envfit(pca, soil_vars, permutations = 999)

# Summary of PCA
summary(pca)

# Screeplot of eigenvalues
screeplot(pca, bstick = TRUE, main = "PCA\nSoil variables")

# PCA site scores
site_scores <- scores(pca, display = "sites", scaling = 2)
site_scores_df <- as.data.frame(site_scores)
site_scores_df$sample_place <- data_raw$sample_place

# Arrows (biplot scores for soil variables)
arrow_scores <- as.data.frame(scores(ef_pca, display = "vectors", scaling = 2))

# Compute centroids
centroids <- aggregate(cbind(PC1 = site_scores_df$PC1, PC2 = site_scores_df$PC2),
                       by = list(sample_place = site_scores_df$sample_place),
                       FUN = mean)

# PCA plot with hulls, centroids, and arrows
pca_plot <- ggplot(site_scores_df, aes(x = PC1, y = PC2, color = sample_place)) +
  geom_point() +
  stat_chull(aes(group = sample_place), alpha = 0.2, geom = "polygon") +
  geom_segment(data = arrow_scores,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")),
               inherit.aes = FALSE,
               color = "red") +
  geom_text_repel(data = centroids,
                  aes(x = PC1, y = PC2, label = sample_place),
                  size = 5, fontface = "bold", color = "black") +
  geom_text_repel(data = arrow_scores,
                  aes(x = PC1, y = PC2, label = rownames(arrow_scores)),
                  inherit.aes = FALSE,
                  color = "red", size = 4) +
  ggplot2::scale_fill_manual(values = habitat_cols, name = "Habitat", guide = "none") +
  ggplot2::scale_colour_manual(values = habitat_cols, name = "Habitat", guide = "none") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("PCA (correlation biplot)\nSoil variables")

ggsave(pca_plot, filename = "Outputs/Plots/pca.png", height = 5, width = 6)

# Save PCA as PDF
ggsave("Outputs/Plots/pca.pdf",
       plot = pca_plot, width = 5, height = 5, device = cairo_pdf)

# --- RDA (constrained ordination) ---
# Explanatory variables: sample_place
rda_model <- rda(
  soil_vars ~ sample_place, 
  data = data_raw, 
  scale = TRUE
  )

# Model summary
summary(rda_model)

# Significance tests
anova(rda_model)                 # overall test
anova(rda_model, by = "axis")    # by axis
anova(rda_model, by = "terms")   # by terms

# Site scores
site_scores <- scores(rda_model, display = "sites", scaling = 2)
site_scores_df <- as.data.frame(site_scores)
site_scores_df$sample_place <- data_raw$sample_place

# Arrows (biplot scores for soil variables)
arrow_scores_rda <- as.data.frame(scores(rda_model, display = "bp", scaling = 2))
arrow_scores_rda$varname <- rownames(arrow_scores_rda)
colnames(arrow_scores_rda)[1:2] <- c("RDA1", "RDA2")

# Compute centroids
centroids <- aggregate(cbind(RDA1 = site_scores_df$RDA1, RDA2 = site_scores_df$RDA2),
                       by = list(sample_place = site_scores_df$sample_place),
                       FUN = mean)

# RDA plot with hulls, centroids, and arrows
rda_plot <- ggplot(site_scores_df, aes(x = RDA1, y = RDA2, color = sample_place)) +
  geom_point(size = 2) +
  stat_chull(aes(group = sample_place), alpha = 0.2, geom = "polygon") +
  #geom_segment(data = arrow_scores_rda,
  #             aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
  #             arrow = arrow(length = unit(0.2, "cm")),
  #             inherit.aes = FALSE,
  #             color = "red") +
  geom_text_repel(data = centroids,
                  aes(x = RDA1, y = RDA2, label = sample_place),
                  size = 5, fontface = "bold", color = "black") +
  #geom_text_repel(data = arrow_scores_rda,
  #                aes(x = RDA1, y = RDA2, label = varname),
  #                inherit.aes = FALSE,
  #                color = "red", size = 4) +
  ggplot2::scale_fill_manual(values = habitat_cols, name = "Habitat", guide = "none") +
  ggplot2::scale_colour_manual(values = habitat_cols, name = "Habitat", guide = "none") +
  theme_minimal() +
  ggtitle("RDA\nSoil variables ~ sample_place + texture") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "RDA1", y = "RDA2")

ggsave(rda_plot, filename = "Outputs/Plots/rda.png", height = 5, width = 6)

# Save RDA as PDF
ggsave("Outputs/Plots/rda.pdf",
       plot = rda_plot, width = 5, height = 5, device = cairo_pdf)

# ==============================
# PCA for site_id
# ==============================

# Response matrix: numeric soil properties
soil_vars_site <- data_raw %>%
  dplyr::select(
    infiltration_adjusted,
    WFC_adjusted,
    AWS,
    SOC,
    BS
  )

# --- PCA (unconstrained ordination) ---
pca_site <- rda(soil_vars_site, scale = TRUE)

# Fit soil variables as vectors (arrows)
ef_pca_site <- envfit(pca_site, soil_vars_site, permutations = 999)

# Summary of PCA
summary(pca_site)

# Screeplot of eigenvalues
screeplot(pca_site, bstick = TRUE, main = "PCA (site_id)\nSoil variables")

# PCA site scores
pca_site_scores <- scores(pca_site, display = "sites", scaling = 2)
pca_site_scores_df <- as.data.frame(pca_site_scores)
pca_site_scores_df$site_id <- data_raw$site_id

# Arrows (biplot scores for soil variables)
pca_arrow_scores_site <- as.data.frame(scores(ef_pca_site, display = "vectors", scaling = 2))

# Compute centroids by site_id
pca_centroids_site <- aggregate(cbind(PC1 = pca_site_scores_df$PC1, PC2 = pca_site_scores_df$PC2),
                                by = list(site_id = pca_site_scores_df$site_id),
                                FUN = mean)

# PCA plot with hulls, centroids, and arrows
pca_site_plot <- ggplot(pca_site_scores_df, aes(x = PC1, y = PC2, color = site_id)) +
  geom_point() +
  stat_chull(aes(group = site_id), alpha = 0.2, geom = "polygon") +
  geom_segment(data = pca_arrow_scores_site,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")),
               inherit.aes = FALSE,
               color = "red") +
  geom_text_repel(data = pca_centroids_site,
                  aes(x = PC1, y = PC2, label = site_id),
                  size = 5, fontface = "bold", color = "black") +
  geom_text_repel(data = pca_arrow_scores_site,
                  aes(x = PC1, y = PC2, label = rownames(pca_arrow_scores_site)),
                  inherit.aes = FALSE,
                  color = "red", size = 4) +
  ggplot2::scale_fill_manual(values = habitat_cols, name = "Site", guide = "none") +
  ggplot2::scale_colour_manual(values = habitat_cols, name = "Site", guide = "none") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("PCA (correlation biplot)\nSoil variables by site_id")

ggsave(pca_site_plot, filename = "Outputs/Plots/pca_site_id.png", height = 5, width = 6)
ggsave("Outputs/Plots/pca_site_id.pdf", plot = pca_site_plot, width = 5, height = 5, device = cairo_pdf)


# ==============================
# RDA for site_id
# ==============================

rda_site <- rda(
  soil_vars_site ~ site_id,
  data = data_raw,
  scale = TRUE
)

# Model summary
summary(rda_site)

# Significance tests
anova(rda_site)                 # overall test
anova(rda_site, by = "axis")    # by axis
anova(rda_site, by = "terms")   # by terms

# Site scores
rda_site_scores <- scores(rda_site, display = "sites", scaling = 2)
rda_site_scores_df <- as.data.frame(rda_site_scores)
rda_site_scores_df$site_id <- data_raw$site_id

# Arrows (biplot scores for soil variables)
rda_arrow_scores_site <- as.data.frame(scores(rda_site, display = "bp", scaling = 2))
rda_arrow_scores_site$varname <- rownames(rda_arrow_scores_site)
colnames(rda_arrow_scores_site)[1:2] <- c("RDA1", "RDA2")

# Compute centroids by site_id
rda_centroids_site <- aggregate(cbind(RDA1 = rda_site_scores_df$RDA1, RDA2 = rda_site_scores_df$RDA2),
                                by = list(site_id = rda_site_scores_df$site_id),
                                FUN = mean)

# RDA plot with hulls, centroids
rda_site_plot <- ggplot(rda_site_scores_df, aes(x = RDA1, y = RDA2, color = site_id)) +
  geom_point(size = 2) +
  stat_chull(aes(group = site_id), alpha = 0.2, geom = "polygon") +
  geom_text_repel(data = rda_centroids_site,
                  aes(x = RDA1, y = RDA2, label = site_id),
                  size = 5, fontface = "bold", color = "black") +
  ggplot2::scale_fill_manual(values = habitat_cols, name = "Site", guide = "none") +
  ggplot2::scale_colour_manual(values = habitat_cols, name = "Site", guide = "none") +
  theme_minimal() +
  ggtitle("RDA\nSoil variables ~ site_id") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "RDA1", y = "RDA2")

ggsave(rda_site_plot, filename = "Outputs/Plots/rda_site_id.png", height = 5, width = 6)
ggsave("Outputs/Plots/rda_site_id.pdf", plot = rda_site_plot, width = 5, height = 5, device = cairo_pdf)

