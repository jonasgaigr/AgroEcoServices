# Response matrix: numeric soil properties
soil_vars <- data_raw %>%
  dplyr::select(
    infiltration_adjusted,
    WFC_adjusted,
    AWS,
    SOC,
    BS,
    WFC_not_adjusted
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
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("PCA (correlation biplot)\nSoil variables")

ggsave(pca_plot, filename = "Outputs/Plots/pca.png", height = 5, width = 6)

# --- RDA (constrained ordination) ---
# Explanatory variables: sample_place + depth_cm + texture
rda_model <- rda(
  soil_vars ~ sample_place + texture, 
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

# Compute centroids
centroids <- aggregate(cbind(RDA1 = site_scores_df$RDA1, RDA2 = site_scores_df$RDA2),
                       by = list(sample_place = site_scores_df$sample_place),
                       FUN = mean)

# RDA plot with hulls, centroids, and arrows
rda_plot <- ggplot(site_scores_df, aes(x = RDA1, y = RDA2, color = sample_place)) +
  geom_point(size = 2) +
  stat_chull(aes(group = sample_place), alpha = 0.2, geom = "polygon") +
  geom_segment(data = arrow_scores_rda,
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(length = unit(0.2, "cm")),
               inherit.aes = FALSE,
               color = "red") +
  geom_text_repel(data = centroids,
                  aes(x = RDA1, y = RDA2, label = sample_place),
                  size = 5, fontface = "bold", color = "black") +
  theme_minimal() +
  ggtitle("RDA\nSoil variables ~ sample_place + depth_cm + texture") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "RDA1", y = "RDA2")

ggsave(rda_plot, filename = "Outputs/Plots/rda.png", height = 5, width = 6)
