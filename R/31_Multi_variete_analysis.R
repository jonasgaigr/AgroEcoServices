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

# Summary of PCA
summary(pca)

# Screeplot of eigenvalues
screeplot(pca, bstick = TRUE, main = "PCA\nSoil variables")

# PCA site scores
site_scores <- scores(pca, display = "sites", scaling = 2)
site_scores_df <- as.data.frame(site_scores)
site_scores_df$sample_place <- data_raw$sample_place

# Compute centroids
centroids <- aggregate(cbind(PC1 = site_scores_df$PC1, PC2 = site_scores_df$PC2),
                       by = list(sample_place = site_scores_df$sample_place),
                       FUN = mean)

# PCA plot with hulls and labels
pca <- ggplot(site_scores_df, aes(x = PC1, y = PC2, color = sample_place)) +
  geom_point() +
  ggpubr::stat_chull(aes(group = sample_place), alpha = 0.2, geom = "polygon") +
  ggrepel::geom_text_repel(data = centroids,
                  aes(x = PC1, y = PC2, label = sample_place),
                  size = 5, fontface = "bold", color = "black") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("PCA\nSoil variables (scaling 2)")

ggsave(pca, filename = "Outputs/Plots/pca.png",
       height = 5, width = 6)


# --- RDA (constrained ordination) ---
# Explanatory variables: sample_place + depth_cm + texture
rda_model <- rda(
  soil_vars ~ sample_place + depth_cm + texture, 
  data = data_raw, 
  scale = TRUE
  )

# Model summary
summary(rda_model)

# Significance tests
anova(rda_model)                 # overall test
anova(rda_model, by = "axis")    # by axis
anova(rda_model, by = "terms")   # by terms

# Extract RDA site scores
site_scores <- scores(rda_model, display = "sites", scaling = 2)
site_scores_df <- as.data.frame(site_scores)
site_scores_df$sample_place <- data_raw$sample_place

# Compute centroids for groups
centroids <- aggregate(cbind(RDA1 = site_scores_df$RDA1, RDA2 = site_scores_df$RDA2),
                       by = list(sample_place = site_scores_df$sample_place),
                       FUN = mean)

# RDA plot with hulls and centroid labels
rda <- ggplot(site_scores_df, aes(x = RDA1, y = RDA2, color = sample_place)) +
  geom_point(size = 2) +
  stat_chull(aes(group = sample_place), alpha = 0.2, geom = "polygon") +
  geom_text_repel(data = centroids,
                  aes(x = RDA1, y = RDA2, label = sample_place),
                  size = 5, fontface = "bold", color = "black") +
  theme_minimal() +
  ggtitle("RDA\nSoil variables ~ sample_place + depth_cm + texture") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "RDA1", y = "RDA2")

ggsave(rda, filename = "Outputs/Plots/rda.png",
       height = 5, width = 6)
