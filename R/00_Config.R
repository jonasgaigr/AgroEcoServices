#----------------------------------------------------------#
# Load packages -----
#----------------------------------------------------------#
packages <- c(
  "tidyverse",
  "janitor",
  "sf", 
  "sp", 
  "proj4", 
  "openxlsx",
  "fuzzyjoin", 
  "remotes",
  "ggtext",
  "vegan",
  "lme4",
  "glmmTMB",
  "ggplot2",
  "ggdist",
  "ggeffects",
  "ggrepel",
  "ggpubr",
  "patchwork",
  "sjPlot"
  )

# Standard package
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

#----------------------------------------------------------#
# Load data -----
#----------------------------------------------------------#
data_raw <- readr::read_csv("Data/Input/data_raw.csv") %>%
  dplyr::mutate(
    site_id = as.factor(site_id),
    sample_place = as.factor(sample_place),
    texture = as.factor(texture),
    depth_cm = as.factor(depth_cm),
    depth_cat = dplyr::case_when(
      depth_cm == "0-5" ~ 1,
      depth_cm == "10-15" ~ 2,
      depth_cm == "25-30" ~ 3
      )
    )

# Check structure
glimpse(data_raw) 

# Check unique values for categorical candidates
lapply(data_raw, function(x) if(is.character(x) | is.factor(x)) unique(x))


# Custom palette for 4 habitats
habitat_cols <- c(
  "Woodland"      = "#006400",  # deep green
  "Field"         = "#E66101",  # warm orange
  "Edible forest" = "#4DAF4A",  # fresh green
  "Bed"           = "#5AB4AC"   # teal
)

# Custom depth palette
depth_cols <- c(
  "0-5"   = "#a6cee3",  # light blue
  "10-15" = "#e07b39",  # ochre/orange
  "25-30" = "#4b2e2e"   # dark brown
)
