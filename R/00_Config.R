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
  "ggplot2",
  "ggdist",
  "ggeffects",
  "ggrepel",
  "ggpubr",
  "patchwork"
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
