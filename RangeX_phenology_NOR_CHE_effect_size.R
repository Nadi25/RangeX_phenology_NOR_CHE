
# RangeX phenology data exploration NOR and CHE ------------

## Data used: RangeX_clean_Phenology_2022_CHE.csv
##            RangeX_clean_phenology_2023_NOR.csv
##            RangeX_clean_MetadataFocal_CHE.csv
##            RangeX_metadata_focal_NOR.csv
## Date:      14.08.25
## Author:    Nadine Arzt
## Purpose:   Data exploration CHE


# source script with joining datasets -------------------------------------
source("RangeX_phenology_NOR_CHE_data_combination.R")
# use this
glimpse(phenology)


# combined treatment column -----------------------------------------------
phenology$treatment <- paste(phenology$site, phenology$treat_warming, phenology$treat_competition, sep = "_")


# filter only NOR ---------------------------------------------------------
phenology_NOR <- phenology |> 
  filter(region == "NOR")

# filter only CHE ---------------------------------------------------------
phenology_CHE <- phenology |> 
  filter(region == "CHE")


# NOR ---------------------------------------------------------------------
# should we only use high site here? to test effect of warming?


phenology_NOR_summary <- phenology_NOR  |> 
  group_by(site, date_measurement, treat_warming, treat_competition, phenology_stage) |>
  summarise(mean_abundance = mean(value, na.rm = TRUE),
            sd_abundance = sd(value, na.rm = TRUE),
            n = n()) |>
  ungroup()

phenology_NOR_summary |>
  count(site, block_ID, date_measurement, phenology_stage, treat_warming)




























# again -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

# Step 0: check raw data
head(phenology_NOR)

# Step 1: Median per block × treatment × date × stage
# Ensure each block × treatment × date × stage has ONE value
pheno_block <- phenology_NOR |>
  group_by(site, block_ID, date_measurement, phenology_stage,
           treat_warming, treat_competition) |>
  summarise(median_abundance = median(value, na.rm = TRUE),
            .groups = "drop")

# Check for duplicates (should be zero)
pheno_block |>
  count(site, block_ID, date_measurement, phenology_stage,
        treat_warming, treat_competition) |>
  filter(n > 1)

# Step 2: Pivot wider to calculate warming effect
# Keep only complete block × date × stage × competition combinations
effect_block <- pheno_block |>
  pivot_wider(
    id_cols = c(site, block_ID, date_measurement, phenology_stage, treat_competition),
    names_from = treat_warming,
    values_from = median_abundance
  ) |>
  filter(!is.na(warm) & !is.na(ambi)) |>  # only rows with both treatments
  mutate(effect_size = warm - ambi)

# Step 3: Average across blocks for plotting
effect_summary <- effect_block |>
  group_by(site, date_measurement, phenology_stage, treat_competition) |>
  summarise(
    mean_effect = mean(effect_size, na.rm = TRUE),
    se_effect   = sd(effect_size, na.rm = TRUE) / sqrt(n()),
    n_blocks    = n(),       # how many blocks contributed
    .groups = "drop"
  )

# Step 4: Plot
ggplot(effect_summary,
       aes(x = date_measurement, y = mean_effect,
           color = phenology_stage, group = phenology_stage)) +
  #geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_effect - se_effect,
                    ymax = mean_effect + se_effect),
                width = 0.2) +
  facet_wrap(~treat_competition) +
  theme_minimal() +
  labs(
    y = "Effect size (Warm − Ambient)",
    x = "Date",
    color = "Phenology Stage"
  )















