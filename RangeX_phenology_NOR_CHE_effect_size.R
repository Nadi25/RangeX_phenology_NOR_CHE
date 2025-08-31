
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
library(dplyr)

# 1. Filter only the high site (where warming exists)
phenology_hi <- phenology_NOR %>%
  filter(site == "hi")

# 2. Summarise within each plot (species x block x treatment x date)
#    Here: take the mean number of structures across individuals
phenology_summary <- phenology_hi %>%
  group_by(block_ID, treat_competition, treat_warming, date_measurement, phenology_stage) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

# 3. Reshape to have Warm and Ambient side by side within each block/competition/species/date
phenology_wide <- phenology_summary %>%
  tidyr::pivot_wider(
    names_from = treat_warming,
    values_from = mean_value
  )

# 4. Calculate warming effect (Warm - Ambient) per block/species/date/stage
phenology_effect <- phenology_wide %>%
  mutate(effect_size = warm - ambi)

# 5. Average across blocks (with SE for error bars)
phenology_effect_summary <- phenology_effect %>%
  group_by(treat_competition,  date_measurement, phenology_stage) %>%
  summarise(
    mean_effect = mean(effect_size, na.rm = TRUE),
    se_effect   = sd(effect_size, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# 6. plot
ggplot(phenology_effect_summary,
       aes(x = mean_effect, 
           y = treat_competition,
           color = phenology_stage)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = mean_effect - se_effect,
                     xmax = mean_effect + se_effect),
                 height = 0.2,
                 position = position_dodge(width = 0.5)) +
  facet_wrap(~ date_measurement, ncol = 3) +
  labs(x = "Effect size (Warm - Ambient)",
       y = "Competition treatment",
       color = "Phenology Stage")





# average over the season -------------------------------------------------

phenology_summary <- phenology_hi %>%
  group_by(block_ID, treat_competition, treat_warming, phenology_stage) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

# 3. Reshape to have Warm and Ambient side by side within each block/competition/species/date
phenology_wide <- phenology_summary %>%
  tidyr::pivot_wider(
    names_from = treat_warming,
    values_from = mean_value
  )

# 4. Calculate warming effect (Warm - Ambient) per block/species/date/stage
phenology_effect <- phenology_wide %>%
  mutate(effect_size = warm - ambi)

# 5. Average across blocks (with SE for error bars)
phenology_effect_summary <- phenology_effect %>%
  group_by(treat_competition, phenology_stage) %>%
  summarise(
    mean_effect = mean(effect_size, na.rm = TRUE),
    se_effect   = sd(effect_size, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# 6. Plot: effect size of warming
ggplot(phenology_effect_summary,
       aes(x = mean_effect, 
           y = treat_competition,
           color = phenology_stage)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = mean_effect - se_effect,
                     xmax = mean_effect + se_effect),
                 height = 0.2,
                 position = position_dodge(width = 0.5)) +
  #facet_wrap(~ date_measurement, ncol = 3) +
  labs(x = "Effect size (Warm - Ambient)",
       y = "Competition treatment",
       color = "Phenology Stage")



















