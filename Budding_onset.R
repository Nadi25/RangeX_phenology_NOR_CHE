
# 02.03.26
# Budding, ....


# Effect of transplantation on flowering onset only vege ---------------------------------------

## Data used: RangeX_clean_Phenology_2022_CHE.csv
##            RangeX_clean_phenology_2023_NOR.csv
##            RangeX_clean_MetadataFocal_CHE.csv
##            RangeX_metadata_focal_NOR.csv
## Date:      02.09.25
## Author:    Nadine Arzt
## Purpose:   Effect of transplantation on flowering onset NOR and CHE
##            only with vegetation


# load library ---------------------------------------------------------
library(lme4)
library(ggeffects)
library(broom.mixed)
library(emmeans)
library(lubridate)


# load clean phenology data -----------------------------------------------
source("RangeX_phenology_NOR_CHE_data_combination.R")

# use this data set
names(phenology)

# set theme for plots for presentation ------------------------------------
theme_set(theme_bw(base_size = 20))

# Filter buds, flowers, infructescences -----------------------------------
## don't use seeds_collected
phenology <- phenology |> 
  filter(phenology$phenology_stage != "No_Seeds")


# combined treatment column -----------------------------------------------
phenology$treatment <- paste(phenology$site, phenology$treat_warming, phenology$treat_competition, sep = "_")


# change region and treatment names  --------------------------------------
phenology <- phenology |>
  mutate(region = case_when(
    region == "NOR" ~ "Norway",
    region == "CHE" ~ "Switzerland",
    TRUE ~ region
  ))

phenology <- phenology |>
  mutate(treat_competition = case_when(
    treat_competition == "bare" ~ "without competition",
    treat_competition == "vege" ~ "with competition",
    TRUE ~ treat_competition
  ))



# filter only hi ambi and lo -----------------------------------------------
# and get julian days ---------------------------------------------------
# yday(date)
phenology_cool <- phenology |> 
  filter(treat_warming == "ambi") |> 
  mutate(
    jday = yday(date_measurement),   # Julian day (1–365)
    jday_scaled = scale(jday))        # optional scaling if you need for models 




# # filter only with veg ----------------------------------------------------
# phenology_cool_vege <- phenology_cool |> 
#   filter(treat_competition == "with competition")
# 

# calculate flowering onset ------------------------------------------------
onset_n_c_cool <- phenology_cool |> 
  filter(phenology_stage == "No_Buds", value > 0) |>
  group_by(region, site, treat_competition, species, block_ID, unique_plot_ID, unique_plant_ID, phenology_stage) |>
  summarise(onset = min(jday, na.rm = TRUE), .groups = "drop") |>
  # remove groups where flowering never occurred
  filter(is.finite(onset))

# model with region for flowering onset lmer ----------------------------------
m_onset_n_c_cooling <- lmerTest::lmer(onset ~ region * site * treat_competition + (1|species) + (1|block_ID), 
                                      data = onset_n_c_cool)

summary(m_onset_n_c_cooling)



# emmeans -----------------------------------------------------------------
# get emmeans for warming within each region × competition
# this calculates marginal means per site
emm_n_c_cool <- emmeans(m_onset_n_c_cooling, ~ site | region * treat_competition)



# contrasts high-low ------------------------------------------------------
# compute contrasts (high - low) within each competition level

contr_n_c_cool <- contrast(
  emm_n_c_cool,
  method = list("hi - lo" = c(1, -1)),
  by = c("region", "treat_competition")
)



# using summary keeps the p-values
contrast_df_n_c_cool <- as.data.frame(summary(contr_n_c_cool, infer = TRUE))







# with raw data points ----------------------------------------------------
# compute mean onset per treatment × group
onset_means_cool <- flowering_onset_n_c_cool |>
  group_by(region, site, treat_competition, species, block_ID) |>
  summarise(mean_onset = mean(onset, na.rm = TRUE), .groups = "drop")

# pivot to get ambi vs warm in same row
delta_onset_cool <- onset_means_cool |>
  pivot_wider(names_from = site, values_from = mean_onset) |>
  mutate(delta = hi - lo) |>
  filter(!is.na(delta))

# check result
head(delta_onset_cool)




# plot raw deltas + model estimates ---------------------------------------

nor_che_delta_raw_cool <- ggplot() +
  # raw deltas
  geom_jitter(
    data = delta_onset_cool,
    aes(
      x = region,
      y = delta,
      color = treat_competition
    ),
    width = 0.1, alpha = 0.4, size = 3
  ) +
  
  # model-based contrasts
  geom_point(
    data = contrast_df_n_c_cool,
    aes(
      x = region,
      y = estimate,
      color = treat_competition,
      shape = region
    ),
    size = 8,
    position = position_dodge(width = 0.5)
  ) +
  geom_errorbar(
    data = contrast_df_n_c_cool,
    aes(
      x = region,
      ymin = lower.CL,
      ymax = upper.CL,
      color = treat_competition
    ),
    linewidth = 1,
    width = 0.1,
    position = position_dodge(width = 0.5)
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  # significance labels
  geom_text(
    data = contrast_df_n_c_cool,
    aes(
      x = region,
      y = estimate,
      label = ifelse(p.value < 0.001, "***",
                     ifelse(p.value < 0.01, "**",
                            ifelse(p.value < 0.05, "*", "n.s."))),
      color = treat_competition
    ),
    vjust = -1.5,
    position = position_dodge(width = 0.5),
    show.legend = FALSE,
    size = 10
  ) +
  
  labs(
    x = "Region",
    y = "Δ days shifted budding onset (high - low)",
    color = "Competition treatment",
    shape = "Region",
    title = "Effect of cooling on budding onset across regions"
  ) +
  
  theme(
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20)
  ) +
  
  scale_color_manual(values = c(
    "with competition" = "#528B8B",
    "without competition" = "#CD950C"
  )) +
  
  scale_shape_manual(values = c("Norway" = 16, "Switzerland" = 17))+
  guides(shape = "none")

nor_che_delta_raw_cool



# ggsave(filename = "Output/Budding_onset_joined_model_CHE_NOR_cooling_effect.png", 
#        plot = nor_che_delta_raw_cool, width = 12, height = 8, units = "in")






















































