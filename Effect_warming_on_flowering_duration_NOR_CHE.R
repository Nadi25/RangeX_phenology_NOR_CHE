
# Effect of warming on flowering duration ---------------------------------------

# RangeX phenology effect of warming NOR and CHE ------------

## Data used: RangeX_clean_Phenology_2022_CHE.csv
##            RangeX_clean_phenology_2023_NOR.csv
##            RangeX_clean_MetadataFocal_CHE.csv
##            RangeX_metadata_focal_NOR.csv
## Date:      04.09.25
## Author:    Nadine Arzt
## Purpose:   Effect of warming on flowering duration NOR and CHE


# load library ------------------------------------------------------------
library(lme4)
library(ggeffects)
library(broom.mixed)
library(emmeans)


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

# filter only high site --------------------------------------------------------
phenology_hi <- phenology |> 
  filter(site == "hi")


# julian days -------------------------------------------------------------
phenology_hi$jday <- as.numeric(phenology_hi$date_measurement)  # converts to days
phenology_hi$jday_scaled <- scale(phenology_hi$jday)



# calculate flowering onset ------------------------------------------------
flowering_duration <- phenology_hi |> 
  filter(phenology_stage == "No_FloOpen", value > 0) |>
  group_by(region, species, unique_plant_ID, block_ID, treat_warming, treat_competition) |>
  summarize(
    onset_date = min(jday),
    end_date   = max(jday),     # last day flowering observed
    duration   = as.numeric(max(jday) - min(jday)) + 1, # include both ends
    .groups = "drop")



# model flowering onset nor lmer ------------------------------------------
m_duration <- lmerTest::lmer(duration ~ region * treat_warming * treat_competition + (1|species) + (1|block_ID),
                          data = flowering_duration)
summary(m_duration)

# get emmeans for warming within each region × competition
emm_n_c_dura <- emmeans(m_duration, ~ treat_warming | region * treat_competition)

# compute warming effect (warm - ambi) in each region × competition
contr_warm_dura <- contrast(emm_n_c_dura, method = "revpairwise", by = c("region", "treat_competition"))

# now compare warming effects across regions
region_diff <- contrast(contr_warm_dura, by = "treat_competition", method = "revpairwise")

summary(region_diff, infer = TRUE)

# get warming effects per region × competition
contr_warm_dura <- contrast(emm_n_c_dura, method = "revpairwise", 
                       by = c("region", "treat_competition"))

contrast_duration_nor_che <- as.data.frame(summary(contr_warm_dura, infer = TRUE))


# plot warming effects side by side for each region
nor_che_effect_duration <- ggplot(contrast_duration_nor_che, 
                         aes(x = region, y = estimate, color = treat_competition)) +
  geom_point(size = 11, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), linewidth = 1,
                width = 0.3, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = ifelse(p.value < 0.001, "***",
                               ifelse(p.value < 0.01, "**",
                                      ifelse(p.value < 0.05, "*", "n.s.")))),
            vjust = -2, position = position_dodge(width = 0.7), show.legend = FALSE, size = 6) +
  labs(x = "Region",
       y = "Δ days shifted flowering duration (warm - ambi)",
       title = "Effect of warming on flowering duration across regions",
       color = "Competition treatment") +
  scale_color_manual(values = c("#528B8B", "#CD950C"))
nor_che_effect_duration

ggsave(filename = "Output/Flowering_duration_joined_model_CHE_NOR_warming_effect_delta_days.png", plot = nor_che_effect_duration, width = 12, height = 8, units = "in")



# with raw data points ----------------------------------------------------
# compute mean onset per treatment × group
duration_means <- flowering_duration |>
  group_by(region, species, block_ID, treat_competition, treat_warming) |>
  summarise(mean_duration = mean(duration, na.rm = TRUE), .groups = "drop")

# pivot to get ambi vs warm in same row
delta_duration <- duration_means |>
  pivot_wider(names_from = treat_warming, values_from = mean_duration) |>
  mutate(delta = warm - ambi) |>
  filter(!is.na(delta))

# check result
head(delta_duration)


# plot raw deltas + model estimates
nor_che_delta_raw_duration <- ggplot() +
  # raw deltas (jittered for visibility)
  geom_jitter(data = delta_duration,
              aes(x = region, y = delta, color = treat_competition),
              width = 0.1, alpha = 0.4, size = 2) +
  
  # model-based warming effects
  geom_point(data = contrast_duration_nor_che, 
             aes(x = region, y = estimate, color = treat_competition),
             size = 6, position = position_dodge(width = 0.5)) +
  geom_errorbar(data = contrast_duration_nor_che,
                aes(x = region, ymin = lower.CL, ymax = upper.CL, color = treat_competition),
                linewidth = 1, width = 0.1,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  # significance labels
  geom_text(data = contrast_duration_nor_che,
            aes(x = region, y = estimate, 
                label = ifelse(p.value < 0.001, "***",
                               ifelse(p.value < 0.01, "**",
                                      ifelse(p.value < 0.05, "*", "n.s."))),
                color = treat_competition),
            vjust = -2, position = position_dodge(width = 0.7),
            show.legend = FALSE, size = 12) +
  
  labs(x = "Region",
       y = "Δ days shifted flowering duration (warmed − ambient)",
       title = "Effect of warming on flowering duration across regions",
       color = "Competition treatment") +
  scale_color_manual(values = c("#528B8B", "#CD950C"))
nor_che_delta_raw_duration


ggsave(filename = "Output/Flowering_duration_joined_model_CHE_NOR_warming_effect_delta_and_raw.png", plot = nor_che_delta_raw_duration, width = 12, height = 8, units = "in")





# flowering end -----------------------------------------------------------

# model flowering end nor lmer ------------------------------------------
m_end <- lmerTest::lmer(end_date ~ region * treat_warming * treat_competition + (1|species) + (1|block_ID),
                             data = flowering_duration)
summary(m_end)

# get emmeans for warming within each region × competition
emm_n_c_end <- emmeans(m_end, ~ treat_warming | region * treat_competition)

# compute warming effect (warm - ambi) in each region × competition
contr_warm_end <- contrast(emm_n_c_end, method = "revpairwise", by = c("region", "treat_competition"))

# now compare warming effects across regions
region_diff <- contrast(contr_warm_end, by = "treat_competition", method = "revpairwise")

summary(region_diff, infer = TRUE)

# get warming effects per region × competition
contr_warm_end <- contrast(emm_n_c_end, method = "revpairwise", 
                            by = c("region", "treat_competition"))

contrast_end_nor_che <- as.data.frame(summary(contr_warm_end, infer = TRUE))


# plot warming effects side by side for each region
nor_che_effect_end <- ggplot(contrast_end_nor_che, 
                                  aes(x = region, y = estimate, color = treat_competition)) +
  geom_point(size = 11, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), linewidth = 1,
                width = 0.3, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = ifelse(p.value < 0.001, "***",
                               ifelse(p.value < 0.01, "**",
                                      ifelse(p.value < 0.05, "*", "n.s.")))),
            vjust = -2, position = position_dodge(width = 0.7), show.legend = FALSE, size = 6) +
  labs(x = "Region",
       y = "Δ days shifted flowering end (warm - ambi)",
       title = "Effect of warming on flowering rnd across regions",
       color = "Competition treatment") +
  scale_color_manual(values = c("#528B8B", "#CD950C"))
nor_che_effect_end




# with raw data points ----------------------------------------------------
# compute mean onset per treatment × group
end_means <- flowering_duration |>
  group_by(region, species, block_ID, treat_competition, treat_warming) |>
  summarise(mean_end = mean(end_date, na.rm = TRUE), .groups = "drop")

# pivot to get ambi vs warm in same row
delta_end <- end_means |>
  pivot_wider(names_from = treat_warming, values_from = mean_end) |>
  mutate(delta = warm - ambi) |>
  filter(!is.na(delta))

# check result
head(delta_end)


# plot raw deltas + model estimates
nor_che_delta_raw_end <- ggplot() +
  # raw deltas (jittered for visibility)
  geom_jitter(data = delta_end,
              aes(x = region, y = delta, color = treat_competition),
              width = 0.1, alpha = 0.4, size = 2) +
  
  # model-based warming effects
  geom_point(data = contrast_end_nor_che, 
             aes(x = region, y = estimate, color = treat_competition),
             size = 6, position = position_dodge(width = 0.5)) +
  geom_errorbar(data = contrast_end_nor_che,
                aes(x = region, ymin = lower.CL, ymax = upper.CL, color = treat_competition),
                linewidth = 1, width = 0.1,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  # significance labels
  geom_text(data = contrast_end_nor_che,
            aes(x = region, y = estimate, 
                label = ifelse(p.value < 0.001, "***",
                               ifelse(p.value < 0.01, "**",
                                      ifelse(p.value < 0.05, "*", "n.s."))),
                color = treat_competition),
            vjust = -2, position = position_dodge(width = 0.7),
            show.legend = FALSE, size = 12) +
  
  labs(x = "Region",
       y = "Δ days shifted flowering end (warmed − ambient)",
       title = "Effect of warming on flowering end across regions",
       color = "Competition treatment") +
  scale_color_manual(values = c("#528B8B", "#CD950C"))
nor_che_delta_raw_end


ggsave(filename = "Output/Flowering_end_joined_model_CHE_NOR_warming_effect_delta_and_raw.png", plot = nor_che_delta_raw_end, width = 12, height = 8, units = "in")






