


# Effect of transplantation on flowering onset ---------------------------------------

## Data used: RangeX_clean_Phenology_2022_CHE.csv
##            RangeX_clean_phenology_2023_NOR.csv
##            RangeX_clean_MetadataFocal_CHE.csv
##            RangeX_metadata_focal_NOR.csv
## Date:      02.09.25
## Author:    Nadine Arzt
## Purpose:   Effect of transplantation on flowering onset NOR and CHE


# load library ------------------------------------------------------------
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


# filter only NOR ---------------------------------------------------------
phenology_NOR <- phenology |> 
  filter(region == "NOR")


# filter only hi ambi and lo -----------------------------------------------
phenology_NOR <- phenology_NOR |> 
  filter(treat_warming == "ambi")


# julian days -------------------------------------------------------------
phenology_NOR$jday <- as.numeric(phenology_NOR$date_measurement)  # converts to days
phenology_NOR$jday_scaled <- scale(phenology_NOR$jday)

# calculate flowering onset ------------------------------------------------
flowering_onset <- phenology_NOR |> 
  filter(phenology_stage == "No_FloOpen", value > 0) |>
  group_by(site, species, unique_plant_ID, block_ID, treat_competition) |>
  summarise(onset = min(jday, na.rm = TRUE), .groups = "drop") |>
  filter(is.finite(onset))

# model flowering onset nor lmer ------------------------------------------
m_onset_cooling <- lmerTest::lmer(onset ~ site * treat_competition + (1|species) + (1|block_ID),
                          data = flowering_onset)
summary(m_onset_cooling)

# plot point with residuals for effect of warming on flo onset -------------
# estimated marginal means
emm_nor_col <- emmeans(m_onset_cooling, ~ site | treat_competition)

# compute contrasts (warm - ambi) within each competition level
#contr_nor_col <- contrast(emm_nor_col, method = "revpairwise")  
contr_nor_col <- contrast(emm_nor_col, method = list("lo - hi" = c(1, -1)))


# using summary keeps the p-values
contrast_df_nor_col <- as.data.frame(summary(contr_nor_col, infer = TRUE))


# plot delta of shift in flowering onset due to warming with significances
nor_effect_col <- ggplot(contrast_df_nor_col, aes(x = treat_competition, y = estimate, color = treat_competition)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = ifelse(p.value < 0.001, "***",
                               ifelse(p.value < 0.01, "**",
                                      ifelse(p.value < 0.05, "*", "n.s.")))),
            vjust = -23, show.legend = FALSE) +
  labs(x = "",
       y = "Δ days shifted flowering onset (hi-lo)",
       title = "Effect of cooling on flowering onset - NOR")
nor_effect_col




# CHE ---------------------------------------------------------------------

# filter only CHE ---------------------------------------------------------
phenology_CHE <- phenology |> 
  filter(region == "CHE")


# filter only hi ambi and lo -----------------------------------------------
phenology_CHE <- phenology_CHE |> 
  filter(treat_warming == "ambi")


# julian days -------------------------------------------------------------
phenology_CHE$jday <- as.numeric(phenology_CHE$date_measurement)  # converts to days
phenology_CHE$jday_scaled <- scale(phenology_CHE$jday)

# calculate flowering onset ------------------------------------------------
flowering_onset_che <- phenology_CHE |> 
  filter(phenology_stage == "No_FloOpen", value > 0) |>
  group_by(site, species, unique_plant_ID, block_ID, treat_competition) |>
  summarise(onset = min(jday, na.rm = TRUE), .groups = "drop") |>
  filter(is.finite(onset))

# model flowering onset nor lmer ------------------------------------------
m_onset_cooling_che <- lmerTest::lmer(onset ~ site * treat_competition + (1|species) + (1|block_ID), data = flowering_onset_che)
summary(m_onset_cooling)

# plot point with residuals for effect of warming on flo onset -------------
# estimated marginal means
emm_che_col <- emmeans(m_onset_cooling_che, ~ site | treat_competition)

# compute contrasts (warm - ambi) within each competition level
#contr_che_col <- contrast(emm_che_col, method = "revpairwise")  
contr_che_col <- contrast(emm_che_col, method = list("lo - hi" = c(1, -1)))


# using summary keeps the p-values
contrast_df_che_col <- as.data.frame(summary(contr_che_col, infer = TRUE))


# plot delta of shift in flowering onset due to warming with significances
che_effect_col <- ggplot(contrast_df_che_col, aes(x = treat_competition, y = estimate, color = treat_competition)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = ifelse(p.value < 0.001, "***",
                               ifelse(p.value < 0.01, "**",
                                      ifelse(p.value < 0.05, "*", "n.s.")))),
            vjust = -23, show.legend = FALSE) +
  labs(x = "",
       y = "Δ days shifted flowering onset (hi - lo)",
       title = "Effect of cooling on flowering onset - NOR")
che_effect_col


# have one plot for both regions ------------------------------------------
# add region
contrast_df_nor_col$region <- "NOR"
contrast_df_che_col$region <- "CHE"

# combine
all_contrasts <- bind_rows(contrast_df_nor_col, contrast_df_che_col)

all_contrasts


# final plot NOR and CHE -----------------------------------------------
che_nor_plot <- ggplot(all_contrasts, aes(x = region, y = estimate, color = treat_competition)) +
  geom_point(position = position_dodge(width = 0.3), size = 11) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                linewidth = 1,
                width = 0.3, position = position_dodge(width = 0.3)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = ifelse(p.value < 0.001, "***",
                               ifelse(p.value < 0.01, "**",
                                      ifelse(p.value < 0.05, "*", "n.s.")))),
            position = position_dodge(width = 0.6),
            vjust = -1.2, show.legend = FALSE,
            size = 6) +
  labs(x = "Region",
       y = "Δ days shifted flowering onset (low - high)",
       title = "Effect of cooling on flowering onset - CHE vs NOR",
       color = "Competition treatment")+
  scale_color_manual(values = c("#CD950C", "#528B8B"))+
  coord_cartesian(ylim = c(-2, 30))
che_nor_plot


ggsave(filename = "Output/Flowering_onset_CHE_NOR_cooling_effect_delta_days.png", plot = che_nor_plot, width = 12, height = 8, units = "in")






# NOR and CHE together ---------------------------------------------------

# filter only hi ambi and lo -----------------------------------------------
phenology_cool <- phenology |> 
  filter(treat_warming == "ambi")


# julian days -------------------------------------------------------------
phenology_cool$jday <- as.numeric(phenology_cool$date_measurement)  # converts to days
phenology_cool$jday_scaled <- scale(phenology_cool$jday)

# calculate flowering onset ------------------------------------------------
flowering_onset_n_c_cool <- phenology_cool |> 
  filter(phenology_stage == "No_FloOpen", value > 0) |>
  group_by(region, site, species, unique_plant_ID, block_ID, treat_competition) |>
  summarise(onset = min(jday, na.rm = TRUE), .groups = "drop") |>
  # remove groups where flowering never occurred
  filter(is.finite(onset))

# model with region for flowering onset lmer ----------------------------------
m_onset_n_c_cooling <- lmerTest::lmer(onset ~ region * site * treat_competition + (1|species) + (1|block_ID), data = flowering_onset_n_c_cool)

summary(m_onset_cooling)



# get emmeans for warming within each region × competition
emm_n_c_cool <- emmeans(m_onset_n_c_cooling, ~ site | region * treat_competition)
# this calculates marginal means per site
# for each region * competition combination, we have one mean for low and high that we can then compare


# compute contrasts (high - low) within each competition level
contr_n_c_cool<- contrast(emm_n_c_cool, method = list("hi - lo" = c(1, -1)))


# using summary keeps the p-values
contrast_df_n_c_cool <- as.data.frame(summary(contr_n_c_cool, infer = TRUE))


# plot delta of shift in flowering onset due to warming with significances
nor_che_effect_col <- ggplot(contrast_df_n_c_cool, 
       aes(x = region, y = estimate, color = treat_competition)) +
  geom_point(size = 11, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), linewidth = 1,
                width = 0.3, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = ifelse(p.value < 0.001, "***",
                               ifelse(p.value < 0.01, "**",
                                      ifelse(p.value < 0.05, "*", "n.s.")))),
            vjust = -1.3, position = position_dodge(width = 0.9), show.legend = FALSE, size = 6) +
  labs(x = "Region",
       y = "Δ days shifted flowering onset (hi - lo)",
       title = "Effect of transplantation (cooling) on flowering onset across regions",
       color = "Competition treatment") +
  scale_color_manual(values = c("#528B8B", "#CD950C"))
nor_che_effect_col










# with raw data points ----------------------------------------------------
# compute mean onset per treatment × group
onset_means_cool <- flowering_onset_n_c_cool |>
  group_by(region, site, species, block_ID, treat_competition) |>
  summarise(mean_onset = mean(onset, na.rm = TRUE), .groups = "drop")

# pivot to get ambi vs warm in same row
delta_onset_cool <- onset_means_cool |>
  pivot_wider(names_from = site, values_from = mean_onset) |>
  mutate(delta = hi - lo) |>
  filter(!is.na(delta))

# check result
head(delta_onset_cool)


# plot raw deltas + model estimates
nor_che_delta_raw_cool <- ggplot() +
  # raw deltas (jittered for visibility)
  geom_jitter(data = delta_onset_cool,
              aes(x = region, y = delta, color = treat_competition),
              width = 0.1, alpha = 0.4, size = 2) +
  
  # model-based warming effects
  geom_point(data = contrast_df_n_c_cool, 
             aes(x = region, y = estimate, color = treat_competition),
             size = 6, position = position_dodge(width = 0.5)) +
  geom_errorbar(data = contrast_df_n_c_cool,
                aes(x = region, ymin = lower.CL, ymax = upper.CL, color = treat_competition),
                linewidth = 1, width = 0.1,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  # significance labels
  geom_text(data = contrast_df_n_c_cool,
            aes(x = region, y = estimate, 
                label = ifelse(p.value < 0.001, "***",
                               ifelse(p.value < 0.01, "**",
                                      ifelse(p.value < 0.05, "*", "n.s."))),
                color = treat_competition),
            vjust = -2, position = position_dodge(width = 0.7),
            show.legend = FALSE, size = 12) +
  
  labs(x = "Region",
       y = "Δ days shifted flowering onset (high - low)",
       title = "Effect of cooling through transplantation on flowering onset across regions",
       color = "Competition treatment") +
  scale_color_manual(values = c("#528B8B", "#CD950C"))
nor_che_delta_raw_cool


ggsave(filename = "Output/Flowering_onset_joined_model_CHE_NOR_cooling_effect_delta_and_raw.png", plot = nor_che_delta_raw_cool, width = 12, height = 8, units = "in")




























