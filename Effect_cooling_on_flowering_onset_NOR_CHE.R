


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
  scale_color_manual(values = c("#CD950C", "#528B8B"))
che_nor_plot


ggsave(filename = "Output/Flowering_onset_CHE_NOR_cooling_effect_delta_days.png", plot = che_nor_plot, width = 12, height = 8, units = "in")






















































