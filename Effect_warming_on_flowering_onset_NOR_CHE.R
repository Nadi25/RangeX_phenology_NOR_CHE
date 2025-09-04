

# Effect of warming on flowering onset ---------------------------------------

# RangeX phenology effect of warming NOR and CHE ------------

## Data used: RangeX_clean_Phenology_2022_CHE.csv
##            RangeX_clean_phenology_2023_NOR.csv
##            RangeX_clean_MetadataFocal_CHE.csv
##            RangeX_metadata_focal_NOR.csv
## Date:      02.09.25
## Author:    Nadine Arzt
## Purpose:   Effect of warming on flowering onset NOR and CHE


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


# filter only high site --------------------------------------------------------
phenology_NOR_hi <- phenology_NOR |> 
  filter(site == "hi")


# julian days -------------------------------------------------------------
phenology_NOR_hi$jday <- as.numeric(phenology_NOR_hi$date_measurement)  # converts to days
phenology_NOR_hi$jday_scaled <- scale(phenology_NOR_hi$jday)


# split by phenology stage ------------------------------------------------
# buds
phenology_NOR_hi_buds <- subset(phenology_NOR_hi, phenology_stage == "No_Buds")

# flowers
phenology_NOR_hi_flowers <- subset(phenology_NOR_hi, phenology_stage == "No_FloOpen")

# infructescences
phenology_NOR_hi_infructescences <- subset(phenology_NOR_hi, phenology_stage == "number_infructescences")



# calculate flowering onset ------------------------------------------------
flowering_onset <- phenology_NOR_hi |> 
  filter(phenology_stage == "No_FloOpen", value > 0) |>
  group_by(species, unique_plant_ID, block_ID, treat_warming, treat_competition) |>
  summarise(onset = min(jday, na.rm = TRUE), .groups = "drop") |>
  # remove groups where flowering never occurred
  filter(is.finite(onset))


# model flowering onset nor lmer ------------------------------------------
m_onset <- lmerTest::lmer(onset ~ treat_warming * treat_competition + (1|species) + (1|block_ID),
                data = flowering_onset)
summary(m_onset)

# predicted onset for each warming x competition combination
pred_onset <- ggpredict(m_onset, terms = c("treat_warming", "treat_competition"))

# inspect
head(pred_onset)


ggplot(pred_onset, aes(x = x, y = predicted, color = group)) +
  geom_point(position = position_dodge(width = 0.2), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1, position = position_dodge(width = 0.2)) +
  theme_minimal() +
  labs(
    x = "Warming treatment",
    y = "Predicted flowering onset (Julian day)",
    color = "Competition"
  )


# convert ggpredict output to data frame
pred_df <- as.data.frame(pred_onset)

# separate ambient and warm predictions and join by competition
ambi <- pred_df |>  
  filter(x == "ambi") |> 
  select(group, predicted) |> 
  rename(ambi = predicted)
warm <- pred_df |>  
  filter(x == "warm") |> 
  select(group, predicted) |> 
  rename(warm = predicted)

effect_size <- left_join(ambi, warm, by = "group") |> 
  mutate(effect_warming = warm - ambi)

# plot effect size
effect_warming_nor <- ggplot(effect_size, aes(x = group, y = effect_warming, fill = group)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Competition treatment",
    y = "Effect of warming (days advanced)"
  )
effect_warming_nor

ggsave(filename = "Output/Flowering_onset_NOR_warming_effect_days.png", plot = effect_warming_nor, width = 12, height = 8, units = "in")


ggplot(effect_size, aes(x = group, y = effect_warming, color = group)) +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = effect_warming - ambi, ymax = effect_warming + ambi),
                width = 0.1, position = position_dodge(width = 0.3)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Competition treatment", 
       y = "Effect of warming (Δ days flowering onset)") +
  theme_minimal()


# Tidy the model output
fixed_effects <- broom.mixed::tidy(m_onset, effects = "fixed")

# Add significance labels
fixed_effects <- fixed_effects |> 
  mutate(signif = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    p.value < 0.1 ~ ".",
    TRUE ~ ""
  ))

# Filter out intercept for plotting
fixed_effects_plot <- fixed_effects |> 
  filter(term != "(Intercept)")

# Plot
effect <- ggplot(fixed_effects_plot, aes(x = term, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = signif), vjust = -4, size = 6) +
  labs(
    x = "Fixed Effect",
    y = "Shift (days)",
    title = "Effect of warming on flowering onset - NOR")
effect    

ggsave(filename = "Output/Flowering_onset_NOR_warming_effect.png", plot = effect, width = 12, height = 8, units = "in")

###########################################

# plot effect of warming with std error -----------------------------------
# get both estimate and SE
ambi <- pred_df |>  
  filter(x == "ambi") |> 
  select(group, predicted, std.error) |> 
  rename(ambi = predicted, se_ambi = std.error)

warm <- pred_df |>  
  filter(x == "warm") |> 
  select(group, predicted, std.error) |> 
  rename(warm = predicted, se_warm = std.error)

# join and compute effect + propagated SE
effect_size <- left_join(ambi, warm, by = "group") |> 
  mutate(
    effect_warming = warm - ambi,
    SE = sqrt(se_warm^2 + se_ambi^2),  # error propagation
    lower = effect_warming - 1.96 * SE,
    upper = effect_warming + 1.96 * SE
  )


ggplot(effect_size, aes(x = group, y = effect_warming, color = group)) +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.1, position = position_dodge(width = 0.3)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Competition treatment", 
       y = "Effect of warming (Δ days flowering onset)")


# plot for presentation nor -----------------------------------------------
# plot point with residuals for effect of warming on flo onset -------------
# estimated marginal means
emm_nor <- emmeans(m_onset, ~ treat_warming | treat_competition)

# compute contrasts (warm - ambi) within each competition level
contr_nor <- contrast(emm_nor, method = "revpairwise")  
contr_df_nor <- as.data.frame(confint(contr_nor))

# using summary keeps the p-values
contrast_df_nor <- as.data.frame(summary(contr_nor, infer = TRUE))


# plot delta of shift in flowering onset due to warming with significances
nor_effect <- ggplot(contrast_df_nor, aes(x = treat_competition, y = estimate, color = treat_competition)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = ifelse(p.value < 0.001, "***",
                               ifelse(p.value < 0.01, "**",
                                      ifelse(p.value < 0.05, "*", "n.s.")))),
            vjust = -23, show.legend = FALSE) +
  labs(x = "",
       y = "Δ days shifted flowering onset (warm - ambi)",
       title = "Effect of warming on flowering onset - NOR")
nor_effect

ggsave(filename = "Output/Flowering_onset_NOR_warming_effect_delta_days.png", plot = nor_effect, width = 12, height = 8, units = "in")

contrast_df_nor
# treat_competition = bare:
#   contrast     estimate       SE     df  lower.CL   upper.CL t.ratio p.value
# warm - ambi -3.614315 1.059456 691.57 -5.694451 -1.5341793  -3.411  0.0007
# 
# treat_competition = vege:
#   contrast     estimate       SE     df  lower.CL   upper.CL t.ratio p.value
# warm - ambi -2.996362 1.201769 692.15 -5.355912 -0.6368127  -2.493  0.0129







# CHE ---------------------------------------------------------------------

# filter only CHE ---------------------------------------------------------
phenology_CHE <- phenology |> 
  filter(region == "CHE")


# filter only high site --------------------------------------------------------
phenology_CHE_hi <- phenology_CHE |> 
  filter(site == "hi")


# julian days -------------------------------------------------------------

phenology_CHE_hi$jday <- as.numeric(phenology_CHE_hi$date_measurement)  # converts to days
phenology_CHE_hi$jday_scaled <- scale(phenology_CHE_hi$jday)



# split by phenology stage ------------------------------------------------
# buds
phenology_CHE_hi_buds <- subset(phenology_CHE_hi, phenology_stage == "No_Buds")

# flowers
phenology_CHE_hi_flowers <- subset(phenology_CHE_hi, phenology_stage == "No_FloOpen")

# infructescences
phenology_CHE_hi_infructescences <- subset(phenology_CHE_hi, phenology_stage == "number_infructescences")




# calculate flowering onset ------------------------------------------------
flowering_onset_CHE <- phenology_CHE_hi |> 
  filter(phenology_stage == "No_FloOpen", value > 0) |>
  group_by(species, unique_plant_ID, block_ID, treat_warming, treat_competition) |>
  summarise(onset = min(jday, na.rm = TRUE), .groups = "drop") |>
  # remove groups where flowering never occurred
  filter(is.finite(onset))


# model flowering onset che lmer ------------------------------------------
m_onset_che <- lmerTest::lmer(onset ~ treat_warming * treat_competition + (1|species) + (1|block_ID),
                          data = flowering_onset_CHE)
summary(m_onset_che)

# predicted onset for each warming x competition combination
pred_onset_che <- ggpredict(m_onset_che, terms = c("treat_warming", "treat_competition"))

# inspect
head(pred_onset_che)


ggplot(pred_onset_che, aes(x = x, y = predicted, color = group)) +
  geom_point(position = position_dodge(width = 0.2), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1, position = position_dodge(width = 0.2)) +
  theme_minimal() +
  labs(
    x = "Warming treatment",
    y = "Predicted flowering onset (Julian day)",
    color = "Competition"
  )


# convert ggpredict output to data frame
pred_df_che <- as.data.frame(pred_onset_che)

# separate ambient and warm predictions and join by competition
ambi_che <- pred_df_che |>  
  filter(x == "ambi") |> 
  select(group, predicted) |> 
  rename(ambi = predicted)
warm_che <- pred_df_che |>  
  filter(x == "warm") |> 
  select(group, predicted) |> 
  rename(warm = predicted)

effect_size_che <- left_join(ambi_che, warm_che, by = "group") |> 
  mutate(effect_warming = warm - ambi)

# plot effect size
effect_warming_che <- ggplot(effect_size_che, aes(x = group, y = effect_warming, fill = group)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Competition treatment",
    y = "Effect of warming (days advanced)"
  )
effect_warming_che

ggsave(filename = "Output/Flowering_onset_CHE_warming_effect_days.png", plot = effect_warming_che, width = 12, height = 8, units = "in")


# plot effect of warming with std error -----------------------------------
# get both estimate and SE
ambi_che <- pred_df_che |>  
  filter(x == "ambi") |> 
  select(group, predicted, std.error) |> 
  rename(ambi = predicted, se_ambi = std.error)

warm_che <- pred_df_che |>  
  filter(x == "warm") |> 
  select(group, predicted, std.error) |> 
  rename(warm = predicted, se_warm = std.error)

# join and compute effect + propagated SE
effect_size_che <- left_join(ambi_che, warm_che, by = "group") |> 
  mutate(
    effect_warming = warm - ambi,
    SE = sqrt(se_warm^2 + se_ambi^2),  # error propagation
    lower = effect_warming - 1.96 * SE,
    upper = effect_warming + 1.96 * SE
  )


ggplot(effect_size_che, aes(x = group, y = effect_warming, color = group)) +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.1, position = position_dodge(width = 0.3)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Competition treatment", 
       y = "Effect of warming (Δ days flowering onset)")





# Tidy the model output
fixed_effects_che <- broom.mixed::tidy(m_onset_che, effects = "fixed")

# Add significance labels
fixed_effects_che <- fixed_effects_che |> 
  mutate(signif = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    p.value < 0.1 ~ ".",
    TRUE ~ ""
  ))

# Filter out intercept for plotting
fixed_effects_che_plot <- fixed_effects_che |> 
  filter(term != "(Intercept)")

# Plot
effect_che <- ggplot(fixed_effects_che_plot, aes(x = term, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = signif), vjust = -4, size = 6) +
  labs(
    x = "Fixed Effect",
    y = "Shift (days)",
    title = "Effect of warming on flowering onset - CHE")
effect_che  

ggsave(filename = "Output/Flowering_onset_CHE_warming_effect.png", plot = effect_che, width = 12, height = 8, units = "in")



# plot for presentation che -----------------------------------------------
# plot point with residuals for effect of warming on flo onset -------------

# estimated marginal means
emm_che <- emmeans(m_onset_che, ~ treat_warming | treat_competition)

# compute contrasts (warm - ambi) within each competition level
contr_che <- contrast(emm_che, method = "revpairwise")  
contr_df_che <- as.data.frame(confint(contr_che))


# plot
che_effect <- ggplot(contr_df_che, aes(x = treat_competition, y = estimate, color = treat_competition)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "",
       y = "Δ days shifted flowering onset",
       title = "Effect of warming on flowering onset - CHE")

# using summary keeps the p-values
contrast_df_che <- as.data.frame(summary(contr_che, infer = TRUE))

# bare soil:
# Effect of warming = –6.37 days
# SE = 1.35 → narrow confidence interval
# Test: t = –4.71, p < 0.0001 → highly significant advance in flowering.
# 
# with vegetation (vege):
# Effect of warming = –7.48 days
# SE = 3.95 → much wider confidence interval
# Test: t = –1.89, p = 0.059 → marginal, not conventionally significant.


che_effect <- ggplot(contrast_df_che, aes(x = treat_competition, y = estimate, color = treat_competition)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = ifelse(p.value < 0.001, "***",
                               ifelse(p.value < 0.01, "**",
                                      ifelse(p.value < 0.05, "*", "n.s.")))),
            vjust = -16, show.legend = FALSE) +
  labs(x = "",
       y = "Δ days shifted flowering onset (warm - ambi)",
       title = "Effect of warming on flowering onset - CHE")
che_effect


ggsave(filename = "Output/Flowering_onset_CHE_warming_effect_delta_days.png", plot = che_effect, width = 12, height = 8, units = "in")







# have one plot for both regions ------------------------------------------
# add region
contrast_df_nor$region <- "NOR"
contrast_df_che$region <- "CHE"

# combine
all_contrasts <- bind_rows(contrast_df_nor, contrast_df_che)

all_contrasts
# treat_competition = bare:
#   contrast     estimate       SE     df   lower.CL  upper.CL t.ratio p.value region
# warm - ambi -3.614315 1.059456 691.57  -5.694451 -1.534179  -3.411  0.0007 NOR   
# warm - ambi -6.372056 1.351822 422.78  -9.029185 -3.714927  -4.714  <.0001 CHE   
# 
# treat_competition = vege:
#   contrast     estimate       SE     df   lower.CL  upper.CL t.ratio p.value region
# warm - ambi -2.996362 1.201769 692.15  -5.355912 -0.636813  -2.493  0.0129 NOR   
# warm - ambi -7.482268 3.951383 424.57 -15.248977  0.284440  -1.894  0.0590 CHE  


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
       y = "Δ days shifted flowering onset (warm - ambi)",
       title = "Effect of warming on flowering onset - CHE vs NOR",
       color = "Competition treatment")+
  scale_color_manual(values = c("#CD950C", "#528B8B"))
che_nor_plot

ggsave(filename = "Output/Flowering_onset_CHE_NOR_warming_effect_delta_days.png", plot = che_nor_plot, width = 12, height = 8, units = "in")





ggplot(all_contrasts, aes(x = region, y = estimate, color = treat_competition)) +
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
       y = "Δ days shifted flowering onset (warm - ambi)",
       title = "Effect of warming on flowering onset - CHE vs NOR",
       color = "Competition treatment")+
  scale_color_manual(values = c("#CD950C", "#528B8B"))+
  facet_wrap(~ species)



# NOR and CHE together ----------------------------------------------------
# filter only high site --------------------------------------------------------
phenology_hi <- phenology |> 
  filter(site == "hi")

# julian days -------------------------------------------------------------
phenology_hi$jday <- as.numeric(phenology_hi$date_measurement)  # converts to days
phenology_hi$jday_scaled <- scale(phenology_hi$jday)

# calculate flowering onset ------------------------------------------------
flowering_onset_n_c <- phenology_hi |> 
  filter(phenology_stage == "No_FloOpen", value > 0) |>
  group_by(region, species, unique_plant_ID, block_ID, treat_warming, treat_competition) |>
  summarise(onset = min(jday, na.rm = TRUE), .groups = "drop") |>
  # remove groups where flowering never occurred
  filter(is.finite(onset))


# model flowering onset lmer ------------------------------------------
m_onset_n_c <- lmerTest::lmer(onset ~ region * treat_warming * treat_competition + (1|species) + (1|block_ID),
                          data = flowering_onset_n_c)
summary(m_onset_n_c)


# get emmeans for warming within each region × competition
emm_n_c_col <- emmeans(m_onset_n_c, ~ treat_warming | region * treat_competition)

# compute warming effect (warm - ambi) in each region × competition
contr_warm <- contrast(emm_n_c_col, method = "revpairwise", by = c("region", "treat_competition"))

# now compare warming effects across regions
region_diff <- contrast(contr_warm, by = "treat_competition", method = "revpairwise")

summary(region_diff, infer = TRUE)

# get warming effects per region × competition
contr_warm <- contrast(emm_n_c_col, method = "revpairwise", 
                       by = c("region", "treat_competition"))

contrast_df_nor_che <- as.data.frame(summary(contr_warm, infer = TRUE))

# plot warming effects side by side for each region
nor_che_effect <- ggplot(contrast_df_nor_che, 
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
       y = "Δ days shifted flowering onset (warm - ambi)",
       title = "Effect of warming on flowering onset across regions",
       color = "Competition treatment") +
  scale_color_manual(values = c("#CD950C", "#528B8B"))
nor_che_effect

ggsave(filename = "Output/Flowering_onset_joined_model_CHE_NOR_warming_effect_delta_days.png", plot = nor_che_effect, width = 12, height = 8, units = "in")







# with raw data points ----------------------------------------------------
# compute mean onset per treatment × group
onset_means <- flowering_onset_n_c |>
  group_by(region, species, block_ID, treat_competition, treat_warming) |>
  summarise(mean_onset = mean(onset, na.rm = TRUE), .groups = "drop")

# pivot to get ambi vs warm in same row
delta_onset <- onset_means |>
  pivot_wider(names_from = treat_warming, values_from = mean_onset) |>
  mutate(delta = warm - ambi) |>
  filter(!is.na(delta))

# check result
head(delta_onset)


# plot raw deltas + model estimates
nor_che_delta_raw <- ggplot() +
  # raw deltas (jittered for visibility)
  geom_jitter(data = delta_onset,
              aes(x = region, y = delta, color = treat_competition),
              width = 0.1, alpha = 0.4, size = 2) +
  
  # model-based warming effects
  geom_point(data = contrast_df_nor_che, 
             aes(x = region, y = estimate, color = treat_competition),
             size = 6, position = position_dodge(width = 0.5)) +
  geom_errorbar(data = contrast_df_nor_che,
                aes(x = region, ymin = lower.CL, ymax = upper.CL, color = treat_competition),
                linewidth = 1, width = 0.1,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  # significance labels
  geom_text(data = contrast_df_nor_che,
            aes(x = region, y = estimate, 
                label = ifelse(p.value < 0.001, "***",
                               ifelse(p.value < 0.01, "**",
                                      ifelse(p.value < 0.05, "*", "n.s."))),
                color = treat_competition),
            vjust = -2, position = position_dodge(width = 0.7),
            show.legend = FALSE, size = 6) +
  
  labs(x = "Region",
       y = "Δ days shifted flowering onset (warm − ambi)",
       title = "Effect of warming on flowering onset across regions",
       color = "Competition treatment") +
  scale_color_manual(values = c("#CD950C", "#528B8B"))
nor_che_delta_raw


ggsave(filename = "Output/Flowering_onset_joined_model_CHE_NOR_warming_effect_delta_and_raw.png", plot = nor_che_delta_raw, width = 12, height = 8, units = "in")

# 69 days difference between the cenjac plants in warm - ambi
# CHE.hi.warm.vege.wf.10.14.1
# CHE.hi.ambi.vege.wf.10.25.1





















