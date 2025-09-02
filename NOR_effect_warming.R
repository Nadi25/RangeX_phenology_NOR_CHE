

# Effect of warming -------------------------------------------------------


# load library ------------------------------------------------------------
library(lme4)
library(DHARMa)
library(glmmTMB)
library(ggeffects)
library(broom.mixed)

theme_set(theme_bw(base_size = 20))

source("RangeX_phenology_NOR_CHE_data_combination.R")

names(phenology)

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
ggplot(effect_size, aes(x = group, y = effect_warming, fill = group)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(
    x = "Competition treatment",
    y = "Effect of warming (days advanced)"
  )




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





































