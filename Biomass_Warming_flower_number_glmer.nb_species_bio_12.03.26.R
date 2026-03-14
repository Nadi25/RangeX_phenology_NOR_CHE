
# BIOMASS 14 ---------------------------------------------------------------


#  final figure of effect of warming on flower number: p_warm_nb_bio ---------------------

# the best model option for effect of warming: m_flower_number_warm_bio_nb_species

# Effect of warming on flower number glmer.nb -------------------------------------
# negative binomial model is better for our data

# uses individual species models for biomass prediction 

# Adding predicted biomass to phenology and fit species models with pred biomass ------------

## Data used: 
## Date:      12.03.26
## Author:    Nadine Arzt
## Purpose:   Does biomass mitigate the effect of warming on no of flowers?



source("Biomass_phenology_combine_species_models.R")


# add predicted species biomass to pheno data -------------------------------------
bio_flower_species_unique <- bio_flower_species |>
  group_by(unique_plant_ID) |>
  summarise(pred_log_biomass_species = first(pred_log_biomass_species),
            .groups = "drop")


phenology_NOR_hi_flowers_bio_species <- phenology_NOR_hi_flowers |>
  left_join(bio_flower_species_unique,
            by = "unique_plant_ID")


#

# fit the glmer.nb with species bimass ------------------------------------
m_flower_number_warm_bio_nb_species <- glmer.nb(
  value ~ treat_warming * treat_competition + pred_log_biomass_species +
    (1|species) + (1|block_ID),
  data = phenology_NOR_hi_flowers_bio_species,
  control = glmerControl(optimizer = "bobyqa") 
)
summary(m_flower_number_warm_bio_nb_species)

#subset = site == "hi"

AIC(m_flower_number_warm_bio_nb_species, m_flower_number_warm_glmer.nb)

# predict number of flowers ----------------------------------------------------------------------

emm_fl_num_warm_nb_bio <- emmeans(
  m_flower_number_warm_bio_nb_species,
  ~ treat_warming | treat_competition,
  cov.reduce = mean,
  type = "response"
)


emm_fl_num_df_warm_nb_bio <- as.data.frame(
  summary(emm_fl_num_warm_nb_bio, infer = TRUE)
) |>
  rename(
    emmean = response,
    lower.CL = asymp.LCL,
    upper.CL = asymp.UCL
  )

y_max   <- max(emm_fl_num_df_warm_nb_bio$emmean, na.rm = TRUE)
spacing <- y_max * 1

####################
contr_warm_nb_bio <- contrast(emm_fl_num_warm_nb_bio, method = "pairwise") |>
  as.data.frame() |>
  mutate(type = "warming",
         sig = case_when(
           p.value < 0.001 ~ "***",
           p.value < 0.01  ~ "**",
           p.value < 0.05  ~ "*",
           TRUE ~ "ns"
         ))

contr_comp_nb_bio <- contrast(
  emmeans(m_flower_number_warm_bio_nb_species, ~ treat_competition | treat_warming),
  method = "pairwise"
) |>
  as.data.frame() |>
  mutate(type = "m_flower_number_warm_bio_nb_species",
         sig = case_when(
           p.value < 0.001 ~ "***",
           p.value < 0.01  ~ "**",
           p.value < 0.05  ~ "*",
           TRUE ~ "ns"
         ))




# final plot ---------------------------------------------------------
dodge  <- position_dodge(width = 0.3)
offset <- 0.3 / 2

warm_levels <- levels(emm_fl_num_df_warm_nb_bio$treat_warming)

ann_warm_nb_bio <- contr_warm_nb_bio |> 
  mutate(
    warm1 = warm_levels[1],
    warm2 = warm_levels[2],
    x_shift = ifelse(treat_competition == "with", -offset, +offset),
    xmin = as.numeric(factor(warm1, levels = warm_levels)) + x_shift,
    xmax = as.numeric(factor(warm2, levels = warm_levels)) + x_shift,
    y = y_max + row_number() * spacing,
    label = sig
  )

ann_comp_nb_bio <- contr_comp_nb_bio |> 
  mutate(
    warm_x = as.numeric(factor(treat_warming, levels = warm_levels)),
    xmin = warm_x - offset,
    xmax = warm_x + offset,
    y = y_max + (nrow(ann_warm_nb) + row_number()) * spacing,
    label = sig
  )


p_warm_nb_bio <- ggplot() +
  
  geom_jitter(
    data = raw_means,
    aes(x = treat_warming, y = mean_value, color = treat_competition),
    width = 0.1, alpha = 0.3, size = 1.5
  ) +
  
  geom_point(
    data = emm_fl_num_df_warm_nb_bio,
    aes(x = treat_warming, y = emmean, color = treat_competition),
    size = 5,
    position = dodge
  ) +
  
  geom_errorbar(
    data = emm_fl_num_df_warm_nb_bio,
    aes(x = treat_warming, ymin = lower.CL, ymax = upper.CL,
        color = treat_competition),
    width = 0.2,
    linewidth = 0.8,
    position = dodge
  ) +
  
  geom_line(
    data = emm_fl_num_df_warm_nb_bio,
    aes(x = treat_warming, y = emmean,
        color = treat_competition,
        group = treat_competition),
    position = dodge
  ) +
  
  bracket_geoms(ann_comp_nb_bio) +
  bracket_geoms(ann_warm_nb_bio) +
  labs(x = "Temperature",
       y = "Predicted mean number of flowers\n(adjusted for biomass)",
       color = "Biotic interactions",
       title = "Effect of warming and competition on flower number\n(glmer.nb) adjusted for biomass")+
  scale_color_manual(values = c("#528B8B", "#CD950C"))
p_warm_nb_bio

ggsave(filename = "Output/Biomass/Warming_competition_flower_number_NOR_glmer.nb_adjusted_for_bimoass.png", plot = p_warm_nb_bio, 
       width = 10, height = 8, units = "in")












