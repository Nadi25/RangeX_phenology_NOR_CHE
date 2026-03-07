
# BIOMASS 12 ---------------------------------------------------------------

# Effect of cooling on flower number glmer.nb -------------------------------------
# negative binomial model is better for our data

# uses one general bioamss prediction model

# Adding predicted biomass to phenology and fit general model with pred biomass ------------

## Data used: 
## Date:      03.03.26
## Author:    Nadine Arzt
## Purpose:   Does biomass mitigate the effect of cooling on no of flowers?



library(performance)

# Use glmer.nb model ------------------------------------------------------

source("Biomass_Cooling_flower_number_04.03.26.R")


# this takes very long
m_flower_number_cool_bio_nb <- glmer.nb(
  value ~ site * treat_competition + pred_log_biomass +
    (1|species) + (1|block_ID),
  data = phenology_NOR_ambi_flowers_bio,
  control = glmerControl(optimizer = "bobyqa")
)
summary(m_flower_number_cool_bio_nb)

AIC(m_flower_number_cool_bio, m_flower_number_cool_bio_nb)

summary(m_flower_number_cool_bio)

performance::check_overdispersion(m_flower_number_cool_bio_nb)
# underdispersion??

# same plot new model -----------------------------------------------------
# 
# emm_fl_num_cool_bio_nb <- emmeans(
#   m_flower_number_cool_bio_nb,
#   ~ site | treat_competition,
#   cov.reduce = mean   
# )
# 
# emm_fl_num_df_cool_bio_nb <- as.data.frame(
#   summary(emm_fl_num_cool_bio_nb, infer = TRUE)
# ) |>
#   rename(lower.CL = asymp.LCL, upper.CL = asymp.UCL)
# 

# OR ----------------------------------------------------------------------
emm_fl_num_cool_bio_nb <- emmeans(
  m_flower_number_cool_bio_nb,
  ~ site | treat_competition,
  cov.reduce = mean,
  type = "response"
)


emm_fl_num_df_cool_bio_nb <- as.data.frame(
  summary(emm_fl_num_cool_bio_nb, infer = TRUE)
) |>
  rename(
    emmean = response,
    lower.CL = asymp.LCL,
    upper.CL = asymp.UCL
  )

####################
contr_site_bio_nb <- contrast(emm_fl_num_cool_bio_nb, method = "pairwise") |>
  as.data.frame() |>
  mutate(type = "site",
         sig = case_when(
           p.value < 0.001 ~ "***",
           p.value < 0.01  ~ "**",
           p.value < 0.05  ~ "*",
           TRUE ~ "ns"
         ))

contr_comp_bio_nb <- contrast(
  emmeans(m_flower_number_cool_bio_nb, ~ treat_competition | site),
  method = "pairwise"
) |>
  as.data.frame() |>
  mutate(type = "competition",
         sig = case_when(
           p.value < 0.001 ~ "***",
           p.value < 0.01  ~ "**",
           p.value < 0.05  ~ "*",
           TRUE ~ "ns"
         ))




# this plot works ---------------------------------------------------------
dodge  <- position_dodge(width = 0.3)
offset <- 0.3 / 2

site_levels <- levels(emm_fl_num_df_cool_bio_nb$site)

ann_site_bio_nb <- contr_site_bio_nb |> 
  mutate(
    site1 = site_levels[1],
    site2 = site_levels[2],
    x_shift = ifelse(treat_competition == "with", -offset, +offset),
    xmin = as.numeric(factor(site1, levels = site_levels)) + x_shift,
    xmax = as.numeric(factor(site2, levels = site_levels)) + x_shift,
    y = y_max + row_number() * spacing,
    label = sig
  )

ann_comp_bio_nb <- contr_comp_bio_nb |> 
  mutate(
    site_x = as.numeric(factor(site, levels = site_levels)),
    xmin = site_x - offset,
    xmax = site_x + offset,
    y = y_max + (nrow(ann_site_bio_nb) + row_number()) * spacing,
    label = sig
  )

p_bio_nb <- ggplot() +
  
  # raw data (optional)
  geom_jitter(
    data = raw_means,
    aes(x = site, y = mean_value, color = treat_competition),
    width = 0.1, alpha = 0.3, size = 1.5
  ) +
  
  # EMM points
  geom_point(
    data = emm_fl_num_df_cool_bio_nb,
    aes(x = site, y = emmean, color = treat_competition),
    size = 5,
    position = dodge
  ) +
  
  # EMM CI
  geom_errorbar(
    data = emm_fl_num_df_cool_bio_nb,
    aes(x = site, ymin = lower.CL, ymax = upper.CL, color = treat_competition),
    width = 0.2,
    linewidth = 0.8,
    position = dodge
  ) +
  
  # EMM lines
  geom_line(
    data = emm_fl_num_df_cool_bio_nb,
    aes(x = site, y = emmean, color = treat_competition,
        group = treat_competition),
    position = dodge
  ) +
  
  # competition brackets (within site)
  bracket_geoms(ann_comp_bio_nb) +
  
  # site brackets (within competition)
  bracket_geoms(ann_site_bio_nb) +
  
  labs(
    x = "Site",
    y = "Predicted mean number of flowers\n(adjusted for biomass)",
    color = "Biotic interactions",
    title = "Effect of site and competition on flower number including biomass"
  ) +
  
  scale_color_manual(values = c("#528B8B", "#CD950C"))
p_bio_nb














