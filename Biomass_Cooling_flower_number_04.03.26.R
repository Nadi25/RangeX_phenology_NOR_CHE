
# BIOMASS 8 ---------------------------------------------------------------

# Adding predicted biomass to phenology and fit model with pred biomass ------------

## Data used: 
## Date:      03.03.26
## Author:    Nadine Arzt
## Purpose:   Does biomass mitigate the effect of cooling on no of flowers?




source("Effect_cooling_on_flower_number_NOR_10.09.25.R")




# add predicted biomass to pheno data -------------------------------------

phenology_NOR_ambi_flowers_bio <- phenology_NOR_ambi_flowers |>
  left_join(bio_flower |> select(unique_plant_ID, pred_log_biomass),
            by = "unique_plant_ID")





# fit the model with pred log biomass -------------------------------------

m_flower_number_cool_bio <- lmerTest::lmer(
  value ~ site * treat_competition + pred_log_biomass +
    (1|species) + (1|block_ID),
  data = phenology_NOR_ambi_flowers_bio
)
summary(m_flower_number_cool_bio)

# Does cooling affect flowers after controlling for plant size? Yes#
# when biomass is included, plants produce sig more flowers at the high site
# no, this cant be said like this because of the interaction effect that is sig

# compared to the old model without biomass
summary(m_flower_number_cool)



# same plot now model -----------------------------------------------------

emm_fl_num_cool_bio <- emmeans(
  m_flower_number_cool_bio,
  ~ site | treat_competition,
  cov.reduce = mean   
)

emm_fl_num_df_cool_bio <- as.data.frame(
  summary(emm_fl_num_cool_bio, infer = TRUE)
) |>
  rename(lower.CL = asymp.LCL, upper.CL = asymp.UCL)



contr_site_bio <- contrast(emm_fl_num_cool_bio, method = "pairwise") |>
  as.data.frame() |>
  mutate(type = "site",
         sig = case_when(
           p.value < 0.001 ~ "***",
           p.value < 0.01  ~ "**",
           p.value < 0.05  ~ "*",
           TRUE ~ "ns"
         ))

contr_comp_bio <- contrast(
  emmeans(m_flower_number_cool_bio, ~ treat_competition | site),
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

site_levels <- levels(emm_fl_num_df_cool_bio$site)

ann_site_bio <- contr_site_bio |> 
  mutate(
    site1 = site_levels[1],
    site2 = site_levels[2],
    x_shift = ifelse(treat_competition == "with", -offset, +offset),
    xmin = as.numeric(factor(site1, levels = site_levels)) + x_shift,
    xmax = as.numeric(factor(site2, levels = site_levels)) + x_shift,
    y = y_max + row_number() * spacing,
    label = sig
  )

ann_comp_bio <- contr_comp_bio |> 
  mutate(
    site_x = as.numeric(factor(site, levels = site_levels)),
    xmin = site_x - offset,
    xmax = site_x + offset,
    y = y_max + (nrow(ann_site_bio) + row_number()) * spacing,
    label = sig
  )

p_bio <- ggplot() +
  
  # raw data (optional)
  geom_jitter(
    data = raw_means,
    aes(x = site, y = mean_value, color = treat_competition),
    width = 0.1, alpha = 0.3, size = 1.5
  ) +
  
  # EMM points
  geom_point(
    data = emm_fl_num_df_cool_bio,
    aes(x = site, y = emmean, color = treat_competition),
    size = 5,
    position = dodge
  ) +
  
  # EMM CI
  geom_errorbar(
    data = emm_fl_num_df_cool_bio,
    aes(x = site, ymin = lower.CL, ymax = upper.CL, color = treat_competition),
    width = 0.2,
    linewidth = 0.8,
    position = dodge
  ) +
  
  # EMM lines
  geom_line(
    data = emm_fl_num_df_cool_bio,
    aes(x = site, y = emmean, color = treat_competition,
        group = treat_competition),
    position = dodge
  ) +
  
  # competition brackets (within site)
  bracket_geoms(ann_comp_bio) +
  
  # site brackets (within competition)
  bracket_geoms(ann_site_bio) +
  
  labs(
    x = "Site",
    y = "Predicted mean number of flowers\n(adjusted for biomass)",
    color = "Biotic interactions",
    title = "Effect of site and competition on flower number including biomass"
  ) +
  
  scale_color_manual(values = c("#528B8B", "#CD950C"))
p_bio


# no sign difference between with and without competition at the high site
# does this speak for facilitation?
ggsave(filename = "Output/Biomass/Cooling_competition_flower_number_NOR_adjusted_biomass.png", 
       plot = p_bio, width = 10, height = 8, units = "in")




