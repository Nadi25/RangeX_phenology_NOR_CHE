

source("Effect_cooling_on_flower_number_NOR_10.09.25.R")

# change reference to be low site ----------------------------------------------
# by factor
phenology_NOR_ambi_flowers <- phenology_NOR_ambi_flowers |>
  mutate(
    site = factor(site),
    site = relevel(site, ref = "low")
  )

# now low site is the reference and will come first in the plot
# fit the glmer.nb  ------------------------------------
m_flower_number_cool_nb <- glmer.nb(value ~ site * treat_competition + 
                                      (1|species) + (1|block_ID),
                                    data = phenology_NOR_ambi_flowers,
                                    control = glmerControl(optimizer = "bobyqa"))
summary(m_flower_number_cool_nb)



# predict number of flowers ----------------------------------------------------------------------

emm_fl_num_cool_nb <- emmeans(
  m_flower_number_cool_nb,
  ~ site | treat_competition,
  cov.reduce = mean,
  type = "response"
)


emm_fl_num_df_cool_nb <- as.data.frame(
  summary(emm_fl_num_cool_nb, infer = TRUE)
) |>
  rename(
    emmean = response,
    lower.CL = asymp.LCL,
    upper.CL = asymp.UCL
  )

####################
contr_site_nb <- contrast(emm_fl_num_cool_nb, method = "pairwise") |>
  as.data.frame() |>
  mutate(type = "site",
         sig = case_when(
           p.value < 0.001 ~ "***",
           p.value < 0.01  ~ "**",
           p.value < 0.05  ~ "*",
           TRUE ~ "ns"
         ))

contr_comp_nb <- contrast(
  emmeans(m_flower_number_cool_nb, ~ treat_competition | site),
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




# final plot ---------------------------------------------------------
dodge  <- position_dodge(width = 0.3)
offset <- 0.3 / 2

site_levels <- levels(emm_fl_num_df_cool_nb$site)

ann_site_nb <- contr_site_nb |> 
  mutate(
    site1 = site_levels[1],
    site2 = site_levels[2],
    x_shift = ifelse(treat_competition == "with", -offset, +offset),
    xmin = as.numeric(factor(site1, levels = site_levels)) + x_shift,
    xmax = as.numeric(factor(site2, levels = site_levels)) + x_shift,
    y = y_max + row_number() * spacing,
    label = sig
  )

ann_comp_nb <- contr_comp_nb |> 
  mutate(
    site_x = as.numeric(factor(site, levels = site_levels)),
    xmin = site_x - offset,
    xmax = site_x + offset,
    y = y_max + (nrow(ann_site_nb) + row_number()) * spacing,
    label = sig
  )

p_nb <- ggplot() +
  
  # raw data (optional)
  geom_jitter(
    data = raw_means,
    aes(x = site, y = mean_value, color = treat_competition),
    width = 0.1, alpha = 0.3, size = 1.5
  ) +
  
  # EMM points
  geom_point(
    data = emm_fl_num_df_cool_nb,
    aes(x = site, y = emmean, color = treat_competition),
    size = 5,
    position = dodge
  ) +
  
  # EMM CI
  geom_errorbar(
    data = emm_fl_num_df_cool_nb,
    aes(x = site, ymin = lower.CL, ymax = upper.CL, color = treat_competition),
    width = 0.2,
    linewidth = 0.8,
    position = dodge
  ) +
  
  # EMM lines
  geom_line(
    data = emm_fl_num_df_cool_nb,
    aes(x = site, y = emmean, color = treat_competition,
        group = treat_competition),
    position = dodge
  ) +
  
  # competition brackets (within site)
  bracket_geoms(ann_comp_nb) +
  
  # site brackets (within competition)
  bracket_geoms(ann_site_nb) +
  
  labs(
    x = "Site",
    y = "Predicted mean number of flowers",
    color = "Biotic interactions",
    title = "Effect of site and competition on flower number\n(glmer.nb)"
  ) +
  
  scale_color_manual(values = c("#528B8B", "#CD950C"))
p_nb

# ggsave(filename = "Output/Biomass/Cooling_competition_flower_number_NOR_glmer.nb.png", 
#        plot = p_nb, width = 10, height = 8, units = "in")




























