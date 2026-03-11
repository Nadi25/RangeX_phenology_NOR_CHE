
# BIOMASS 5 ---------------------------------------------------------------

# RangeX biomass predictions 24 ------------

## Data used: 
## Date:      07.03.26
## Author:    Nadine Arzt
## Purpose:   Figure out which prediction method to use - one general model across species
##            or species specific models

# maybe species models is slightly better
# for 7/10 species 

theme_set(theme_bw(base_size = 22))

# colors ------------------------------------------------------------------
colors <- c(
  "#000000",
  "#E69F00",
  "#56B4E9",
  "#009E73",
  "#0072B2",
  "#D55E00",
  "#CC79A7",
  "#8DD3C7",
  "#FB8072",
  "#80B1D3"
)

# colors more like the sticks were
colors2 <- c(
  "#000000",
  "#8DD3C7",
  "#E69F00",
  "#56B4E9",
  "#D55E00",
  "#CC79A7",
  "#009E73",
  "#0072B2",
  "#80B1D3",
  "#FB8072"
)

# source script with both model methods -----------------------------------
# general
source("Biomass_traits_correlation_NOR.R")

# species
source("Biomass_traits_correlation_per_species.R")

# use
analysis_data_24_log

# predict biomass for 24 


# predict biomass for 24 with general model -------------------------------
analysis_data_24_log$pred_log_biomass_general <- predict(m_stems_height3, 
                                                 newdata = analysis_data_24_log)




# predict biomass for 24 with species specific models ---------------------
# sucpra ------------------------------------------------------------------
df_2024_sucpra <- analysis_data_24_log |> 
  filter(species == "sucpra") |> 
  drop_na(log_no_stems, log_number_leaves) # this removes plants where not both traits are available

df_2024_sucpra$pred_log_biomass <- predict(m_sucpra, newdata = df_2024_sucpra, re.form = NA)


# cennig ------------------------------------------------------------------
df_2024_cennig <- analysis_data_24_log |> 
  filter(species == "cennig") |> 
  drop_na(log_no_stems, log_height_reproductive_str)

df_2024_cennig$pred_log_biomass <- predict(m_cennig, newdata = df_2024_cennig, re.form = NA)


# pimsax ------------------------------------------------------------------
df_2024_pimsax <- analysis_data_24_log |> 
  filter(species == "pimsax") |> 
  drop_na(log_no_stems, log_number_leaves)

df_2024_pimsax$pred_log_biomass <- predict(m_pimsax, newdata = df_2024_pimsax, re.form = NA)


# luzmul ------------------------------------------------------------------
df_2024_luzmul <- analysis_data_24_log |> 
  filter(species == "luzmul") |> 
  drop_na(log_no_stems, log_number_leaves)

df_2024_luzmul$pred_log_biomass <- predict(m_luzmul, newdata = df_2024_luzmul, re.form = NA)


# leuvul ------------------------------------------------------------------
df_2024_leuvul <- analysis_data_24_log |> 
  filter(species == "leuvul") |> 
  drop_na(log_no_stems, log_number_leaves )

df_2024_leuvul$pred_log_biomass <- predict(m_leuvul, newdata = df_2024_leuvul, re.form = NA)


# tripra ------------------------------------------------------------------
df_2024_tripra <- analysis_data_24_log |> 
  filter(species == "tripra") |> 
  drop_na(log_height_reproductive_str)

df_2024_tripra$pred_log_biomass <- predict(m_tripra, newdata = df_2024_tripra, re.form = NA)


# hypmac ------------------------------------------------------------------
df_2024_hypmac <- analysis_data_24_log |> 
  filter(species == "hypmac") |> 
  drop_na(log_no_stems, log_number_leaves)

df_2024_hypmac$pred_log_biomass <- predict(m_hypmac, newdata = df_2024_hypmac, re.form = NA)


# plalan ------------------------------------------------------------------
df_2024_plalan <- analysis_data_24_log |> 
  filter(species == "plalan") |> 
  drop_na(log_height_vegetative_str, log_height_reproductive_str)

df_2024_plalan$pred_log_biomass <- predict(m_plalan, newdata = df_2024_plalan, re.form = NA)


# cyncri ------------------------------------------------------------------
df_2024_cyncri <- analysis_data_24_log |> 
  filter(species == "cyncri") |> 
  drop_na(log_no_stems, log_number_leaves )

df_2024_cyncri$pred_log_biomass <- predict(m_cyncri, newdata = df_2024_cyncri, re.form = NA)


# sildio ------------------------------------------------------------------
df_2024_sildio <- analysis_data_24_log |> 
  filter(species == "sildio") |> 
  drop_na(log_number_leaves, log_height_reproductive_str )

df_2024_sildio$pred_log_biomass <- predict(m_sildio, newdata = df_2024_sildio, re.form = NA)



# combine all species -----------------------------------------------------
df_2024_pred_species <- bind_rows(
  df_2024_sucpra,
  df_2024_cennig,
  df_2024_pimsax,
  df_2024_luzmul,
  df_2024_leuvul,
  df_2024_tripra,
  df_2024_hypmac,
  df_2024_plalan,
  df_2024_cyncri,
  df_2024_sildio
)

df_2024_pred_species <- df_2024_pred_species |> 
  rename(pred_log_biomass_species = pred_log_biomass)



# plot --------------------------------------------------------------------

# real bio vs pred bio general --------------------------------------------
ggplot(df_2024_pred_species, aes(log_biomass, pred_log_biomass_general, color = species)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

ggplot(df_2024_pred_species,
       aes(log_biomass, pred_log_biomass_general, color = species)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  scale_color_manual(values = colors)

# plot with species colros and functional group shape
e <- ggplot(df_2024_pred_species,
       aes(log_biomass,
           pred_log_biomass_general,
           color = species,
           shape = functional_group)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0) +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = c(16, 17, 15, 3, 8))+
  labs(x = "log(biomass real 24)",
       y = "log(pred biomass 24 general model)",
       color = "Species",
       shape = "Functional group")
e

ggsave(filename = "Output/Biomass/Log(biomass24)_log(pred_biomass24_general).png", 
       plot = e, width = 12, height = 9, units = "in")



# real bio vs pred bio species --------------------------------------------
ggplot(df_2024_pred_species, aes(log_biomass, pred_log_biomass_species, color = species)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

f <- ggplot(df_2024_pred_species,
       aes(log_biomass,
           pred_log_biomass_species,
           color = species,
           shape = functional_group)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0) +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = c(16, 17, 15, 3, 8))+
  labs(x = "log(biomass real 24)",
       y = "log(pred biomass 24 species model)",
       color = "Species",
       shape = "Functional group")
f

ggsave(filename = "Output/Biomass/Log(biomass24)_log(pred_biomass24_species).png", 
       plot = f, width = 12, height = 9, units = "in")


g <- ggplot(df_2024_pred_species,
            aes(log_biomass,
                pred_log_biomass_species,
                color = species,
                shape = functional_group)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0) +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = c(16, 17, 15, 3, 8))+
  labs(x = "log(biomass real 24)",
       y = "log(pred biomass 24 species model)",
       color = "Species",
       shape = "Functional group")+
  theme(legend.position = "none")+
  facet_wrap(~ species)
g

ggsave(filename = "Output/Biomass/Log(biomass24)_log(pred_biomass24_species_seperated).png", 
       plot = g, width = 12, height = 9, units = "in")

# what is better ----------------------------------------------------------
# rmse = root mean squared error
# metric that tells us how far apart our predicted values are from our observed values
rmse_general <- sqrt(mean(
  (df_2024_pred_species$log_biomass -
     df_2024_pred_species$pred_log_biomass_general)^2,
  na.rm = TRUE))
rmse_general

rmse_species <- sqrt(mean(
  (df_2024_pred_species$log_biomass -
     df_2024_pred_species$pred_log_biomass_species)^2,
  na.rm = TRUE))
rmse_species

# R2
r2_general <- cor(
  df_2024_pred_species$log_biomass,
  df_2024_pred_species$pred_log_biomass_general,
  use = "complete.obs"
)^2
r2_general

r2_species <- cor(
  df_2024_pred_species$log_biomass,
  df_2024_pred_species$pred_log_biomass_species,
  use = "complete.obs"
)^2
r2_species


data.frame(
  model = c("species_models", "general_model"),
  RMSE = c(rmse_species, rmse_general),
  R2 = c(r2_species, r2_general)
)


n <- df_2024_pred_species |>
  group_by(species) |>
  summarise(
    RMSE_species = sqrt(mean((log_biomass - pred_log_biomass_species)^2)),
    RMSE_general = sqrt(mean((log_biomass - pred_log_biomass_general)^2))
  )
n
# for 8/10 species the individual model is better then the general

n |>
  mutate(diff = RMSE_general - RMSE_species) |>
  summarise(mean_diff = mean(diff))


ggplot(df_2024_pred_species) +
  geom_point(aes(log_biomass, pred_log_biomass_species), alpha = 0.5) +
  geom_point(aes(log_biomass, pred_log_biomass_general), alpha = 0.5, color = "red") +
  geom_abline(slope = 1, intercept = 0)


## so we take the species specific model?





# figure out why some species are divided in two groups -------------------

ggplot(df_2024_pred_species,
       aes(log_no_stems, log_number_leaves, color = species)) +
  geom_point() +
  facet_wrap(~ species)

m_cyncri


ggplot(df_2024_pred_species,
       aes(log_biomass, pred_log_biomass_species, color = height_reproductive_str)) +
  geom_point() +
  facet_wrap(~ species)

# biomass differences per site, actually interesting
ggplot(df_2024_pred_species,
       aes(log_biomass, pred_log_biomass_species, color = site)) +
  geom_point() +
  facet_wrap(~ species)

ggplot(df_cyncri, aes(log_no_stems, log_biomass)) +
  geom_point() +
  labs(title = "cyncri biomass vs stems")

ggplot(df_cyncri, aes(log_number_leaves, log_biomass)) +
  geom_point() +
  labs(title = "CYNCRI: biomass vs leaves")


analysis_data_24_log |>
  filter(species == "cyncri") |>
  count(no_stems, number_leaves)


# seems like cyncri has many individuals with 1 stem and some amount of leaves
# this might be one group
# then others have several stems - second group

ggplot(df_2024_pred_species,
       aes(log_biomass, log_no_stems, color = treat_competition)) +
  geom_point() +
  facet_wrap(~ species)

ggplot(df_2024_cyncri,
       aes(pred_log_biomass, log_biomass,
           color = number_leaves)) +
  geom_point()

ggplot(df_2024_cyncri,
       aes(pred_log_biomass, log_biomass,
           color = factor(no_stems))) +
  geom_point()

df_cyncri |>
  ggplot(aes(site, log_biomass)) +
  geom_boxplot()

ggplot(df_2024_cyncri,
       aes(pred_log_biomass, log_biomass, color = site)) +
  geom_point()

# check if total is sum from individul
biomass_NOR_ <- biomass_NOR |>
  mutate(
    dry_sum = dry_weight_stem_g +
      dry_weight_leaves_g +
      dry_weight_flowers_g
  )

summary(biomass_NOR$dry_weight_total_g)
hist(biomass_NOR$dry_weight_total_g)

biomass_NOR |>
  filter(species == "cyncri") |>
  ggplot(aes(dry_weight_total_g)) +
  geom_histogram()


## is it the 0 in dry_weight_total_g


biomass_traits_NOR |>
  filter(species == "cyncri") |>
  ggplot(aes(dry_weight_total_g)) +
  geom_histogram(bins = 30)

biomass_traits_NOR |>
  filter(species == "cyncri") |>
  ggplot(aes(dry_weight_total_g, color = dry_weight_stem_g > 0)) +
  geom_density()



ggplot(df_2024_cyncri,
       aes(pred_log_biomass, log_biomass)) +
  geom_point() +
  geom_hline(yintercept = mean(df_2024_cyncri$log_biomass))


ggplot(df_cyncri,
       aes(log_no_stems, log_biomass)) +
  geom_point()

ggplot(biomass_traits_NOR |> 
         filter(species == "cyncri"),
       aes(number_leaves, dry_weight_total_g, color = no_stems)) +
  geom_point()

ggplot(biomass_traits_NOR, aes(no_stems, dry_weight_total_g)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Number of stems",
       y = "Total dry biomass (g)")

ggplot(
  biomass_traits_NOR |> 
    filter(species == "cyncri"),
  aes(no_stems, dry_weight_total_g)
) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    x = "Number of stems",
    y = "Total dry biomass (g)"
  )


## cyncri 

# Observation
# 
# Biomass vs. no_stems and biomass vs. number_leaves both show two distinct clusters.
# 
# Plants with similar stem or leaf numbers can have very different biomass.
# 
# Example from raw data:
#   
#   2 stems → ~1 g biomass
# 2 stems → ~15–20 g biomass
# 
# Interpretation
# 
# Stem number and leaf number do not fully capture plant size in cyncri.
# 
# Two structural plant states likely exist:
#   
#   Small individuals
# 
# few leaves
# 
# thin tillers
# 
# biomass ≈ 0–3 g
# 
# Established clumps
# 
# similar stem/leaf counts
# 
# thicker shoots / more leaf mass
# 
# biomass ≈ 10–30 g
# 
# Thus:
#   
#   biomass ≠ function(stems, leaves) only
# 
# Consequence for the biomass model
# 
# The biomass model (log_biomass ~ log_no_stems * log_number_leaves) cannot distinguish these two plant types.
# 
# Therefore predictions group plants with similar stem/leaf numbers together, producing two clouds in predicted vs. observed plots.
# 
# Conclusion
# 
# The pattern is not a coding error.
# 
# It reflects biological variation not captured by the predictors (e.g., tiller thickness or leaf mass).
# 
# The model still captures the overall biomass gradient, but with substantial unexplained variance.
# 








