
# BIOMASS 5 ---------------------------------------------------------------

# RangeX biomass predictions 24 ------------

## Data used: 
## Date:      07.03.26
## Author:    Nadine Arzt
## Purpose:   Figure out which prediction method to use - one general model across species
##            or species specific models

# maybe species models is slightly better
# for 7/10 species 



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
  drop_na(log_no_stems, log_number_leaves)

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
  drop_na(log_no_stems)

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
  drop_na(log_no_stems)

df_2024_cyncri$pred_log_biomass <- predict(m_cyncri, newdata = df_2024_cyncri, re.form = NA)


# sildio ------------------------------------------------------------------
df_2024_sildio <- analysis_data_24_log |> 
  filter(species == "sildio") |> 
  drop_na(log_number_leaves)

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
# for 7/10 species the individual model is better then the general

n |>
  mutate(diff = RMSE_general - RMSE_species) |>
  summarise(mean_diff = mean(diff))


ggplot(df_2024_pred_species) +
  geom_point(aes(log_biomass, pred_log_biomass_species), alpha = 0.5) +
  geom_point(aes(log_biomass, pred_log_biomass_general), alpha = 0.5, color = "red") +
  geom_abline(slope = 1, intercept = 0)


## so we take the species specific model?
