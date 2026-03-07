
# BIOMASS 7 ---------------------------------------------------------------

# RangeX biomass - traits predict 2023 biomass ------------

## Data used: 
## Date:      03.03.26
## Author:    Nadine Arzt
## Purpose:   Predict biomass from 23 plants with traits



# source scripts ----------------------------------------------------------
# source biomass correlation preparation script
source("Biomass_traits_correlation_NOR.R")

# source clean traits 23 script
source("RangeX_data_paper_cleaning_demographic_traits_23.R")


# rename no_stems ---------------------------------------------------------
demo_traits_2023_bio <- demo_traits_2023 |> 
  rename(no_stems = no_rep_stems,
         block_ID = block_ID_original)



# log no stems also in 2023 -----------------------------------------------
# remove 0 stems
demo_traits_2023_bio <- demo_traits_2023_bio |>
  filter(no_stems > 0)

demo_traits_2023_bio <- demo_traits_2023_bio |> 
  mutate(log_no_stems = log(no_stems))



# use best model to predict biomass for 2023 ------------------------------
# based on no_stems and height_reproductive_str

demo_traits_2023_bio$pred_log_biomass <- predict(m_stems_height3, 
                                                 newdata = demo_traits_2023_bio)



# plotting ----------------------------------------------------------------
# histogram of predicted biomass
ggplot(demo_traits_2023_bio,
       aes(x = pred_log_biomass)) +
  geom_histogram(bins = 30) +
  labs(x = "Predicted log(biomass)",
       y = "Frequency")

# pred biomass vs no stems
ggplot(demo_traits_2023_bio,
       aes(x = log_no_stems,
           y = pred_log_biomass)) +
  geom_point(alpha = 0.6) +
  labs(x = "Log(number of stems)",
       y = "Predicted log(biomass)")

# plot predicted biomass and observed in same plot
a <- ggplot() +
  geom_point(data = analysis_data,
             aes(x = log_no_stems,
                 y = log_biomass),
             color = "grey50",
             alpha = 0.4) +
  geom_point(data = demo_traits_2023_bio,
             aes(x = log_no_stems,
                 y = pred_log_biomass),
             color = "red",
             alpha = 0.6) +
  labs(x = "Log(number of stems)",
       y = "Log(biomass)",
       subtitle = "Grey = observed 2024, Red = predicted 2023")
a

ggsave(filename = "Output/Biomass/Log_biomass_log_no_stems_observed_24_prediction_23.png", 
       plot = a, width = 13, height = 9, units = "in")

# 
# analysis_data$species <- droplevels(as.factor(analysis_data$species))
# demo_traits_2023_bio$species <- droplevels(as.factor(demo_traits_2023_bio$species))
# 
# levels(analysis_data$species)
# levels(demo_traits_2023_bio$species)

# per species
ggplot() +
  geom_point(data = analysis_data,
             aes(x = log_no_stems,
                 y = log_biomass),
             color = "grey50",
             alpha = 0.4) +
  geom_point(data = demo_traits_2023_bio,
             aes(x = log_no_stems,
                 y = pred_log_biomass),
             color = "red",
             alpha = 0.6) +
  facet_grid(~species)+
  labs(x = "Log(number of stems)",
       y = "log(biomass)",
       subtitle = "Grey = observed 2024, Red = predicted 2023")






# add pred_biomass to demo_23 ---------------------------------------------

# use this dataset with predicted log biomass
demo_traits_2023_bio

## make correct order as in yearly_demographics
col_order_traits_23_pb <- c("site", "block_ID", "plot_ID_original","unique_plant_ID", 
                            "species", "year", "collector", "observer", "height_vegetative_str", 
                            "height_reproductive_str", "height_vegetative", "height_reproductive", 
                            "vegetative_width", "vegetative_length", "stem_diameter",
                            "leaf_length1", "leaf_length2", "leaf_length3", "leaf_width", "petiole_length", 
                            "petiole_length1", "no_stems",
                            "number_leaves", "number_tillers", "number_branches", "number_leafclusters", 
                            "number_flowers", "mean_inflorescence_size", "herbivory", 
                            "pred_log_biomass", "log_no_stems")

rangex_traits_23_pb <- demo_traits_2023_bio[, col_order_traits_23_pb]
rangex_traits_23_pb


## collector = observer
## put values from leaf_length in column leaf_length1
## same with petiole_length
rangex_traits_23_pb <- rangex_traits_23_pb |> 
  mutate(collector = coalesce(collector, observer)) |> 
  mutate(petiole_length = coalesce(petiole_length, petiole_length1)) |> 
  select(-observer, - petiole_length1)










































