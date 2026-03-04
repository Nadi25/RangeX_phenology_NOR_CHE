
# RangeX biomass - traits preditct 2023 biomass ------------

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



# use best model to predict biomass for 2023 ------------------------------
# based on no_stems and height_reproductive_str

demo_traits_2023_bio$pred_log_biomass <- predict(m_stems_height2, newdata = demo_traits_2023_bio)



# plotting ----------------------------------------------------------------
# histogram of prediczed biomass
ggplot(demo_traits_2023_bio,
       aes(x = pred_log_biomass)) +
  geom_histogram(bins = 30) +
  labs(x = "Predicted log(biomass)",
       y = "Frequency")

# pred biomass vs no stems
ggplot(demo_traits_2023_bio,
       aes(x = no_stems,
           y = pred_log_biomass)) +
  geom_point(alpha = 0.6) +
  labs(x = "Number of stems",
       y = "Predicted log(biomass)")

# plot predicted biomass and observed in same plot
a <- ggplot() +
  geom_point(data = analysis_data,
             aes(x = no_stems,
                 y = log_biomass),
             color = "grey50",
             alpha = 0.4) +
  geom_point(data = demo_traits_2023_bio,
             aes(x = no_stems,
                 y = pred_log_biomass),
             color = "red",
             alpha = 0.6) +
  labs(x = "Number of stems",
       y = "log(biomass)",
       subtitle = "Grey = observed 2024, Red = predicted 2023")
a

ggsave(filename = "Output/Biomass_observed_24_prediction_23.png", 
       plot = a, width = 13, height = 9, units = "in")





