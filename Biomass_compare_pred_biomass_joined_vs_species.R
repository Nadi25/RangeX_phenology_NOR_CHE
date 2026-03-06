

# BIOMASS 9 ---------------------------------------------------------------


# compare the predicted biomass for 23  -----------------------------------
# with species specific models and with one joined model


theme_set(theme_bw(base_size = 22))

# source script that predicts the 23 biomass
source("Biomass_prediction_for_23.R")
source("Biomass_prediction_for_23_per_species.R")

# combine 
bio_pred_23_species 
# and
rangex_traits_23_pb
# to compare pred biomass



rangex_traits_23_pb_2 <- rangex_traits_23_pb |> 
  left_join(
    bio_pred_23_species |> 
      select(unique_plant_ID, pred_log_biomass) |> 
      rename(pred_log_biomass_species = pred_log_biomass),
    by = "unique_plant_ID"
  )





c <- ggplot(rangex_traits_23_pb_2, aes(x = pred_log_biomass, y = pred_log_biomass_species)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Predicted log biomass (one model)",
    y = "Predicted log biomass (species-specific models)",
    title = "Comparison of predicted log biomass methods"
  ) 
c

ggsave(filename = "Output/Biomass/Comparison_of_predicted_bio_general_vs_species.png", 
       plot = c, width = 13, height = 9, units = "in")


d <- ggplot(rangex_traits_23_pb_2, aes(x = pred_log_biomass, y = pred_log_biomass_species, color = species)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Predicted log biomass (one model)",
    y = "Predicted log biomass (species-specific models)",
    title = "Comparison of predicted log biomass methods"
  )
d
ggsave(filename = "Output/Biomass/Comparison_of_predicted_bio_general_vs_species_colored.png", 
       plot = d, width = 13, height = 9, units = "in")

cor(rangex_traits_23_pb_2$pred_log_biomass,
    rangex_traits_23_pb_2$pred_log_biomass_species,
    use = "complete.obs")
#  0.8708552

rangex_traits_23_pb_2$diff <- 
  rangex_traits_23_pb_2$pred_log_biomass_species -
  rangex_traits_23_pb_2$pred_log_biomass

ggplot(rangex_traits_23_pb_2, aes(species, diff)) +
  geom_boxplot()














