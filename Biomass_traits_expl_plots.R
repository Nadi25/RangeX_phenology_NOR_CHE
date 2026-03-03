

source("Biomass_traits_correlation_NOR.R")


# Create new data for prediction
pred_data <- expand.grid(
  no..stems = seq(min(analysis_data$no..stems), max(analysis_data$no..stems), length.out = 30),
  height.rep.stretch..cm. = quantile(analysis_data$height.rep.stretch..cm., probs = c(0.25, 0.5, 0.75))
)

# Add predicted values
pred_data$pred_log_biomass <- predict(m_stems_height2, newdata = pred_data, re.form = NA)

# Plot
ggplot(pred_data, aes(x = no..stems, y = pred_log_biomass, color = as.factor(height.rep.stretch..cm.))) +
  geom_line(size = 1.2) +
  labs(x = "Number of stems",
       y = "Predicted log(biomass)",
       color = "Height (cm)") +
  theme_minimal()



library(ggplot2)
library(dplyr)

# 1. Create a grid of predictor values
grid <- expand.grid(
  no..stems = seq(min(analysis_data$no..stems), max(analysis_data$no..stems), length.out = 50),
  height.rep.stretch..cm. = seq(min(analysis_data$height.rep.stretch..cm.), max(analysis_data$height.rep.stretch..cm.), length.out = 50)
)

# 2. Add predicted log_biomass from the interaction model
grid$pred_log_biomass <- predict(m_stems_height2, newdata = grid, re.form = NA)

# 3. Plot observed points + predicted surface
ggplot() +
  geom_point(data = analysis_data, 
             aes(x = no..stems, y = height.rep.stretch..cm., color = log_biomass), alpha = 0.6) +
  geom_tile(data = grid, 
            aes(x = no..stems, y = height.rep.stretch..cm., fill = pred_log_biomass), alpha = 0.4) +
  scale_color_viridis_c(option = "C") +
  scale_fill_viridis_c(option = "C") +
  labs(
    x = "Number of stems",
    y = "Height (cm)",
    color = "Observed log(biomass)",
    fill = "Predicted log(biomass)"
  ) +
  theme_minimal() +
  theme(legend.position = "right")
















