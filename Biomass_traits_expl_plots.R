

library(ggeffects)


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





ggplot(analysis_data, aes(x = no..stems, y = log_biomass)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(x = "Number of stems",
       y = "log(biomass)")


ggplot(analysis_data,
       aes(x = no..stems,
           y = log_biomass,
           color = height.rep.stretch..cm.)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(x = "Number of stems",
       y = "log(biomass)",
       color = "Height (cm)")


analysis_data <- analysis_data |>
  mutate(height_class = cut(height.rep.stretch..cm., 3))

ggplot(analysis_data,
       aes(x = no..stems, y = log_biomass)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ height_class) +
  theme_minimal()




# predict data? -----------------------------------------------------------
# make several classes of height with the quantiles
# min, low, medium, high, max
pred <- ggpredict(m_stems_height2,
                  terms = c("no..stems",
                            "height.rep.stretch..cm. [quantile]"))

plot(pred)


ggplot() +
  geom_point(data = analysis_data,
             aes(x = no..stems,
                 y = log_biomass,
                 color = height.rep.stretch..cm.),
             alpha = 0.5) +
  geom_line(data = pred,
            aes(x = x,
                y = predicted,
                group = group),
            size = 1.2) +
  geom_ribbon(data = pred,
              aes(x = x,
                  ymin = conf.low,
                  ymax = conf.high,
                  group = group),
              alpha = 0.15) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(x = "Number of stems",
       y = "log(biomass)",
       color = "Height (cm)")



# plot predicted regression of biomass and no stems and height ------------
# the lines represent different height classes because the model that fits best 
# is with height as interaction
# the dots are the raw data
ggplot() +
  geom_point(data = analysis_data,
             aes(x = no..stems,
                 y = log_biomass),
             alpha = 0.3,
             color = "grey50") +
  geom_line(data = pred,
            aes(x = x,
                y = predicted,
                color = group),
            size = 1.3) +
  geom_ribbon(data = pred,
              aes(x = x,
                  ymin = conf.low,
                  ymax = conf.high,
                  fill = group),
              alpha = 0.15) +
  theme_minimal() +
  labs(x = "Number of stems",
       y = "log(biomass total)",
       color = "Height",
       fill = "Height")















