
# BIOMASS 2 ---------------------------------------------------------------

# RangeX biomass - traits plots ------------

## Data used: source("Biomass_traits_correlation_NOR.R")
## Date:      03.03.26
## Author:    Nadine Arzt
## Purpose:   Biomass and trait regressions plots



source("Biomass_traits_correlation_NOR.R")

library(ggeffects)
theme_set(theme_bw(base_size = 22))

# Create new data for prediction
pred_data <- expand.grid(
  no_stems = seq(min(analysis_data$no_stems), max(analysis_data$no_stems), length.out = 30),
  height_reproductive_str = quantile(analysis_data$height_reproductive_str, probs = c(0.25, 0.5, 0.75))
)

# Add predicted values
pred_data$pred_log_biomass <- predict(m_stems_height2, newdata = pred_data, re.form = NA)

# Plot
ggplot(pred_data, aes(x = no_stems, y = pred_log_biomass, color = as.factor(height_reproductive_str))) +
  geom_line(size = 1.2) +
  labs(x = "Number of stems",
       y = "Predicted log(biomass)",
       color = "Height (cm)")



# Heatmap plot ------------------------------------------------------------
# 1. Create a grid of predictor values
grid <- expand.grid(
  no_stems = seq(min(analysis_data$no_stems), max(analysis_data$no_stems), length.out = 50),
  height_reproductive_str = seq(min(analysis_data$height_reproductive_str), max(analysis_data$height_reproductive_str), length.out = 50)
)

# 2. Add predicted log_biomass from the interaction model
grid$pred_log_biomass <- predict(m_stems_height2, newdata = grid, re.form = NA)

# 3. Plot observed points + predicted surface
ggplot() +
  geom_point(data = analysis_data, 
             aes(x = no_stems, y = height_reproductive_str, color = log_biomass), alpha = 0.6) +
  geom_tile(data = grid, 
            aes(x = no_stems, y = height_reproductive_str, fill = pred_log_biomass), alpha = 0.4) +
  scale_color_viridis_c(option = "C") +
  scale_fill_viridis_c(option = "C") +
  labs(
    x = "Number of stems",
    y = "Height rep str(cm)",
    color = "Observed log(biomass)",
    fill = "Predicted log(biomass)"
  ) +
  theme(legend.position = "right")



# boxplot biomass low high ------------------------------------------------
ggplot(analysis_data, aes(x = treat_warming, y = log_biomass))+
  geom_boxplot()

ggplot(analysis_data, aes(x = site, y = log_biomass))+
  geom_boxplot()


# regression no stems log biomass -----------------------------------------
ggplot(analysis_data, aes(x = log_no_stems, y = log_biomass, colour = treat_warming)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "log(number of stems)",
       y = "log(biomass)")


ggplot(analysis_data,
       aes(x = log_no_stems,
           y = log_biomass,
           color = height_reproductive_str)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c() +
  labs(x = "log(number of stems)",
       y = "log(biomass)",
       color = "Height (cm)")


analysis_data <- analysis_data |>
  mutate(height_class = cut(height_reproductive_str, 3))

ggplot(analysis_data,
       aes(x = log_no_stems, y = log_biomass)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ height_class)




# predict data? -----------------------------------------------------------
# make several classes of height with the quantiles
# min, low, medium, high, max
pred <- ggpredict(m_stems_height3,
                  terms = c("log_no_stems",
                            "height_reproductive_str [quantile]"))

plot(pred)


ggplot() +
  geom_point(data = analysis_data,
             aes(x = log_no_stems,
                 y = log_biomass,
                 color = height_reproductive_str),
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
  labs(x = "log(number of stems)",
       y = "log(biomass)",
       color = "Height rep str (cm)")



# plot predicted regression of biomass and no stems and height ------------
# the lines represent different height classes because the model that fits best 
# is with height as interaction
# the dots are the raw data
p <- ggplot() +
  geom_point(data = analysis_data,
             aes(x = log_no_stems,
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
  labs(x = "log(number of stems)",
       y = "log(biomass total)",
       color = "Height rep str",
       fill = "Height rep str")
p

ggsave(filename = "Output/Biomass/Log(biomass)_log(no_stems)_correlation.png", 
       plot = p, width = 13, height = 9, units = "in")


# since the slopes are different with height:
# The effect of stems on biomass depends on plant height.



# color code by species ---------------------------------------------------


p2 <- ggplot() +
  geom_point(data = analysis_data,
             aes(x = log_no_stems,
                 y = log_biomass,
                 color = species),
             alpha = 0.3) +
  
  geom_line(data = pred,
            aes(x = x,
                y = predicted,
                group = group),
            color = "black",
            linewidth = 1.2) +
  
  geom_ribbon(data = pred,
              aes(x = x,
                  ymin = conf.low,
                  ymax = conf.high,
                  group = group),
              fill = "grey60",
              alpha = 0.2) +
  
  labs(x = "log(number of stems)",
       y = "log(total biomass)",
       color = "Species")

p2

ggplot() +
  geom_point(data = analysis_data,
             aes(x = log_no_stems,
                 y = log_biomass,
                 color = species),
             alpha = 0.3) +
  
  # geom_line(data = pred,
  #           aes(x = x,
  #               y = predicted,
  #               group = group),
  #           color = "black",
  #           linewidth = 1.2) +
  
  # geom_ribbon(data = pred,
  #             aes(x = x,
  #                 ymin = conf.low,
  #                 ymax = conf.high,
  #                 group = group),
  #             fill = "grey60",
  #             alpha = 0.2) +
  
  labs(x = "log(number of stems)",
       y = "log(total biomass)",
       color = "Species")+
  facet_grid(~species)







