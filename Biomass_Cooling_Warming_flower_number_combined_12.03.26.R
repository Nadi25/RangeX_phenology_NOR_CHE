

# Combine thw two models cooling warming effects on flowre number  --------


# make sure pred_log_biomass is numeric
phenology_NOR_ambi_flowers_bio$pred_log_biomass <- as.numeric(phenology_NOR_ambi_flowers_bio$pred_log_biomass)

mean_biomass <- mean(phenology_NOR_ambi_flowers_bio$pred_log_biomass, na.rm = TRUE)

# predicted flower numbers for cooling experiment
emm_cool <- emmeans(m_flower_number_cool_bio_nb, 
                    ~ site * treat_competition,
                    at = list(pred_log_biomass_species = mean_biomass))
df_cool <- as.data.frame(emm_cool)

df_cool$temperature_level <- ifelse(df_cool$site == "low", "Low site", "High site ambient")


# predicted flower numbers for warming experiment
emm_warm <- emmeans(m_flower_number_warm_bio_nb_species, 
                    ~ treat_warming * treat_competition, 
                    adjust = "none")  # emmeans will average over the covariate

df_warm <- as.data.frame(emm_warm)
df_warm$temperature_level <- ifelse(df_warm$treat_warming == "ambient", "High site ambient", "High site warmed")
df_warm





df_all <- rbind(
  df_cool[, c("temperature_level", "treat_competition", "emmean", "SE")],
  df_warm[, c("temperature_level", "treat_competition", "emmean", "SE")]
)

df_all$temperature_level <- factor(df_all$temperature_level, 
                                   levels = c("Low site", "High site ambient", "High site warmed"))




ggplot(df_all, aes(x = temperature_level, y = emmean, color = treat_competition, group = treat_competition)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_line(position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.2, position = position_dodge(width = 0.3)) +
  labs(x = "",
       y = "Predicted flower number (adjusted for biomass)",
       color = "Biotic interactions") +
  scale_color_manual(values = c("#528B8B", "#CD950C"))



# keep the two models coolig warming separate -----------------------------

# Add an 'experiment' column to keep track of origin
df_cool$experiment <- "cooling"
df_warm$experiment <- "warming"

# Combine
df_all <- bind_rows(
  df_cool[, c("site", "treat_competition", "emmean", "SE", "experiment")],
  df_warm[, c("treat_warming", "treat_competition", "emmean", "SE", "experiment")]
)

# Define temperature_level for plotting
df_all <- df_all %>%
  mutate(
    temperature_level = case_when(
      site == "low" | treat_warming == "Low site" ~ "Low site",
      site == "high" & experiment == "cooling" | treat_warming == "ambient" & experiment == "warming" ~ "High site ambient",
      treat_warming == "warmed" ~ "High site warmed"
    )
  )

# Make factor with correct gradient order
df_all$temperature_level <- factor(df_all$temperature_level, levels = c("Low site", "High site ambient", "High site warmed"))


df_all <- df_all |> 
  mutate(
    emmean_resp = exp(emmean),
    SE_resp = emmean_resp * SE  # approximate delta method for error bars
  )

# Plot
j <- ggplot(df_all, aes(x = temperature_level, y = emmean_resp,
                   color = treat_competition, shape = experiment, group = interaction(treat_competition, experiment))) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_line(position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = emmean_resp - SE_resp, ymax = emmean_resp + SE_resp),
                width = 0.2, position = position_dodge(width = 0.3)) +
  labs(x = "", y = "Predicted flower number\n(adjusted for biomass)", color = "Biotic interactions", shape = "Experiment") +
  scale_color_manual(values = c("#528B8B", "#CD950C")) 
j

# ggsave(filename = "Output/Biomass/Cooling_Warming_flower_number_adjsuted_for_biomass.png", 
#        plot = j, width = 12, height = 8, units = "in")



