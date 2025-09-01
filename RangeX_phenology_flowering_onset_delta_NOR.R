

# delta flwowering onset NOR ----------------------------------------------



# filter only high --------------------------------------------------------
phenology_nor_pres_abs_hi <- phenology_nor_pres_abs |>
  filter(site == "hi")



# flowering onset nor ---------------------------------------------------------
flowering_onset_nor <- phenology_nor_pres_abs_hi |> 
  filter(phenology_stage == "number_flowers", presence == 1) |>   # keep only flowering observations
  group_by(species, unique_plant_ID, site_treatment, block_ID) |> 
  summarize(onset_date = min(date_measurement),  # first date flowering occurs
            .groups = "drop")

flowering_onset_nor <- flowering_onset_nor %>%
  mutate(onset_date = as.numeric(onset_date))   # convert Date → number of days

# convert into julian days nor --------------------------------------------
flowering_onset_nor <- flowering_onset_nor |>
  mutate(onset_julian = as.numeric(strftime(onset_date, format = "%j")))

flowering_onset_treatment_nor <- flowering_onset_nor |> 
  group_by(site_treatment) |> 
  summarize(mean_onset_julian = median(onset_julian, na.rm = TRUE),
            sd_onset_julian = sd(onset_julian, na.rm = TRUE),
            n = n(),
            .groups = "drop")


# calculate delta flowering onset --------------------------------------
delta_onset <- flowering_onset_nor %>%
  separate(site_treatment, into = c("site", "temp", "competition"), sep = " ", remove = FALSE) %>%
  select(species, unique_plant_ID, block_ID, site, temp, competition, onset_date) %>%
  group_by(species, site, competition, block_ID) %>%
  summarize(
    delta_onset = median(onset_date[temp == "warm"], na.rm = TRUE) -
      mean(onset_date[temp == "ambi"], na.rm = TRUE),
    .groups = "drop"
  )

# plot delta -----------------------------------------------------------
ggplot(delta_onset, aes(x = competition, y = delta_onset, fill = competition)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(y = "Δ Flowering onset (warm - ambi, days)", x = "Competition") +
  theme_minimal()



# per species
ggplot(delta_onset, aes(x = competition, y = delta_onset, fill = competition)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(y = "Δ Flowering onset (warm - ambi, days)", x = "Competition") +
  facet_wrap(~ species)+
  theme_bw()







































