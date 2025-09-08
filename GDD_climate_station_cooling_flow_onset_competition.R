

# Growing degree days (GDD) -----------------------------------------------

# RangeX phenology effect of cooling NOR and CHE ------------

## Data used: RangeX_clean_Phenology_2022_CHE.csv
##            RangeX_clean_phenology_2023_NOR.csv
##            RangeX_clean_MetadataFocal_CHE.csv
##            RangeX_metadata_focal_NOR.csv
## Date:      02.09.25
## Author:    Nadine Arzt
## Purpose:   Effect of cooling on flowering onset NOR and CHE - compare julian days with gdd


# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(lubridate)
library(slider) 


# get phenology data ------------------------------------------------------

# load clean phenology data -----------------------------------------------
source("RangeX_phenology_NOR_CHE_data_combination.R")

# use this data set
names(phenology)

# Filter buds, flowers, infructescences -----------------------------------
## don't use seeds_collected
phenology <- phenology |> 
  filter(phenology$phenology_stage != "No_Seeds")

# combined treatment column -----------------------------------------------
phenology$treatment <- paste(phenology$site, phenology$treat_warming, phenology$treat_competition, sep = "_")


# change region and treatment names  --------------------------------------
phenology <- phenology |>
  mutate(region = case_when(
    region == "NOR" ~ "Norway",
    region == "CHE" ~ "Switzerland",
    TRUE ~ region
  ))

phenology <- phenology |>
  mutate(treat_competition = case_when(
    treat_competition == "bare" ~ "without",
    treat_competition == "vege" ~ "with",
    TRUE ~ treat_competition
  ))


# filter only nor ------------------------------------------------------
phenology_nor <- phenology |> 
  filter(region == "Norway")



unique(phenology_nor$date_measurement)
# start: 2023-06-07
# end: 2023-09-13


# import cliamte station data NOR -----------------------------------------
climate <- read.csv("Data/RangeX_clean_climate_station_NOR_2021-2025.csv", row.names = 1)



# filter year 23 -------------------------------------------------------
climate_23 <- climate |> 
  filter(year == 2023)


# calculate Tmax, Tmin and Temp average per day -----------------
climate_23_daily <- climate_23 |> 
  mutate(date = as_date(date_time)) |> 
  group_by(site, date) |> 
  summarise(Tmax = max(AirTemp_Avg, na.rm = TRUE),
            Tmin = min(AirTemp_Avg, na.rm = TRUE),
            .groups = "drop") |> 
  mutate(Tavg = (Tmax + Tmin) / 2)



# filter only 2023 -----------------------------------------------------
climate_23_daily <- climate_23_daily |> 
  filter(date >= as.Date("2023-01-01"))


# define Tbase and growing season start  -------------------------
Tbase <- 5 # base temperature for plants to grow
Nconsec <- 5 # number of consecutive days above Tbase to define growing season start
# three days above 5 degrees


# 1) indicator: Tavg > Tbase
clim_flag <- climate_23_daily |> 
  arrange(site, date) |> 
  mutate(is_warm = (Tavg > Tbase))


# find out start of growing season -------------------------------------
# 2) compute run of Nconsec TRUE per site and find first date
season_start <- clim_flag |> 
  group_by(site) |> 
  mutate(run_N = slider::slide_dbl(is_warm, ~ ifelse(sum(.x) == length(.x), 1, 0),
                                   .before = Nconsec - 1, .complete = TRUE)) |> 
  # run_N == 1 on the last day of a full Nconsec-run of TRUEs
  filter(run_N == 1) |> 
  summarise(season_start = min(date), .groups = "drop")


# calculate gdd per site ----------------------------------------------
# 3) join start dates back and calculate GDD from that start in april
climate_gdd <- climate_23_daily |> 
  left_join(season_start, by = "site") |> 
  filter(!is.na(season_start)) |> 
  group_by(site) |> 
  arrange(date) |> 
  mutate(
    # only accumulate on/after season_start
    GDD_day = if_else(date >= season_start, pmax(0, Tavg - Tbase), 0),
    GDD_cum = cumsum(GDD_day)
  ) |> 
  ungroup()


# # calculate gdd per site -----------------------------------------
# gdd_daily <- climate_23_daily |> 
#   mutate(GDD_day = pmax(0, Tavg - Tbase))|>    # daily GDD
#   group_by(site)|> 
#   arrange(date) |> 
#   mutate(GDD_cum = cumsum(GDD_day)) |>        # cumulative over season
#   ungroup()


# rename date to date_measurement --------------------------------------
# to match phenology
climate_gdd <- climate_gdd |> 
  rename("date_measurement" = "date")


# filter by timeframe from phenology ----------------------------------
climate_gdd_pt <- climate_gdd |> 
  filter(date_measurement >= as.Date("2023-05-12"),
         date_measurement <= as.Date("2023-10-23"))




# combine gdd_cum and phenology_nor ------------------------------------
phenology_with_gdd <- phenology_nor |> 
  left_join(climate_gdd_pt |> 
              select(site, date_measurement, GDD_cum),
            by = c("site", "date_measurement" = "date_measurement"))


# filter only ambi both sites  -------------------------------------
# to compare low ambi with hi ambi = cooling effect
phenology_with_gdd_ambi <- phenology_with_gdd |> 
  filter(treat_warming == "ambi")

# and get julian days --------------------------------------
phenology_with_gdd_ambi <- phenology_with_gdd_ambi |> 
  mutate(jday = yday(date_measurement),   # Julian day (1–365)
         jday_scaled = scale(jday))        # optional scaling if you need for models 


# calculate flowering onset ------------------------------------------------
flowering_onset_gdd_ambi <- phenology_with_gdd_ambi |> 
  filter(phenology_stage == "No_FloOpen", value > 0) |>
  group_by(site, species, unique_plant_ID, block_ID, treat_competition) |>
  summarise(onset = min(GDD_cum, na.rm = TRUE), .groups = "drop") |>
  # remove groups where flowering never occurred
  filter(is.finite(onset))


# model flowering onset nor lmer ------------------------------------------
m_onset_gdd_ambi <- lmerTest::lmer(onset ~ site * treat_competition + (1|species) + (1|block_ID),
                                   data = flowering_onset_gdd_ambi)
summary(m_onset_gdd_ambi)


# plot point with residuals for effect of warming on flo onset -------------
# estimated marginal means
emm_nor_gdd_ambi <- emmeans(m_onset_gdd_ambi, ~ site | treat_competition)

contr_nor_gdd_ambi <- contrast(emm_nor_gdd_ambi, method = list("lo - hi" = c(1, -1)))

# using summary keeps the p-values
contrast_df_nor_gdd_ambi <- as.data.frame(summary(contr_nor_gdd_ambi, infer = TRUE))


ggplot(contrast_df_nor_gdd_ambi, aes(x = treat_competition, y = estimate, color = treat_competition)) +
  geom_point(size = 8) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = ifelse(p.value < 0.001, "***",
                               ifelse(p.value < 0.01, "**",
                                      ifelse(p.value < 0.05, "*", "n.s.")))),
            vjust = -1, , position = position_nudge(x = 0.1),
            show.legend = FALSE,
            size = 8) +
  labs(x = "Biotic interaction",
       y = "Δ GDD shifted flowering onset (high - low)",
       title = "Effect of cooling on flowering onset - NOR",
       color = "Biotic interaction")+
  scale_color_manual(values = c("#528B8B", "#CD950C"))+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))
#





















