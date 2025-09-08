
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
    treat_competition == "bare" ~ "without competition",
    treat_competition == "vege" ~ "with competition",
    TRUE ~ treat_competition
  ))


# filter only nor --------------------------------------------------------
phenology_nor <- phenology |> 
  filter(region == "Norway")



unique(phenology_nor$date_measurement)
# start: 2023-06-07
# end: 2023-09-13


# filter lo ambi and hi ambi ----------------------------------------------
phenology_nor_ambi <- phenology_nor |> 
  filter(treat_warming == "ambi")

# # filter only vege -----------------------------------------------------
# phenology_nor_ambi_vege <- phenology_nor_ambi |> 
#   filter(treat_competition == "with competition")

# and get julian days --------------------------------------
phenology_nor_ambi <- phenology_nor_ambi |> 
  mutate(jday = yday(date_measurement),   # Julian day (1–365)
    jday_scaled = scale(jday))        # optional scaling if you need for models 


# filter by timeframe from tomst --------------------------------------
phenology_nor_ambi_pt <- phenology_nor_ambi |> 
  filter(date_measurement >= as.Date("2023-07-04"),
         date_measurement <= as.Date("2023-10-10"))

# but tomst is only from 21.06 to 22.10
# so better 2023-07-04 - 2023-10-10



# import tomst data 23 -----------------------------------------------
tomst_23 <- read.csv("Data/RangeX_clean_tomst_NOR_2023.csv", , row.names = 1)

# import meta data plot ---------------------------------------------------
meta_plot <- read.csv("Data/RangeX_metadata_plot_NOR.csv", , row.names = 1)

# combine meta plot with tomst --------------------------------------------
tomst_23_NOR <- left_join(meta_plot, tomst_23, by = c("unique_plot_ID"))


# filter lo ambi and hi ambi vege ---------------------------------
tomst_23_NOR_ambi <- tomst_23_NOR |> 
  filter(treat_warming == "ambi") 
  #filter(treat_competition == "vege")



# calculate Tmax, Tmin and Temp average per day --------------------------------
tomst_23_daily <- tomst_23_NOR_ambi |> 
  filter(!is.na(date_time)) |> # filter out rows with na
  mutate(date = as_date(date_time)) |> 
  group_by(site, treat_competition, date) |> 
  summarise(Tmax = max(TMS_T3, na.rm = TRUE),
            Tmin = min(TMS_T3, na.rm = TRUE),
            .groups = "drop") |> 
  mutate(Tavg = (Tmax + Tmin) / 2)

# filter by timeframe from pehnology --------------------------------------
tomst_23_daily_pt <- tomst_23_daily |> 
  filter(date >= as.Date("2023-07-04"),
         date <= as.Date("2023-10-10"))


# calculate gdd per site --------------------------------------------------
Tbase <- 5

gdd_daily <- tomst_23_daily_pt |> 
  mutate(GDD_day = pmax(0, Tavg - Tbase))|>    # daily GDD
  group_by(site, treat_competition)|> 
  arrange(date) |> 
  mutate(GDD_cum = cumsum(GDD_day)) |>        # cumulative over season
  ungroup()


# rename date to date_measurement -------------------------------
# to match phenology
gdd_daily <- gdd_daily |> 
  rename("date_measurement" = "date")

gdd_daily <- gdd_daily |>
  mutate(treat_competition = case_when(
    treat_competition == "bare" ~ "without competition",
    treat_competition == "vege" ~ "with competition",
    TRUE ~ treat_competition
  ))

# combine gdd_cum and phenology_nor ---------------------------------------
phenology_with_gdd <- phenology_nor_ambi_pt |> 
  left_join(gdd_daily |> 
              select(site, 
                     date_measurement, GDD_cum),
            by = c("site", "date_measurement" = "date_measurement"))

##


# calculate flowering onset ------------------------------------------------
flowering_onset_gdd_ambi <- phenology_with_gdd |> 
  filter(phenology_stage == "No_FloOpen", value > 0) |>
  group_by(site, species, unique_plant_ID, block_ID, treat_competition) |>
  summarise(onset = min(GDD_cum, na.rm = TRUE), .groups = "drop") |>
  # remove groups where flowering never occurred
  filter(is.finite(onset))


# model flowering onset nor lmer ------------------------------------------
m_onset_gdd_ambi <- lmerTest::lmer(onset ~ site * treat_competition + (1|species) + (1|block_ID), data = flowering_onset_gdd_ambi)
summary(m_onset_gdd_ambi)

# plot point with residuals for effect of warming on flo onset -------------
# estimated marginal means
emm_nor_gdd_ambi <- emmeans(m_onset_gdd_ambi, ~ site | treat_competition)

contr_nor_gdd_ambi <- contrast(emm_nor_gdd_ambi, method = list("lo - hi" = c(1, -1)))


# using summary keeps the p-values
contrast_df_nor_gdd_ambi <- as.data.frame(summary(contr_nor_gdd_ambi, infer = TRUE))


ggplot(contrast_df_nor_gdd_ambi, aes(x = treat_competition, y = estimate, color = treat_competition)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = ifelse(p.value < 0.001, "***",
                               ifelse(p.value < 0.01, "**",
                                      ifelse(p.value < 0.05, "*", "n.s.")))),
            vjust = -2, show.legend = FALSE) +
  labs(x = "",
       y = "Δ GDD shifted flowering onset (high - low)",
       title = "Effect of cooling on flowering onset - NOR")+
  scale_color_manual(values = c("#528B8B", "#CD950C"))
#























