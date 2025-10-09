

# Seeds exploration phase ----------------------------------------

## Data used: RangeX_clean_phenology_2023_NOR.csv
##            RangeX_metadata_focal_NOR.csv
##            RangeX_clean_seeds_NOR_2023.csv
## Date:      08.10.25
## Author:    Nadine Arzt
## Purpose:   Explore if it makes sense to include seed data


# comments ----------------------------------------------------------------
# we have seed count and weight for 336 individuals
# can't include Siljes data because she only used bare plots --> not balanced


# load library ------------------------------------------------------------
library(lme4)
library(ggeffects)
library(broom.mixed)
library(emmeans)
library(lubridate)


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

phenology <- phenology |>
  mutate(treat_warming = case_when(
    treat_warming == "ambi" ~ "ambient",
    treat_warming == "warm" ~ "warmed",
    TRUE ~ treat_warming
  ))

phenology <- phenology |>
  mutate(site = case_when(
    site == "lo" ~ "low",
    site == "hi" ~ "high",
    TRUE ~ site
  ))

# filter only NOR ---------------------------------------------------------
phenology_NOR <- phenology |> 
  filter(region == "Norway")


# # filter only hi ambi and lo -----------------------------------------------
# phenology_NOR_ambi <- phenology_NOR |> 
#   filter(treat_warming == "ambient")


# and get julian days --------------------------------------
phenology_NOR<- phenology_NOR |> 
  mutate(jday = yday(date_measurement),   # Julian day (1–365)
         jday_scaled = scale(jday))   



# only flowers ----------------------------------------------------------
phenology_NOR_flowers <- phenology_NOR |> 
  filter(phenology_stage == "No_FloOpen")



# Import seed data ------------------------------------------------------------
seed_nor <- read.csv("Data/RangeX_clean_seeds_NOR_2023.csv")




# join phenology and seed data --------------------------------------------
pheno_seed <- left_join(phenology_NOR_flowers, seed_nor, by = c("unique_plant_ID", "species"))




# Subset to plants with seed data -----------------------------------------
seed_subset <- pheno_seed |> 
  filter(!is.na(no_seeds), !is.na(value), phenology_stage == "No_FloOpen")



# first plotting ----------------------------------------------------------
# Plot number of flowers vs. seeds
ggplot(seed_subset, aes(x = value, y = no_seeds)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Number of flowers open",
    y = "Number of seeds",
    title = "Relationship between flowering and seed production"
  )

s <- ggplot(seed_subset, aes(x = value, y = no_seeds)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  scale_y_log10() +  # log scale to reduce outlier influence
  scale_x_log10() +
  labs(x = "Number of flowers (log)", y = "Number of seeds (log)")+
  facet_wrap(~ date_measurement, nrow = 2)
s
# no relationship even when log transforming the data
# but if we facet by date it makes more sense

ggsave(filename = "Output/Number_flowers_vs_number_seeds_date_NOR.png", plot = s, width = 20, height = 8, units = "in")



# flower number vs seed weight
ggplot(seed_subset, aes(x = value, y = seedweight)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Number of flowers open",
    y = "Seed weight",
    title = "Relationship between flowering and seed production"
  )

ggplot(seed_subset, aes(x = value, y = seedweight)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  scale_y_log10() +  # log scale to reduce outlier influence
  scale_x_log10() +
  labs(x = "Number of flowers (log)", y = "Seed weight (log)")



# try with max no flowers --------------------------------------------------

flower_summary <- pheno_seed |> 
  group_by(unique_plant_ID) |> 
  summarise(max_flowers = max(value, na.rm = TRUE),
            no_seeds = first(no_seeds),
            seedweight = first(seedweight),
            site = first(site),
            treat_warming = first(treat_warming),
            treat_competition = first(treat_competition),
            treatment = first(treatment),
            species = first(species),
            .groups = "drop")

# seed number vs max flower number
ggplot(flower_summary, aes(x = max_flowers, y = no_seeds)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10() + scale_y_log10()

ggplot(flower_summary, aes(x = max_flowers, y = no_seeds)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10() + scale_y_log10()+
  facet_grid(~ treatment)

# seed weight vs max flower number
ggplot(flower_summary, aes(x = max_flowers, y = seedweight)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10() + scale_y_log10()

ggplot(flower_summary, aes(x = max_flowers, y = seedweight)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10() + scale_y_log10()+
  facet_grid(~ treatment)



# seed weight vs seed number? ---------------------------------------------

ggplot(flower_summary, aes(x = no_seeds, y = seedweight)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10() + scale_y_log10()+
  facet_grid(~ treatment)

# hm, alsways positive correlations but should it not be the opposite?






# models -------------------------------------------------------------------

flower_summary_2 <- seed_subset |> 
  group_by(unique_plant_ID) |> 
  summarise(max_flowers = max(value, na.rm = TRUE),
            no_seeds = first(no_seeds),
            seedweight = first(seedweight),
            site = first(site),
            treat_warming = first(treat_warming),
            treat_competition = first(treat_competition),
            treatment = first(treatment),
            species = first(species),
            .groups = "drop")
# seed number
m_seed_flower <- lmerTest::lmer(no_seeds ~ max_flowers * site * treat_warming * treat_competition + (1 | species), data = flower_summary_2)
summary(m_seed_flower)

plot(ggpredict(m_seed_flower, terms = "max_flowers"))



# use combined treatment column
# yes, but makes no sense
m2_seed_flower <- lmerTest::lmer(no_seeds ~ max_flowers *treatment + (1 | species), data = flower_summary_2)
summary(m2_seed_flower)

plot(ggpredict(m2_seed_flower, terms = "max_flowers"))






# seed weight
m_seedweight_flower <- lmer(seedweight ~ max_flowers * site * treat_warming * treat_competition + (1 | species), 
                            data = flower_summary)
summary(m_seedweight_flower)

plot(ggpredict(m_seedweight_flower, terms = "max_flowers"))






# cooling: high vs low ----------------------------------------------------
# filter only low and high ambient to exclude warming 
flower_summary_cooling <- flower_summary_2 |> 
  filter(treat_warming == "ambient")


# seed number
m_seed_flower_cool <- lmerTest::lmer(no_seeds ~ max_flowers * site * treat_competition + (1 | species), data = flower_summary_cooling)
summary(m_seed_flower_cool)

plot(ggpredict(m_seed_flower_cool, terms = "max_flowers"))

# seed weight
m_seedweight_flower_cool <- lmerTest::lmer(seedweight ~ max_flowers * site * treat_competition + (1 | species), data = flower_summary_cooling)
summary(m_seedweight_flower_cool)

plot(ggpredict(m_seedweight_flower_cool, terms = "max_flowers"))



# warming: hi ambi vs hi warm ---------------------------------------------
flower_summary_warming <- flower_summary_2 |> 
  filter(site == "high")

# seed number
m_seed_flower_warm <- lmerTest::lmer(no_seeds ~ max_flowers * treat_warming * treat_competition + (1 | species), data = flower_summary_warming)
summary(m_seed_flower_warm)

plot(ggpredict(m_seed_flower_warm, terms = "max_flowers"))


# seed weight
m_seedweight_flower_warm <- lmerTest::lmer(seedweight ~ max_flowers * treat_warming * treat_competition + (1 | species), data = flower_summary_warming)
summary(m_seedweight_flower_warm)

plot(ggpredict(m_seedweight_flower_warm, terms = "max_flowers"))






















