
# BIOMASS 7 ---------------------------------------------------------------

# Adding predicted biomass to phenology ------------

## Data used: 
## Date:      03.03.26
## Author:    Nadine Arzt
## Purpose:   Add pred log biomass to phenology max flowers per plant


# load library ------------------------------------------------------------
library(ggeffects)

theme_set(theme_bw(base_size = 22))


# source script that predicts the 23 biomass
source("Biomass_prediction_for_23.R")

# use rangex_traits_23_pb



# import pheno 23 data --------------------------------------------------------
# this is the latest clean version of phenology data
# what changed is the unique_plant_ID because I added the number in the end 
# a 1 since all plants survived without being replaced
phenol <- read.csv("Data/Clean/RangeX_clean_Phenology_2023_NOR.csv")


# import metadata NOR -----------------------------------------------------
meta_NOR <- read.csv("Data/RangeX_clean_MetadataFocal_NOR.csv")


# combine pheno with metadata ---------------------------------------------
phenol_23_NOR <- left_join(meta_NOR, phenol, by = c("unique_plant_ID", "species"))


# rename pheno stages to match regions ------------------------------------
# "number_infructescences" in NOR correpsonfs to "No_FloWithrd" in CHE?
phenol_23_NOR <- phenol_23_NOR |>
  mutate(phenology_stage = recode(phenology_stage,
                                  "number_buds" = "No_Buds",
                                  "number_flowers" = "No_FloOpen",
                                  #"number_infructescences" = "No_FloWithrd",
                                  "seeds_collected" = "No_Seeds"))



rangex_traits_23_pb

bio_23 <- rangex_traits_23_pb[, c("unique_plant_ID",
                                  "pred_log_biomass")]


any(duplicated(bio_23$unique_plant_ID))


# combine pheno with pred biomass -----------------------------------------
phenol_23_NOR_bio <- phenol_23_NOR |>
  left_join(bio_23,
            by = "unique_plant_ID")




# plot pred biomass against max no of flowers -----------------------------
# get max number of flowers per plant
flowers_per_plant <- phenol_23_NOR_bio |>
  group_by(unique_plant_ID, species) |>
  summarise(flowers_max = max(value, na.rm = TRUE))



bio_flower <- rangex_traits_23_pb |>
  left_join(flowers_per_plant, by = c("unique_plant_ID", "species"))


# vizualization
ggplot(bio_flower, aes(x = flowers_max, y = pred_log_biomass)) +
  geom_point(alpha = 0.5, color = "darkred") +
  facet_wrap(~ species) +
  labs(
    x = "Maximum number of flowers",
    y = "Predicted log(biomass)",
    title = "Predicted biomass vs. flower production"
  )

# looks like a positive trend for all species
# treatment not included



# filter out max flower number 0 ----------------------------------------------
bio_flower <- bio_flower |>
  filter(flowers_max > 0)

hist(bio_flower$flowers_max)
hist(log(bio_flower$flowers_max))


# log max flower -----------------------------------------------------------
qqnorm(bio_flower$flowers_max); qqline(bio_flower$flowers_max)
qqnorm(log(bio_flower$flowers_max)); qqline(log(bio_flower$flowers_max))

# log 
bio_flower <- bio_flower |>
  mutate(log_max_flower = log(flowers_max))




# fit model ---------------------------------------------------------------
# to see if the observed trend is significant
m_flowers <- lmerTest::lmer(pred_log_biomass ~ log_max_flower + (1|species) + (1|block_ID),
  data = bio_flower)
summary(m_flowers)

# it is significant: <2e-16 ***

# plot per species
# plot biomass vs max flowers and the trend with a lm
ggplot(bio_flower, aes(x = log_max_flower, y = pred_log_biomass)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  #facet_wrap(~ species) +
  labs(x = "Log (max number of flowers)",
       y = "Predicted log(biomass)")


# so we know that a plant produces significantly more flowers when it is bigger



# differences between species? --------------------------------------------

m_flowers_sp <- lmerTest::lmer(pred_log_biomass ~ log_max_flower * species + (1|block_ID),
  data = bio_flower)
anova(m_flowers, m_flowers_sp)



ggplot(bio_flower, aes(x = log_max_flower, y = pred_log_biomass)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ species) +
  labs(x = "Log (max number of flowers)",
       y = "Predicted log(biomass)")





# plot with correct model  ------------------------------------------------
# and confidence intervals
pred_df <- ggpredict(m_flowers, terms = "log_max_flower")

b <- ggplot() +
  geom_point(data = bio_flower,
             aes(x = log_max_flower, y = pred_log_biomass),
             alpha = 0.4) +
  geom_line(data = pred_df,
            aes(x = x, y = predicted),
            color = "turquoise4", size = 1.2) +
  geom_ribbon(data = pred_df,
              aes(x = x, ymin = conf.low, ymax = conf.high),
              alpha = 0.2, fill = "turquoise4") +
  labs(x = "Log(max number of flowers)",
       y = "Predicted log(biomass)")
b
# the points are the actual log data, the line is the model
# title = "Mixed-model prediction with 95% CI"

ggsave(filename = "Output/Biomass/Log_pred_biomass_log_max_flowers_lmm.png", 
       plot = b, width = 13, height = 9, units = "in")








