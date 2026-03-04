

# RangeX biomass - traits ------------

## Data used: 
## Date:      03.03.26
## Author:    Nadine Arzt
## Purpose:   Biomass and trait regressions


# comment -----------------------------------------------------------------

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)


# import metadata ---------------------------------------------------------
meta_NOR <- read.csv("Data/RangeX_clean_MetadataFocal_NOR.csv")


# import biomass data -------------------------------------------------------------
biomass_high <- read.csv("Data/Raw/RangeX_raw_NOR_biomass_high_2024.csv")

biomass_low <- read.csv("Data/Raw/RangeX_raw_NOR_biomass_low_2024.csv")


# add column site ---------------------------------------------------------
biomass_high <- biomass_high |> 
  mutate(site = "hi")

biomass_low <- biomass_low |> 
  mutate(site = "lo")


# delete superfluous columns -----------------------------------------------
biomass_high <- biomass_high |> 
  select(-c(X.1, X.2, X.3, X.4))

biomass_low <- biomass_low |> 
  select(-X)

# why is there a column f now?
# this is plot_ID
# change to same as in high to merge
biomass_low <- biomass_low |> 
  rename("X" = "f")

# combine low and high ----------------------------------------------------
biomass <- bind_rows(biomass_high, biomass_low)



# rename columns to match meta --------------------------------------------
biomass <- biomass |> 
  rename("block_ID_original" = "X",
         "plot_ID_original" = "treat",
         "position_ID_original" = "coord")


# merge biomass with metadata ---------------------------------------------
biomass_NOR <- left_join(meta_NOR, biomass, by = c("site", "block_ID_original",
                                                   "plot_ID_original",
                                                   "position_ID_original",
                                                   "species"))


# check structure ---------------------------------------------------------
glimpse(biomass_NOR)
# weight is all chr


# change to numeric and . -------------------------------------------------
biomass_NOR <- biomass_NOR |> 
  mutate(
    across(
      starts_with("dry_weight"),
      ~ .x |>
        str_replace(",", ".") |>
        as.numeric()
    )
  )
  
# combined treatment column -----------------------------------------------
biomass_NOR$treatment <- paste(biomass_NOR$site, biomass_NOR$treat_warming, 
                               biomass_NOR$treat_competition, sep = "_")


# first control plot of total weight --------------------------------------
ggplot(biomass_NOR,
       aes(x = species, y = dry_weight_total_g)) +
  geom_boxplot()+
  geom_jitter()

ggplot(biomass_NOR,
       aes(x = treatment, y = dry_weight_total_g)) +
  geom_boxplot()+
  geom_jitter()


ggplot(biomass_NOR, aes(x = dry_weight_total_g, y = dry_weight_leaves_g,
                        colour = species))+
  geom_point()





# import trait data 2024 --------------------------------------------------
trait_hi <- read.csv("Data/Raw/RangeX_raw_demographic_traits_high_2024.csv")

trait_lo <- read.csv("Data/Raw/RangeX_raw_demographic_traits_low_2024.csv")


# make heights numeric ----------------------------------------------------
trait_hi <- trait_hi |> 
  mutate(
    across(
      starts_with("height"),
      ~ .x |>
        as.numeric()
    )
  )

trait_lo <- trait_lo |> 
  mutate(
    across(
      starts_with("height"),
      ~ .x |>
        as.numeric()
    )
  )

trait_lo <- trait_lo |> 
  rename("block" = "X")

# add column site ---------------------------------------------------------
trait_hi <- trait_hi |> 
  mutate(site = "hi")

trait_lo <- trait_lo |> 
  mutate(site = "lo")


# combine hi and lo -------------------------------------------------------
traits <- bind_rows(trait_hi, trait_lo)


# rename column names -----------------------------------------------------
traits <- traits |> 
  rename("block_ID_original" = "block",
         "plot_ID_original" = "treat",
         "position_ID_original" = "coord")


# merge traits with metadata ---------------------------------------------
traits_NOR <- left_join(meta_NOR, traits, by = c("site", "block_ID_original",
                                                   "plot_ID_original",
                                                   "position_ID_original",
                                                   "species"))


# combined treatment column -----------------------------------------------
traits_NOR$treatment <- paste(traits_NOR$site, traits_NOR$treat_warming, 
                              traits_NOR$treat_competition, sep = "_")




# replace # leaves with harvested -----------------------------------------
traits_NOR <- traits_NOR |>
  mutate(
    no..leaves.tillers.if.different...harvest = 
      as.numeric(no..leaves.tillers.if.different...harvest),
    no..leaves.tillers = if_else(
      !is.na(no..leaves.tillers.if.different...harvest),
      no..leaves.tillers.if.different...harvest,
      no..leaves.tillers
    )
  )


traits_NOR <- traits_NOR |>
  mutate(
    no..stems.if.different...harvest = 
      as.numeric(no..stems.if.different...harvest),
    no..stems = if_else(
      !is.na(no..stems.if.different...harvest),
      no..stems.if.different...harvest,
      no..stems
    )
  )



# first control plot of total weight --------------------------------------
ggplot(traits_NOR,
       aes(x = species, y = no..leaves.tillers)) +
  geom_boxplot()+
  geom_jitter()




# rename columns ----------------------------------------------------------
traits_NOR <- traits_NOR |>
  rename(
    # Heights
    height_vegetative       = height.veg..cm.,
    height_vegetative_str   = height.veg.stretch..cm.,
    height_reproductive     = height.rep..cm.,
    height_reproductive_str = height.rep.stretch..cm.,
    height_Nathan_stretch_cm = height.Nathan.stretch..cm.,
    
    # Leaves / tillers
    number_leaves_tillers = no..leaves.tillers,
    
    # Stems
    no_stems = no..stems,
    
    # Herbivory
    herbivory_leaf = herbivory.leaf,
    herbivory_flower  = herbivory.flower,
    herbivory  = herbivory.if.different...harvest,
    
    sampled_quarter = sampled.quarter,
    sampled_half = sampled.half)



# sampled quarter/half ---------------------------------------------------------
# where sampled quarter, multiply by 4
# half by 2
traits_NOR$number_leaves <- ifelse(
  traits_NOR$sampled_quarter == "yes",
  traits_NOR$number_leaves_tillers * 4,
  ifelse(
    traits_NOR$sampled_half == "yes",
    traits_NOR$number_leaves_tillers * 2,
    traits_NOR$number_leaves_tillers))

# so number_leaves is the actual one











# combine biomass with traits ---------------------------------------------
key <- "unique_plant_ID"

traits_vars <- traits_NOR |>
  select(
    unique_plant_ID,
    height_vegetative,
    height_vegetative_str,
    height_reproductive,
    height_reproductive_str,
    height_Nathan_stretch_cm,
    number_leaves_tillers,
    number_leaves,
    no..leaves.tillers.if.different...harvest,
    no_stems,
    no..stems.if.different...harvest,
    herbivory_leaf,
    herbivory_flower
  )


biomass_traits_NOR <- biomass_NOR |>
  left_join(traits_vars, by = key)



# plot --------------------------------------------------------------------

ggplot(biomass_traits_NOR, aes(x = dry_weight_total_g, y = height_vegetative_str))+
  geom_point()

ggplot(biomass_traits_NOR, aes(x = dry_weight_total_g, y = height_reproductive_str))+
  geom_point()

ggplot(biomass_traits_NOR, aes(x = dry_weight_total_g, y = number_leaves, colour =  species))+
  geom_point()

ggplot(biomass_traits_NOR, aes(x = dry_weight_total_g, y = number_leaves_tillers, colour =  species))+
  geom_point()


# biomass - trait - regression --------------------------------------------

# filter out plants where weigth is 0
biomass_traits <- biomass_traits_NOR |>
  filter(dry_weight_total_g > 0)

# test distribution of biomass total
hist(biomass_traits$dry_weight_total_g)
hist(log(biomass_traits$dry_weight_total_g))


qqnorm(biomass_traits$dry_weight_total_g); qqline(biomass_traits$dry_weight_total_g)
qqnorm(log(biomass_traits$dry_weight_total_g)); qqline(log(biomass_traits$dry_weight_total_g))

# log biomass
biomass_traits <- biomass_traits |>
  mutate(log_biomass = log(dry_weight_total_g))



# models ------------------------------------------------------------------

m_height <- lmerTest::lmer(log_biomass ~ height_vegetative + height_reproductive +
    (1|species) + (1|block_ID),
  data = biomass_traits
)

summary(m_height)

cor(biomass_traits$height_vegetative,
    biomass_traits$height_reproductive,
    use="complete.obs")






# make dataset for models that include plants that have all traits --------
# use only plants that have all the traits available 
analysis_data <- biomass_traits |>
  select(
    log_biomass,
    height_vegetative,
    height_vegetative_str,
    height_reproductive,
    height_reproductive_str,
    no_stems,
    number_leaves,
    species,
    block_ID
  ) |>
  na.omit()


# models individual per trait ---------------------------------------------
m_height_v <- lmerTest::lmer(log_biomass ~ height_vegetative + (1|species) + (1|block_ID), data=analysis_data)
summary(m_height_v)

m_height_vs<- lmerTest::lmer(log_biomass ~ height_vegetative_str + (1|species) + (1|block_ID), data= analysis_data)
summary(m_height_vs)


m_leaves <- lmerTest::lmer(log_biomass ~ number_leaves + (1|species) + (1|block_ID), data=analysis_data)
summary(m_leaves)

m_height_r<- lmerTest::lmer(log_biomass ~ height_reproductive + (1|species) + (1|block_ID), data= analysis_data)
summary(m_height_r)

m_height_rs<- lmerTest::lmer(log_biomass ~ height_reproductive_str + (1|species) + (1|block_ID), data= analysis_data)
summary(m_height_rs)

m_stems <- lmerTest::lmer(log_biomass ~ no_stems + (1|species) + (1|block_ID), data= analysis_data)
summary(m_stems)

AIC(m_height_v, m_height_vs, m_leaves,m_height_r, m_height_rs, m_stems)
# no stems is the best model

# Does height add to stems ------------------------------------------------
m_stems_height <- lmerTest::lmer(log_biomass ~ no_stems + height_reproductive_str + (1|species) + (1|block_ID), data= analysis_data)
summary(m_stems_height)

AIC(m_stems, m_stems_height)


m_stems_height2 <- lmerTest::lmer(log_biomass ~ no_stems * height_reproductive_str + (1|species) + (1|block_ID), data= analysis_data)
summary(m_stems_height2)

AIC(m_stems, m_stems_height, m_stems_height2)

# m_stems_height2 seems to be the best fit
# meaning the interaction between no stems and rep height str






















