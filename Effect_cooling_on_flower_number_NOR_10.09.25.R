
# Effect of transplantation on flower number ---------------------------------

## Data used: RangeX_clean_Phenology_2022_CHE.csv
##            RangeX_clean_phenology_2023_NOR.csv
##            RangeX_clean_MetadataFocal_CHE.csv
##            RangeX_metadata_focal_NOR.csv
## Date:      02.09.25
## Author:    Nadine Arzt
## Purpose:   Effect of transplantation on flower number NOR


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


# set theme for plots for presentation ------------------------------------
theme_set(theme_bw(base_size = 20))

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


# filter only hi ambi and lo -----------------------------------------------
phenology_NOR_ambi <- phenology_NOR |> 
  filter(treat_warming == "ambient")


# and get julian days --------------------------------------
phenology_NOR_ambi <- phenology_NOR_ambi |> 
  mutate(jday = yday(date_measurement),   # Julian day (1–365)
         jday_scaled = scale(jday))   



# only flowers ----------------------------------------------------------
phenology_NOR_ambi_flowers <- phenology_NOR_ambi |> 
  filter(phenology_stage == "No_FloOpen")


# model --------------------------------------------------------------
m_flower_number_cool <- lmerTest::lmer(value ~ site * treat_competition +
                                    (1|species) + (1|block_ID),
                                  data = phenology_NOR_ambi_flowers)

summary(m_flower_number_cool)


# emmeans ------------------------------------------------------------
emm_fl_num_cool <- emmeans(m_flower_number_cool, ~ site | treat_competition)
emm_fl_num_df_cool <- as.data.frame(summary(emm_fl_num_cool, infer = TRUE)) |> 
  rename(lower.CL = asymp.LCL, upper.CL = asymp.UCL)
emm_fl_num_df_cool


# contrasts ----------------------------------------------------------
# site
contr_site <- contrast(emm_fl_num_cool, method = "pairwise") |> 
  as.data.frame() |> 
  mutate(type = "site",
         sig = case_when(p.value < 0.001 ~ "***",
                         p.value < 0.01  ~ "**",
                         p.value < 0.05  ~ "*",
                         TRUE ~ "ns"))

# competition (with vs without within warming levels)
contr_comp <- contrast(emmeans(m_flower_number_cool, ~ treat_competition | site),
                       method = "pairwise") |> 
  as.data.frame() |> 
  mutate(type = "competition",
         sig = case_when(p.value < 0.001 ~ "***",
                         p.value < 0.01  ~ "**",
                         p.value < 0.05  ~ "*",
                         TRUE ~ "ns"))


# raw means for jittered points --------------------------------------
raw_means <- phenology_NOR_ambi_flowers |> 
  group_by(site, treat_competition, species, block_ID) |> 
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")


ggplot() +
  # raw points
  geom_jitter(data = raw_means,
              aes(x = site, y = mean_value, color = treat_competition),
              width = 0.1, alpha = 0.3, size = 1.5) +
  # marginal means
  geom_point(data = emm_fl_num_df_cool,
             aes(x = site, y = emmean, color = treat_competition),
             size = 5, position = position_dodge(width = 0.3)) +
  geom_errorbar(data = emm_fl_num_df_cool,
                aes(x = site, ymin = lower.CL, ymax = upper.CL, color = treat_competition ),
                width = 0.2, position = position_dodge(width = 0.3)) +
  geom_line(data = emm_fl_num_df_cool,
            aes(x = site, y = emmean, color = treat_competition , group = treat_competition),
            position = position_dodge(width = 0.3)) +
  labs(x = "Site",
       y = "Estimated flower number",
       color = "Biotic interactions",
       title = "Effect of warming and competition on flower number (Norway, high site)") +
  scale_color_manual(values = c("#528B8B", "#CD950C"))



# prepare annotations
y_max   <- max(emm_fl_num_df_cool$emmean, na.rm = TRUE)
spacing <- 0.4   # vertical spacing between brackets
offset  <- 0.15  # horizontal spacing around bracket ends

# site contrasts: ambi vs hi (for each competition treatment)
# ann_site <- contr_site %>%
#   mutate(x_center = ifelse(treat_competition == "with", 1, 2), # site ambi=1, hi=2
#          xmin = 1, xmax = 2,                # sites on x axis
#          y = y_max + (row_number()) * spacing,
#          label = sig)

ann_site <- contr_site %>%
  mutate(x_shift = ifelse(treat_competition == "with", -0.1, +0.1),  # shift left/right
         xmin = 1 + x_shift, 
         xmax = 2 + x_shift,
         y = y_max + (row_number()) * spacing,
         label = sig)

# competition contrasts: with vs without (for each site)
ann_comp <- contr_comp %>%
  mutate(xmin = 0.8, xmax = 1.2, 
         y = y_max + (nrow(ann_site) + row_number()) * spacing,
         label = sig,
         site_x = ifelse(site == "low", 1, 2)) %>%
  mutate(xmin = site_x - offset, xmax = site_x + offset)

# helper to draw brackets
bracket_geoms <- function(df) {
  list(
    geom_segment(data = df, aes(x = xmin, xend = xmax, y = y, yend = y)),
    geom_segment(data = df, aes(x = xmin, xend = xmin, y = y - 0.05, yend = y)),
    geom_segment(data = df, aes(x = xmax, xend = xmax, y = y - 0.05, yend = y)),
    geom_text(data = df, aes(x = (xmin + xmax)/2, y = y + 0.05, label = label),
              vjust = 0, size = 5)
  )
}

# --------------------------------------------------
# plot
p <- ggplot() +
  # raw points
  geom_jitter(data = raw_means,
              aes(x = site, y = mean_value, color = treat_competition),
              width = 0.1, alpha = 0.3, size = 1.5) +
  # marginal means
  geom_point(data = emm_fl_num_df_cool,
             aes(x = site, y = emmean, color = treat_competition),
             size = 7, position = position_dodge(width = 0.3)) +
  geom_errorbar(data = emm_fl_num_df_cool,
                aes(x = site, ymin = lower.CL, ymax = upper.CL, color = treat_competition), linewidth = 1,
                width = 0.3, position = position_dodge(width = 0.3)) +
  geom_line(data = emm_fl_num_df_cool,
            aes(x = site, y = emmean, color = treat_competition, group = treat_competition),
            position = position_dodge(width = 0.3)) +
  labs(x = "Site",
       y = "Predicted mean number of flowers",
       color = "Biotic interactions",
       title = "Effect of site and competition on flower number (Norway)") +
  scale_color_manual(values = c("#528B8B", "#CD950C")) +
  bracket_geoms(ann_site) +
  bracket_geoms(ann_comp)+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

p

ggsave(filename = "Output/Biomass/Cooling_competition_flower_number_NOR.png", plot = p, width = 10, height = 8, units = "in")






















