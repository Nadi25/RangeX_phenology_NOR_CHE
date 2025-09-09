

# Effect of warming and competition on flower number -----------------------

# RangeX phenology effect of warming NOR and CHE ------------

## Data used: RangeX_clean_Phenology_2022_CHE.csv
##            RangeX_clean_phenology_2023_NOR.csv
##            RangeX_clean_MetadataFocal_CHE.csv
##            RangeX_metadata_focal_NOR.csv
## Date:      02.09.25
## Author:    Nadine Arzt
## Purpose:   Effect of warming and competition on flower number NOR


# load library -----------------------------------------------------------
library(dplyr)
library(lmerTest)
library(ggeffects)
library(broom.mixed)
library(emmeans)


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



# NOR only----------------------------------------
phenology_NOR <- phenology |> 
  filter(region == "Norway")


# filter only high site --------------------------------------------------------
phenology_NOR_hi <- phenology_NOR |> 
  filter(site == "hi")

# and get julian days --------------------------------------
phenology_NOR_hi <- phenology_NOR_hi |> 
  mutate(jday = yday(date_measurement),   # Julian day (1–365)
         jday_scaled = scale(jday))        # optional scaling if you need for models 



# only flowers ----------------------------------------------------------
phenology_NOR_hi_flowers <- phenology_NOR_hi |> 
  filter(phenology_stage == "No_FloOpen")


# model --------------------------------------------------------------
m_flower_number <- lmerTest::lmer(value ~ treat_warming * treat_competition +
                          (1|species) + (1|block_ID),
                        data = phenology_NOR_hi_flowers)

summary(m_flower_number)

# emmeans ------------------------------------------------------------
emm_fl_num <- emmeans(m_flower_number, ~ treat_warming | treat_competition)
emm_fl_num_df <- as.data.frame(summary(emm_fl_num, infer = TRUE)) |> 
  rename(lower.CL = asymp.LCL, upper.CL = asymp.UCL)

# contrasts ----------------------------------------------------------
# warming (ambi vs warm within competition levels)
contr_warm <- contrast(emm_fl_num, method = "pairwise") |> 
  as.data.frame() |> 
  mutate(type = "warming",
         sig = case_when(p.value < 0.001 ~ "***",
                         p.value < 0.01  ~ "**",
                         p.value < 0.05  ~ "*",
                         TRUE ~ "ns"))

# competition (with vs without within warming levels)
contr_comp <- contrast(emmeans(m_flower_number, ~ treat_competition | treat_warming),
                       method = "pairwise") |> 
  as.data.frame() |> 
  mutate(type = "competition",
         sig = case_when(p.value < 0.001 ~ "***",
                         p.value < 0.01  ~ "**",
                         p.value < 0.05  ~ "*",
                         TRUE ~ "ns"))

# raw means for jittered points --------------------------------------
raw_means <- phenology_NOR_hi_flowers |> 
  group_by(treat_warming, treat_competition, species, block_ID) |> 
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

# annotation prep ----------------------------------------------------
y_max   <- max(emm_fl_num_df$emmean, na.rm = TRUE)
spacing <- 0.35
offset  <- 0.15

ann_warm <- contr_warm |> 
  mutate(x_center = ifelse(treat_competition == "with", 1, 2),
         xmin = x_center - offset, xmax = x_center + offset,
         y = y_max + (row_number()) * spacing,
         label = sig)

ann_comp <- contr_comp |> 
  mutate(xmin = 1, xmax = 2,
         y = y_max + (nrow(ann_warm) + row_number()) * spacing,
         label = sig)

# helper for brackets ------------------------------------------------
bracket_geoms <- function(df) {
  list(
    geom_segment(data = df, aes(x = xmin, xend = xmax, y = y, yend = y)),
    geom_segment(data = df, aes(x = xmin, xend = xmin, y = y - 0.05, yend = y)),
    geom_segment(data = df, aes(x = xmax, xend = xmax, y = y - 0.05, yend = y)),
    geom_text(data = df, aes(x = (xmin + xmax)/2, y = y + 0.05, label = label),
              vjust = 0, size = 5)
  )
}

# plot ---------------------------------------------------------------
p <- ggplot() +
  geom_jitter(data = raw_means,
              aes(x = treat_competition, y = mean_value, color = treat_warming),
              width = 0.12, alpha = 0.35, size = 1.3) +
  geom_point(data = emm_fl_num_df,
             aes(x = treat_competition, y = emmean, color = treat_warming),
             size = 7, position = position_dodge(width = 0.3)) +
  geom_errorbar(data = emm_fl_num_df,
                aes(x = treat_competition, ymin = lower.CL, ymax = upper.CL,
                    color = treat_warming), linewidth = 1,
                width = 0.2, position = position_dodge(width = 0.3)) +
  geom_line(data = emm_fl_num_df,
            aes(x = treat_competition, y = emmean, color = treat_warming,
                group = treat_warming),
            position = position_dodge(width = 0.3)) +
  labs(x = "Biotic interactions", y = "Predicted mean number of flowers", color = "Temperature") +
  scale_color_manual(values = c("darkblue", "pink3"))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  bracket_geoms(ann_warm) +
  bracket_geoms(ann_comp)

p


ggsave(filename = "Output/Warming_competition_flower_number_NOR.png", plot = p, width = 10, height = 8, units = "in")







