

# ## RangeX data cleaning for traits 2023 -------------------------------------------------

## Data used: RangeX_raw_traits_high_2023.csv,
##            RangeX_raw_traits_low_2023.csv, 
##            RangeX_Metadata.csv,
##            RangeX_YearlyDemographics.csv
## Date:      13.10.23
## Author:    Nadine Arzt
## Purpose:   Transform the raw data in the format we agreed on


# comments ----------------------------------------------------------------
# one plant has no block_ID_original (f sucpra e4) --> block 6

# load packages -----------------------------------------------------------
library(tidyverse) ## if you use this you dont need dplyr and stringr, tidyr

# load data 2023 traits low ---------------------------------------------------------------
traits_low_23 <- read.csv2("Data/Raw/RangeX_raw_traits_low_2023.csv")
head(traits_low_23)

# check structure of data set ---------------------------------------------
str(traits_low_23)
length(traits_low_23) ## 71 columns

## get column names
dput(colnames(traits_low_23))

# delete superfluous columns ----------------------------------------------
traits_low_23 <- traits_low_23 %>%
  select(
    where(
      ~!all(is.na(.x))
    )
  )

length(traits_low_23) ## 63 columns

# delete superfluous rows with NAs ----------------------------------------
## remove rows at the end, which have only NAs or nothing in it
traits_low_23 <- traits_low_23 %>% drop_na(block)
length(traits_low_23$block) ## 600 rows


traits_low_23 <- traits_low_23 %>%
  filter(rowSums(is.na(.)) < ncol(traits_low_23))


# change column names -----------------------------------
## read yearly demographics table for right column names
yearly_demographic <- read.csv2("Data/RangeX_YearlyDemographics.csv", sep = ",")
## get column names
dput(colnames(yearly_demographic))

#
## get column names of traits 23
dput(colnames(traits_low_23))

## add column with region = NOR for Norway
traits_low_23 <- traits_low_23 %>%
  add_column(region = "NOR")
traits_low_23

## add column with site = lo for low
traits_low_23 <- traits_low_23 %>%
  add_column(site = "lo")
traits_low_23


## rename column names
traits_low_23 <- traits_low_23 %>%
  rename("block_ID_original" = "block",
         "plot_ID_original" = "treat",
         "position_ID_original" = "coord",
         "height_vegetative_str" = "height_veg_stretch_cm",
         "height_reproductive_str" = "height_rep_strech_cm",
         "height_vegetative" = "height_veg_cm",
         "height_reproductive" = "height_rep_cm",
         "leaf_length1" = "leaf_length_mm",
         "leaf_width" = "leaf_width_mm",
         "petiole_length" = "petiole_length_mm")

# sampled quarter ---------------------------------------------------------
## where sampled quarter, multiply by 4

traits_low_23$number_leaves <- ifelse(traits_low_23$sampled_quarter == "yes", 
                                      traits_low_23$no_leaves * 4, traits_low_23$no_leaves)

traits_low_23$number_leaves
# View(traits_low_23)
# View(traits_low_23[, 1:66])  # Display the first 66 columns --> does not work

# utils::View(traits_low_23)

traits_low_23$no_flowers_col40

# calculate number of flowers ----------------------------------
str(traits_low_23)

# Convert non-numeric columns to numeric (e.g., if they contain character strings)
traits_low_23$no_flowers_col1 <- as.numeric(traits_low_23$no_flowers_col1)
traits_low_23$no_flowers_col2 <- as.numeric(traits_low_23$no_flowers_col2)
traits_low_23$no_flowers_col3 <- as.numeric(traits_low_23$no_flowers_col3)
traits_low_23$no_flowers_col4 <- as.numeric(traits_low_23$no_flowers_col4)
traits_low_23$no_flowers_col5 <- as.numeric(traits_low_23$no_flowers_col5)
traits_low_23$no_flowers_col6 <- as.numeric(traits_low_23$no_flowers_col6)
traits_low_23$no_flowers_col7 <- as.numeric(traits_low_23$no_flowers_col7)
traits_low_23$no_flowers_col8 <- as.numeric(traits_low_23$no_flowers_col8)
traits_low_23$no_flowers_col9 <- as.numeric(traits_low_23$no_flowers_col9)
traits_low_23$no_flowers_col10 <- as.numeric(traits_low_23$no_flowers_col10)
traits_low_23$no_flowers_col11 <- as.numeric(traits_low_23$no_flowers_col11)
traits_low_23$no_flowers_col12 <- as.numeric(traits_low_23$no_flowers_col12)
traits_low_23$no_flowers_col13 <- as.numeric(traits_low_23$no_flowers_col13)
traits_low_23$no_flowers_col14 <- as.numeric(traits_low_23$no_flowers_col14)
traits_low_23$no_flowers_col15 <- as.numeric(traits_low_23$no_flowers_col15)
traits_low_23$no_flowers_col16 <- as.numeric(traits_low_23$no_flowers_col16)
traits_low_23$no_flowers_col17 <- as.numeric(traits_low_23$no_flowers_col17)
traits_low_23$no_flowers_col18 <- as.numeric(traits_low_23$no_flowers_col18)
traits_low_23$no_flowers_col19 <- as.numeric(traits_low_23$no_flowers_col19)
traits_low_23$no_flowers_col20 <- as.numeric(traits_low_23$no_flowers_col20)
traits_low_23$no_flowers_col21 <- as.numeric(traits_low_23$no_flowers_col21)
traits_low_23$no_flowers_col22 <- as.numeric(traits_low_23$no_flowers_col22)
traits_low_23$no_flowers_col23 <- as.numeric(traits_low_23$no_flowers_col23)
traits_low_23$no_flowers_col24 <- as.numeric(traits_low_23$no_flowers_col24)
traits_low_23$no_flowers_col25 <- as.numeric(traits_low_23$no_flowers_col25)
traits_low_23$no_flowers_col26 <- as.numeric(traits_low_23$no_flowers_col26)
traits_low_23$no_flowers_col27 <- as.numeric(traits_low_23$no_flowers_col27)
traits_low_23$no_flowers_col28 <- as.numeric(traits_low_23$no_flowers_col28)
traits_low_23$no_flowers_col29 <- as.numeric(traits_low_23$no_flowers_col29)
traits_low_23$no_flowers_col30 <- as.numeric(traits_low_23$no_flowers_col30)
traits_low_23$no_flowers_col31 <- as.numeric(traits_low_23$no_flowers_col31)
traits_low_23$no_flowers_col32 <- as.numeric(traits_low_23$no_flowers_col32)
traits_low_23$no_flowers_col33 <- as.numeric(traits_low_23$no_flowers_col33)
traits_low_23$no_flowers_col34 <- as.numeric(traits_low_23$no_flowers_col34)
traits_low_23$no_flowers_col35 <- as.numeric(traits_low_23$no_flowers_col35)
traits_low_23$no_flowers_col36 <- as.numeric(traits_low_23$no_flowers_col36)
traits_low_23$no_flowers_col37 <- as.numeric(traits_low_23$no_flowers_col37)
traits_low_23$no_flowers_col38 <- as.numeric(traits_low_23$no_flowers_col38)
traits_low_23$no_flowers_col39 <- as.numeric(traits_low_23$no_flowers_col39)
traits_low_23$no_flowers_col40 <- as.numeric(traits_low_23$no_flowers_col40)

## add the individual numbers of flowers per stem
traits_low_23$number_flowers <- rowSums(traits_low_23[, c("no_flowers_col1", "no_flowers_col2", "no_flowers_col3", "no_flowers_col4", "no_flowers_col5", 
                                                          "no_flowers_col6", "no_flowers_col7", "no_flowers_col8", "no_flowers_col9", "no_flowers_col10",
                                                          "no_flowers_col11", "no_flowers_col12", "no_flowers_col13", "no_flowers_col14", "no_flowers_col15",
                                                          "no_flowers_col16", "no_flowers_col17", "no_flowers_col18", "no_flowers_col19", 
                                                          "no_flowers_col20", "no_flowers_col21", "no_flowers_col22", "no_flowers_col23", 
                                                          "no_flowers_col24", "no_flowers_col25", "no_flowers_col26", "no_flowers_col27",
                                                          "no_flowers_col28", "no_flowers_col29", "no_flowers_col30", "no_flowers_col31", 
                                                          "no_flowers_col32", "no_flowers_col33", "no_flowers_col34", "no_flowers_col35", 
                                                          "no_flowers_col36", "no_flowers_col37", "no_flowers_col38", "no_flowers_col39", "no_flowers_col40")], na.rm = TRUE)

# utils::View(traits_low_23)
traits_low_23$number_flowers

dput(colnames(traits_low_23))


# delete columns with flower number ---------------------------------------
# define columns that are to be removed
cols_to_remove <- c(
  "no_flowers_col1", "no_flowers_col2", "no_flowers_col3", "no_flowers_col4", "no_flowers_col5",
  "no_flowers_col6", "no_flowers_col7", "no_flowers_col8", "no_flowers_col9", "no_flowers_col10",
  "no_flowers_col11", "no_flowers_col12", "no_flowers_col13", "no_flowers_col14", "no_flowers_col15",
  "no_flowers_col16", "no_flowers_col17", "no_flowers_col18", "no_flowers_col19",
  "no_flowers_col20", "no_flowers_col21", "no_flowers_col22", "no_flowers_col23",
  "no_flowers_col24", "no_flowers_col25", "no_flowers_col26", "no_flowers_col27",
  "no_flowers_col28", "no_flowers_col29", "no_flowers_col30", "no_flowers_col31",
  "no_flowers_col32", "no_flowers_col33", "no_flowers_col34", "no_flowers_col35",
  "no_flowers_col36", "no_flowers_col37", "no_flowers_col38", "no_flowers_col39", "no_flowers_col40"
)

# Remove the specified columns from the data frame
traits_low_23 <- traits_low_23[, !names(traits_low_23) %in% cols_to_remove]


# remove columns with nathan height for now -------------------------------
# traits_low_23 <- subset(traits_low_23, select = -nathan_old_new)

## remember that you have to deal with this later, but for now you can ignore the nathan height


# number of tillers -------------------------------------------------------
## we have this for cyncri and luzmul
## there is a column for this in yearly_demographics

# remove column tillers_yes_no
traits_low_23 <- subset(traits_low_23, select = -tillers_yes_no)









# load data 2023 traits high ---------------------------------------------------------------
traits_high_23 <- read.csv2("Data/Raw/RangeX_raw_traits_high_2023.csv")
head(traits_high_23)


# check structure of data set ---------------------------------------------
str(traits_high_23)
length(traits_high_23) ## 70 columns

## get column names
dput(colnames(traits_high_23))

# delete superfluous columns ----------------------------------------------
traits_high_23 <- traits_high_23 %>%
  select(
    where(
      ~!all(is.na(.x))
    )
  )

length(traits_high_23) ## 47 columns


# fix missing block -------------------------------------------------------
# block 6 for supra in e4
traits_high_23 <- traits_high_23 |> 
  mutate(block = case_when(treat == "f" & species == "sucpra" & 
                              coord == "e4" ~ 6,
                            TRUE ~ block))


# change column names -----------------------------------
## get column names
dput(colnames(yearly_demographic))

## get column names of traits 23
dput(colnames(traits_high_23))

## add column with region = NOR for Norway
traits_high_23 <- traits_high_23 %>%
  add_column(region = "NOR")
traits_high_23

## add column with site = hi for high
traits_high_23 <- traits_high_23 %>%
  add_column(site = "hi")
traits_high_23


## rename column names
traits_high_23 <- traits_high_23 %>%
  rename("block_ID_original" = "block",
         "plot_ID_original" = "treat",
         "position_ID_original" = "coord",
         "height_vegetative_str" = "height_veg_stretch_cm",
         "height_reproductive_str" = "height_rep_strech_cm",
         "height_vegetative" = "height_veg_cm",
         "height_reproductive" = "height_rep_cm",
         "leaf_length1" = "leaf_length_mm",
         "leaf_width" = "leaf_width_mm",
         "petiole_length" = "petiole_length_mm")

dput(colnames(traits_high_23))


# sampled quarter ---------------------------------------------------------
## where sampled quarter, multiply by 4

traits_high_23$number_leaves <- ifelse(traits_high_23$sampled_quarter == "yes", 
                                       traits_high_23$no_leaves * 4, traits_high_23$no_leaves)


# calculate mean value number of flowers ----------------------------------
str(traits_high_23)

# Convert non-numeric columns to numeric (e.g., if they contain character strings)
traits_high_23$no_flowers_col1 <- as.numeric(traits_high_23$no_flowers_col1)
traits_high_23$no_flowers_col2 <- as.numeric(traits_high_23$no_flowers_col2)
traits_high_23$no_flowers_col3 <- as.numeric(traits_high_23$no_flowers_col3)
traits_high_23$no_flowers_col4 <- as.numeric(traits_high_23$no_flowers_col4)
traits_high_23$no_flowers_col5 <- as.numeric(traits_high_23$no_flowers_col5)
traits_high_23$no_flowers_col6 <- as.numeric(traits_high_23$no_flowers_col6)
traits_high_23$no_flowers_col7 <- as.numeric(traits_high_23$no_flowers_col7)
traits_high_23$no_flowers_col8 <- as.numeric(traits_high_23$no_flowers_col8)
traits_high_23$no_flowers_col9 <- as.numeric(traits_high_23$no_flowers_col9)
traits_high_23$no_flowers_col10 <- as.numeric(traits_high_23$no_flowers_col10)
traits_high_23$no_flowers_col11 <- as.numeric(traits_high_23$no_flowers_col11)
traits_high_23$no_flowers_col12 <- as.numeric(traits_high_23$no_flowers_col12)
traits_high_23$no_flowers_col13 <- as.numeric(traits_high_23$no_flowers_col13)
traits_high_23$no_flowers_col14 <- as.numeric(traits_high_23$no_flowers_col14)
traits_high_23$no_flowers_col15 <- as.numeric(traits_high_23$no_flowers_col15)
traits_high_23$no_flowers_col16 <- as.numeric(traits_high_23$no_flowers_col16)
traits_high_23$no_flowers_col17 <- as.numeric(traits_high_23$no_flowers_col17)
traits_high_23$no_flowers_col18 <- as.numeric(traits_high_23$no_flowers_col18)
traits_high_23$no_flowers_col19 <- as.numeric(traits_high_23$no_flowers_col19)
traits_high_23$no_flowers_col20 <- as.numeric(traits_high_23$no_flowers_col20)
traits_high_23$no_flowers_col21 <- as.numeric(traits_high_23$no_flowers_col21)
traits_high_23$no_flowers_col22 <- as.numeric(traits_high_23$no_flowers_col22)
traits_high_23$no_flowers_col23 <- as.numeric(traits_high_23$no_flowers_col23)
traits_high_23$no_flowers_col24 <- as.numeric(traits_high_23$no_flowers_col24)
traits_high_23$no_flowers_col25 <- as.numeric(traits_high_23$no_flowers_col25)
traits_high_23$no_flowers_col26 <- as.numeric(traits_high_23$no_flowers_col26)



traits_high_23$number_flowers <- rowSums(traits_high_23[, c("no_flowers_col1", "no_flowers_col2", "no_flowers_col3", "no_flowers_col4", "no_flowers_col5", 
                                                            "no_flowers_col6", "no_flowers_col7", "no_flowers_col8", "no_flowers_col9", "no_flowers_col10",
                                                            "no_flowers_col11", "no_flowers_col12", "no_flowers_col13", "no_flowers_col14", "no_flowers_col15",
                                                            "no_flowers_col16", "no_flowers_col17", "no_flowers_col18", "no_flowers_col19", 
                                                            "no_flowers_col20", "no_flowers_col21", "no_flowers_col22", "no_flowers_col23", 
                                                            "no_flowers_col24", "no_flowers_col25", "no_flowers_col26")], na.rm = TRUE)

# utils::View(traits_high_23)
traits_high_23$number_flowers

# delete columns with flower number ---------------------------------------
# define columns that are to be removed
cols_to_remove_high <- c(
  "no_flowers_col1", "no_flowers_col2", "no_flowers_col3", "no_flowers_col4", "no_flowers_col5",
  "no_flowers_col6", "no_flowers_col7", "no_flowers_col8", "no_flowers_col9", "no_flowers_col10",
  "no_flowers_col11", "no_flowers_col12", "no_flowers_col13", "no_flowers_col14", "no_flowers_col15",
  "no_flowers_col16", "no_flowers_col17", "no_flowers_col18", "no_flowers_col19",
  "no_flowers_col20", "no_flowers_col21", "no_flowers_col22", "no_flowers_col23",
  "no_flowers_col24", "no_flowers_col25", "no_flowers_col26"
)

# Remove the specified columns from the data frame
traits_high_23 <- traits_high_23[, !names(traits_high_23) %in% cols_to_remove_high]

dput(colnames(traits_high_23))
dput(colnames(traits_low_23))


# Find common columns
common_columns <- intersect(colnames(traits_high_23), colnames(traits_low_23))

# Find columns unique to traits_high_23
unique_to_high <- setdiff(colnames(traits_high_23), colnames(traits_low_23))

# Find columns unique to traits_low_23
unique_to_low <- setdiff(colnames(traits_low_23), colnames(traits_high_23))

# Display the results
cat("Common Columns: ", common_columns, "\n")
cat("Columns unique to traits_high_23: ", unique_to_high, "\n")
cat("Columns unique to traits_low_23: ", unique_to_low, "\n")

## we counted tillers for cyncri and luzmul
## why do we have the tillers only in the low site?
## we did it at the high site as well but don't have a column for it

# nathan height -------------------------------
# traits_high_23 <- subset(traits_high_23, select = -height_nathan_cm)

## but you need a column with nathan_old_new
traits_high_23$nathan_old_new <- NA


# merge data traits high with traits low ---------------------------------------
## get column names
dput(colnames(traits_high_23))
dput(colnames(traits_low_23))

## combine high and low site
traits_23 <- rbind(traits_high_23, traits_low_23)
head(traits_23)


# delete no_leaves and sampled quarter ------------------------------------
traits_23 <- subset(traits_23, select = -c(no_leaves, sampled_quarter))


# number of tillers -------------------------------------------------------
## we have this for cyncri and luzmul
## there is a column for this in yearly_demographics

# Define the species which should have values in number_tillers instead of number_leaves
species_to_move <- c("cyncri", "luzmul")

# Create a new column "number_tillers" with NA for all rows
traits_23$number_tillers <- NA

# Move the values from "number_leaves" to "number_tillers" for the specified species
for (species in species_to_move) {
  traits_23$number_tillers[traits_23$species == species] <- traits_23$number_leaves[traits_23$species == species]
}

# Set NA values in "number_leaves" for the specified species
traits_23$number_leaves[traits_23$species %in% species_to_move] <- NA


# traits and yearly_demographics comparison off columns -------------------
# Find common columns
(common_columns <- intersect(colnames(traits_23), colnames(yearly_demographic)))

# Find columns unique to traits_high_23
(unique_to_traits <- setdiff(colnames(traits_23), colnames(yearly_demographic)))

# Find columns unique to traits_low_23
(unique_to_yearly_demo <- setdiff(colnames(yearly_demographic), colnames(traits_23)))



# herbivory ---------------------------------------------------------------
## Is the plant damaged by herbivory → yes (1), no (0)
str(traits_23)
traits_23$herbivory_flower <- as.numeric(traits_23$herbivory_flower)
traits_23$herbivory_leaf <- as.numeric(traits_23$herbivory_leaf)

# Create a new column "herbivory" based on "herbivory_flower" and "herbivory_leaves"
traits_23$herbivory <- ifelse(!is.na(traits_23$herbivory_flower) | !is.na(traits_23$herbivory_leaf), 1, 0)

## 1 and 0 for comparing the data over all countries, but for NOR we have it in more detail



# dealing with Nathans_height ---------------------------------------------
## do it later, not for Ireland
# mean_inflorescence_size -------------------------------------------------

## Nathan style: total height minus flower stretched (careful: it was changed on 11.07.23 to stretched! 
## All plots of 10.07.23 are not stretched  calculate rep stretched minus Nathan stretched to get 
## flower length for the first plots (1A,B; 3A,B; 5A,B; 6B; 2B)) 


# Calculate the flower length and store it in a new column
traits_23 <- traits_23 %>%
  mutate(flower_length = height_reproductive_str - height_nathan_cm)



# delete nathan style related columns -------------------------------------
dput(colnames(traits_23))

traits_23 <- subset(traits_23, select = -c(height_nathan_cm, nathan_old_new, flower_length, comment))


# sort after site, block, plot, position ----------------------------------
traits_23 <- traits_23 %>%
  group_by(site, block_ID_original , plot_ID_original) %>%
  arrange(block_ID_original,plot_ID_original, position_ID_original, .by_group = TRUE)



# add column year ----------------------------------------------------------
traits_23 <- traits_23 %>%
  add_column(year = 2023)
traits_23

class(traits_23$year)




# load metadata file for all countries ------------------------------------------------------
metadata <- read.csv(here("Data/RangeX_metadata_focal_NOR.csv"), row.names = 1)
head(metadata)
dput(colnames(metadata))




# merge metadata with trait data ------------------------------------------

dput(colnames(metadata))
dput(colnames(traits_23))

demo_traits_2023 <- left_join(traits_23, metadata,
                         by = c("region", "site", "block_ID_original",
                                "plot_ID_original",
                                "position_ID_original", "species"))



dput(colnames(demo_traits_2023))
dput(colnames(yearly_demographic))

# adapt demo_traits_2023 in the format of yearly demographics ------------------

## add all columns that are in yearly demo but not in traits 2023

demo_traits_2023 <- demo_traits_2023 %>%
  dplyr::mutate(
    collector = NA,
    vegetative_width = NA,
    vegetative_length = NA,
    stem_diameter = NA,
    leaf_length2 = NA,
    leaf_length3 = NA,
    petiole_length1 = NA,
    number_branches = NA,
    number_leafclusters = NA,
    mean_inflorescence_size = NA
  )

dput(colnames(demo_traits_2023))

## delete "region", "site", "block_ID_original", "plot_ID_original", 
## "position_ID_original","treat_warming", "treat_competition", 
## "added_focals", "block_ID", "position_ID", "unique_plot_ID"

## delete unnecessary columns
demo_traits_2023 <- demo_traits_2023 %>%
  dplyr::select(-region, -site, -block_ID_original, -plot_ID_original, 
                -position_ID_original, -treat_warming, -treat_competition, 
                -added_focals, -block_ID, -position_ID, -unique_plot_ID) %>% 
  dplyr::ungroup()

## Adding missing grouping variables: `site`, `block_ID_original`, `plot_ID_original`
## WHY cant I delete them

dput(colnames(demo_traits_2023))
length(demo_traits_2023) # 33
length(yearly_demographic) # 23


## make correct order as in yearly_demographics
col_order_traits_23 <- c("site", "block_ID_original", "plot_ID_original","unique_plant_ID", 
                         "species", "year", "collector", "observer", "height_vegetative_str", 
                         "height_reproductive_str", "height_vegetative", "height_reproductive", 
                         "vegetative_width", "vegetative_length", "stem_diameter",
                         "leaf_length1", "leaf_length2", "leaf_length3", "leaf_width", "petiole_length", 
                         "petiole_length1", 
                         "number_leaves", "number_tillers", "number_branches", "number_leafclusters", 
                         "number_flowers", "mean_inflorescence_size", "herbivory")

rangex_traits_23 <- demo_traits_2023[, col_order_traits_23]
rangex_traits_23


## collector = observer
## put values from leaf_length in column leaf_length1
## same with petiole_length
rangex_traits_23 <- rangex_traits_23 %>%
  dplyr::mutate(collector = dplyr::coalesce(collector, observer)) %>%
  dplyr::mutate(petiole_length = dplyr::coalesce(petiole_length, petiole_length1)) %>%
  dplyr::select(-observer, - petiole_length1)


## delete site, block_ID_original, plot_ID_original
rangex_traits_23 <- rangex_traits_23 %>%
  dplyr::select(-site, -block_ID_original, -plot_ID_original)


dput(colnames(rangex_traits_23))
dput(colnames(yearly_demographic))


# save csv file -----------------------------------------------------------
# write.csv(rangex_traits_23, "Data/Data_demographic_traits/RangeX_clean_traits_2023.csv", row.names = FALSE)

## read cleaned data
# data_23 <- read.csv("Data/Data_demographic_traits/RangeX_clean_traits_2023.csv")













