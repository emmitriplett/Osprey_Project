#' ---
#' title: Map of birds encountered dead
#' author: Emmi Triplett
#' date: 13 Apr 2023
#' output: html_document
#' ---
#' 

library(ggplot2)
library(mapview)
library(sf)

# read data 
dat <- read.csv('./data/BBL2022_Req_828_enc_3640_20230209_120001 (2).csv')

# loop to subset encounters found dead
uni_band_code <- unique(dat$band)
band_ct <- table(dat$band)
dat_sub <- data.frame()
for (i in uni_band_code) {                        # loop through each band
  tmp <- subset(dat, band == i)                   # subset to just that band
  if (sum(tmp$event_type %in% c('B', 'E')) >= 2) { # make sure B & E present
    if (all(tmp$other_bands %in% "")) {           # make sure no other band codes
      encounter_event <- subset(tmp, event_type == 'E') # pull encounter row
      # remove how obtained codes and age that should be excluded
      encounter_gd <- !(encounter_event$how_obtained_code %in% c(29, 33, 52, 53, 59, 66, 28, 50, 56, 97, 98))
      encounter_event <- encounter_event[encounter_gd, ]
      if (nrow(encounter_event) > 0) {       # make sure there is still some encounters 
        tmp <- tail(encounter_event, 1) # create output
        dat_sub <- rbind(dat_sub, tmp)              # save output to big object
      }  
    }
  }
}

# remove missing data
dat_sub <- dat_sub[complete.cases(dat_sub[c("lat_dd", "lon_dd")]), ]

codes <- c(1, 2, 3, 4, 10, 12, 13, 15, 17, 21, 23, 24, 26, 27, 30, 39, 42, 44, 
           54, 57, 60, 61, 62, 63, 64, 7, 9, 11, 14, 20, 25, 31, 34, 45, 91)
dat_sub_subset <- dat_sub[dat_sub$how_obtained_code %in% codes, ]

# group codes
road_casualty <- c(45, 14, 60)
taken_by_animal <- c(34, 31, 7, 11, 9, 64, 12)
Poisoning <- c(25, 62)
Disease <- c(20, 61)
Striking <- c(63, 54, 42, 39, 13, 27)
Entanglement <- c(57, 26)
control_operations <- 44
Exhaustion <- 36
nest_mortality <- c(30, 24)
oil_or_tar <- 23
found_in_building <- 21
Drowned <- 17
weather_conditions <- 15
banding_mortality <- 10
traps_or_snares <- 4
Injury <- 3
Starvation <- 2
Shot <- 1
illegally_taken <- 91

# create a new column, name it cause_of_death, put it in the data frame
library(dplyr)
dat_sub_subset <- dat_sub_subset %>%
  mutate(cause_of_death = case_when(
    how_obtained_code %in% road_casualty ~ "Road Casualty",
    how_obtained_code %in% taken_by_animal ~ "Taken by Animal",
    how_obtained_code %in% Poisoning ~ "Poisoning",
    how_obtained_code %in% Disease ~ "Disease",
    how_obtained_code %in% Striking ~ "Striking",
    how_obtained_code %in% Entanglement ~ "Entanglement",
    how_obtained_code %in% control_operations ~ "Control Operations",
    how_obtained_code %in% Exhaustion ~ "Exhaustion",
    how_obtained_code %in% nest_mortality ~ "Nest Mortality",
    how_obtained_code %in% oil_or_tar ~ "Oil or Tar",
    how_obtained_code %in% found_in_building ~ "Found in Building",
    how_obtained_code %in% Drowned ~ "Drowned",
    how_obtained_code %in% weather_conditions ~ "Weather Conditions",
    how_obtained_code %in% banding_mortality ~ "Banding Mortality",
    how_obtained_code %in% traps_or_snares ~ "Traps or Snares",
    how_obtained_code %in% Injury ~ "Injury",
    how_obtained_code %in% Starvation ~ "Starvation",
    how_obtained_code %in% Shot ~ "Shot",
    how_obtained_code %in% illegally_taken ~ "Illegally Taken",
    TRUE ~ "Unknown"
  ))

# remove missing data
dat_sub_subset <- dat_sub_subset[complete.cases(dat_sub_subset[c("lat_dd", "lon_dd")]), ]

#subset out unneeded columns
map_sub <- dat_sub_subset[ , c("species_scientific_name", "event_year", "how_obtained_code", "lat_dd", "lon_dd")]     

# make a map
mapview(map_sub, xcol = "lon_dd", ycol = "lat_dd", crs = 4269, grid = FALSE,
        col.regions = "lightskyblue")






