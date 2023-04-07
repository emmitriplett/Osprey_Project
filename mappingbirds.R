

# Lets try to make a map

library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")


# basic world map
theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
ggplot(data = world) +
  geom_sf()

# read data 
dat <- read.csv('./data/BBL2022_Req_828_enc_3640_20230209_120001 (2).csv')

uni_band_code <- unique(dat$band)
band_ct <- table(dat$band)

# run loop to subset only banding events
dat_b <- data.frame()
for (i in uni_band_code) {                        # loop through each band
  tmp <- subset(dat, band == i)                   # subset to just that band
  if (sum(tmp$event_type %in% c('B', 'E')) >= 2) { # make sure B & E present
    if (all(tmp$other_bands %in% "")) {           # make sure no other band codes
      banding_event <- subset(tmp, event_type == 'B') # pull banding row
        tmp <- rbind(banding_event) # create output
        dat_b <- rbind(dat_b, tmp)              # save output to big object
      }  
    }
  }

# run loop to subset only encounter events
  
dat_e <- data.frame()
for (i in uni_band_code) {                        # loop through each band
  tmp <- subset(dat, band == i)                   # subset to just that band
  if (sum(tmp$event_type %in% c('B', 'E')) >= 2) { # make sure B & E present
    if (all(tmp$other_bands %in% "")) {           # make sure no other band codes
      encounter_event <- subset(tmp, event_type == 'E') # pull encounter row
      if (nrow(encounter_event) > 0) {       # make sure there is still some encounters 
        tmp <- rbind(tail(encounter_event, 1)) # create output
        dat_e <- rbind(dat_e, tmp)              # save output to big object
      }  
    }
  }
}

# remove missing data
dat_b <- dat_b[complete.cases(dat_b[c("lat_dd", "lon_dd")]), ]
dat_e <- dat_e[complete.cases(dat_e[c("lat_dd", "lon_dd")]), ]

# plot cords
mapview(dat_b, xcol = "lon_dd", ycol = "lat_dd", crs = 4269, grid = FALSE, col.regions = "seagreen")  
mapview(dat_e, xcol = "lon_dd", ycol = "lat_dd", crs = 4269, grid = FALSE, col.regions = "lightskyblue") 

# All the points.. not dif colors

mapview(dat_sub, xcol = "lon_dd", ycol = "lat_dd", crs = 4269, grid = FALSE)

