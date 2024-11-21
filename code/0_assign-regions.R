###############################################################################
# Assign NuSEDS populations to PSE regions based on locations
###############################################################################


library(sf)
library(dplyr)
source("code/functions.R")

wd_data_input <- paste0(getwd(),"/data_input")
wd_data_output <- paste0(getwd(),"/data_output")

#------------------------------------------------------------------------------
# Read in data and create spatial variables
#------------------------------------------------------------------------------

# Load region shapefile
regions_spat <- st_read(paste0(wd_data_input,"/se_boundary_regions.shp")) %>%
  st_transform(crs = 4269)

sf_use_s2(FALSE)

#' Import the cleaned NuSEDS data matched with PSF cuid and streamid
#' This is the clean version of the New Salmon Escapement Database (NuSEDS). It 
#' must be downloaded at https://zenodo.org/records/14194639 and placed in the
#' /data_input folder.
nuseds <- read.csv(paste0(wd_data_input,"/nuseds_cuid_streamid_20240419.csv"), header = T)

# Create spatial variable of survey sites
nuseds_loc <- nuseds %>% 
  filter(!is.na(cuid)) %>%
  distinct(SPECIES, POP_ID, cuid, cu_name_pse, streamid, sys_nm_final, latitude_final, longitude_final) %>%
  st_as_sf(coords = c("longitude_final", "latitude_final"), crs = 4269)

dim(nuseds_loc) # 6766 "populations" 

#------------------------------------------------------------------------------
# Assign region to each population based on location
#------------------------------------------------------------------------------

pop_regions <- st_intersection(nuseds_loc, regions_spat)

# Match regions back to orignal NuSEDS data (called "region_survey")
nuseds$region_survey <- pop_regions$region[match(nuseds$streamid, pop_regions$streamid)]
nuseds_loc$region_survey <- pop_regions$region[match(nuseds_loc$streamid, pop_regions$streamid)]

# There are some that are not matched...
sum(is.na(pop_regions$region))
length(unique(pop_regions$streamid))
length(unique(nuseds_loc$streamid))

# There are some streamid's missing
nuseds_loc[which(nuseds_loc$streamid %in% pop_regions$streamid == FALSE),]

plot(st_geometry(regions_spat), col = paste0(pnw_palette("Bay", n = 9), 30), border = pnw_palette("Bay", n = 9))
plot(st_geometry(nuseds_loc), pch = 21, col = 1, lwd = 0.5, add = TRUE, cex =0.8)

plot(st_geometry(nuseds_loc), add =TRUE, col = pnw_palette("Bay", n = 9)[match(nuseds_loc$region_survey, regions_spat$region)], pch = 19, cex = 0.5)
#plot(st_geometry(nuseds_loc), add =TRUE, col = pnw_palette("Bay", n = 9)[match(nuseds_loc$region_cu, regions_spat$region)], pch = 19, cex = 0.5)

plot(st_geometry(nuseds_loc[which(nuseds_loc$streamid %in% pop_regions$streamid == FALSE),]), add =TRUE, col = 1, pch = 19, cex = 0.5)

# Case of being too close to the boundary...
plot(st_geometry(nuseds_loc[which(nuseds_loc$streamid %in% pop_regions$streamid == FALSE),]), col = 1)
plot(st_geometry(regions_spat), border = pnw_palette("Bay", n = 9), col = NA, add = TRUE)

# Assign those
nuseds_loc$region_cu <- nuseds$region[match(nuseds_loc$streamid, nuseds$streamid)]
plot(st_geometry(nuseds_loc[which(nuseds_loc$streamid %in% pop_regions$streamid == FALSE),]), col = pnw_palette("Bay", n = 9)[match(nuseds_loc$region_cu[which(nuseds_loc$streamid %in% pop_regions$streamid == FALSE)], regions_spat$region)], add = TRUE, pch = 19, cex = 0.5)

#------------------------------------------------------------------------------
# Some misassignments based on CU regions for VIMI vs CC
#------------------------------------------------------------------------------

# Try buffering out?
regions_buffed <- st_buffer(regions_spat, dist = 0.1)

# Assign new regions one-by-one
# Start with VIMI
pop_regions_VIMI <- st_intersection(nuseds_loc[which(nuseds_loc$streamid %in% pop_regions$streamid == FALSE),], regions_buffed[regions_buffed$region == "Vancouver Island & Mainland Inlets",]) # distance is in m?

# plot(st_geometry(pop_regions_VIMI), pch = 19, col = pnw_palette("Bay", n = 9)[which(regions_buffed$region == "Vancouver Island & Mainland Inlets")], add = TRUE)

nuseds_loc$region_survey[nuseds_loc$streamid %in% pop_regions_VIMI$streamid] <- "Vancouver Island & Mainland Inlets"

# Fraser
pop_regions_Fraser <- st_intersection(nuseds_loc[is.na(nuseds_loc$region_survey),], regions_buffed[regions_buffed$region == "Fraser",]) # distance is in m?
plot(st_geometry(pop_regions_Fraser), pch = 19, col = pnw_palette("Bay", n = 9)[which(regions_buffed$region == "Fraser")], add = TRUE)

nuseds_loc$region_survey[nuseds_loc$streamid %in% pop_regions_Fraser$streamid] <- "Fraser"

# Haida Gwaii
pop_regions_HG <- st_intersection(nuseds_loc[is.na(nuseds_loc$region_survey),], regions_buffed[regions_buffed$region == "Haida Gwaii",]) # distance is in m?
plot(st_geometry(pop_regions_HG), pch = 19, col = pnw_palette("Bay", n = 9)[which(regions_buffed$region == "Haida Gwaii")], add = TRUE)

nuseds_loc$region_survey[nuseds_loc$streamid %in% pop_regions_HG$streamid] <- "Haida Gwaii"

# Central Coast
pop_regions_CC <- st_intersection(nuseds_loc[is.na(nuseds_loc$region_survey),], regions_buffed[regions_buffed$region == "Central Coast",]) # distance is in m?
plot(st_geometry(pop_regions_CC), pch = 19, col = pnw_palette("Bay", n = 9)[which(regions_buffed$region == "Central Coast")], add = TRUE)

nuseds_loc$region_survey[nuseds_loc$streamid %in% pop_regions_CC$streamid] <- "Central Coast"

# Plot remaining
plot(st_geometry(nuseds_loc[is.na(nuseds_loc$region_survey),]))
plot(st_geometry(regions_spat), border = pnw_palette("Bay", n = 9), col = paste0(pnw_palette("Bay", n = 9), 40), add = TRUE)

# Nass
pop_regions_Nass <- st_intersection(nuseds_loc[is.na(nuseds_loc$region_survey),], regions_buffed[regions_buffed$region == "Nass",]) # distance is in m?
plot(st_geometry(pop_regions_Nass), pch = 19, col = pnw_palette("Bay", n = 9)[which(regions_buffed$region == "Nass")], add = TRUE)

nuseds_loc$region_survey[nuseds_loc$streamid %in% pop_regions_Nass$streamid] <- "Nass"

# Skeena
pop_regions_Skeena <- st_intersection(nuseds_loc[is.na(nuseds_loc$region_survey),], regions_buffed[regions_buffed$region == "Skeena",]) # distance is in m?
plot(st_geometry(pop_regions_Skeena), pch = 19, col = pnw_palette("Bay", n = 9)[which(regions_buffed$region == "Skeena")], add = TRUE)

nuseds_loc$region_survey[nuseds_loc$streamid %in% pop_regions_Skeena$streamid] <- "Skeena"

# Any left?
sum(is.na(nuseds_loc$region_survey)) # No!

# How many are mismatched?
plot(st_geometry(nuseds_loc[which(nuseds_loc$region_cu != nuseds_loc$region_survey),]))
plot(st_geometry(regions_spat), border = pnw_palette("Bay", n = 9), col = paste0(pnw_palette("Bay", n = 9), 40), add = TRUE)

nuseds_loc[which(nuseds_loc$region_cu != nuseds_loc$region_survey),]

tapply(nuseds_loc$SPECIES[which(nuseds_loc$region_cu != nuseds_loc$region_survey)], nuseds_loc$SPECIES[which(nuseds_loc$region_cu != nuseds_loc$region_survey)], length)

nuseds_loc %>%
  filter(region_cu != region_survey, SPECIES == "Coho")

#------------------------------------------------------------------------------
# Write CSV of streamid <-> region_survey
#------------------------------------------------------------------------------

output <- nuseds_loc %>% 
  as.data.frame() %>% 
  select(streamid, region_survey)

write.csv(output, file = "data_output/region_survey.csv", row.names = FALSE)

nuseds$region_survey <- nuseds_loc$region_survey[match(nuseds$streamid, nuseds_loc$streamid)]

#------------------------------------------------------------------------------
# Plot regions and monitoring locations
#------------------------------------------------------------------------------

rivers_low <- readRDS("~/Salmon Watersheds Dropbox/Stephanie Peacock/X Drive/1_PROJECTS/1_Active/Climate Change/Data & Analysis/ccva/freshwater/data/spatial/layers/watercourse_lowRes.rds")

# Shoreline
shoreline <- st_read("~/Documents/Mapping/gshhg-shp-2.3.7/GSHHS_shp/f/GSHHS_f_L1.shp") %>%
  st_transform(crs = 4269) %>%
  st_crop(ymin = 40, ymax = 70, xmin = -160, xmax = -105)

# What about adding open points for those monitored in the most recent decade?
mon_decade <- nuseds %>%
  filter(Year >= 2013, !is.na(MAX_ESTIMATE)) %>%
  group_by(streamid) %>%
  summarise(mon = unique(streamid)) %>%
  select(streamid)
length(mon_decade$streamid)/length(unique(nuseds$streamid)) # 37%, right

# Bruno's palette:
colours_rg <- c("#CBC106", "#27993C", "#1C6838", "#8EBCB5", "#389CA7", "#4D83AB", "#CB7B26", "#BF565D", "#9E163C")
names(colours_rg) <- regions

loc_monitored <- nuseds %>%
  filter(!is.na(MAX_ESTIMATE), !is.na(streamid), streamid %in% mon_decade$streamid) %>%
  group_by(paste(latitude_final, longitude_final)) %>%
  reframe(lat = latitude_final, lon = longitude_final, region = region_survey) %>%
  distinct() %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4269)

loc_NOTmonitored <- nuseds %>%
  filter(!is.na(MAX_ESTIMATE), !is.na(streamid), streamid %in% mon_decade$streamid == FALSE) %>%
  group_by(paste(latitude_final, longitude_final)) %>%
  reframe(lat = latitude_final, lon = longitude_final, region = region_survey) %>%
  distinct() %>%
  filter(`paste(latitude_final, longitude_final)` %in% loc_monitored$`paste(latitude_final, longitude_final)` == FALSE) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4269)

# Doesn't quite add up to 2300..perhaps because I didn't distinguish different SYS_name at the same lat/lon?
# Leav it for now; minor
jpeg("figures/map.jpeg",
     width = 660, height = 685, units = 'px')
par(bg = 'white', mar = c(4,4,1,1), oma = rep(0, 4))
  plot(st_geometry(regions_spat), col = NA, border = NA, axes = TRUE, xlim = c(-142, -115), ylim = c(48, 67), bg = grey(0.8))
plot(st_geometry(shoreline), add = TRUE, col = "white", lwd = 0.8)
plot(st_geometry(regions_spat), col = paste0(colours_rg[regions_spat$region], 30), border = NA, add = TRUE)
plot(st_geometry(nuseds_loc), col = colours_rg[nuseds_loc$region_survey], pch = 19, cex = 0.4, add = TRUE)
# plot(st_geometry(nuseds_loc[nuseds_loc$streamid %in% mon_decade$streamid,]), col = colours_rg[nuseds_loc$region_survey[nuseds_loc$streamid %in% mon_decade$streamid]], pch = 19, cex = 0.5, add = TRUE)
legend("topright", pch = 19, col = colours_rg, legend = names(colours_rg), bty = "n")

dev.off()

# Separating monitored and not monitored in the past 10 years
par(bg = 'white', mar = c(4,4,1,1), oma = rep(0, 4))
plot(st_geometry(regions_spat), col = NA, border = NA, axes = TRUE, xlim = c(-140, -118), ylim = c(48.5, 66), bg = grey(0.8))
plot(st_geometry(shoreline), add = TRUE, col = "white", lwd = 0.8)
plot(st_geometry(regions_spat), col = paste0(colours_rg[regions_spat$region], 30), border = NA, add = TRUE)
plot(st_geometry(loc_NOTmonitored), col = colours_rg[loc_NOTmonitored$region], bg = "white", pch = 21, cex = 0.6, add = TRUE)
plot(st_geometry(loc_monitored), col = colours_rg[loc_monitored$region], pch = 19, cex = 0.5, add = TRUE)
plot(st_geometry(bbp), col = NA, border = 1, add = TRUE)

# Inset
plot(st_geometry(regions_spat), col = NA, border = NA, axes = TRUE, xlim = c(-132, -119), ylim = c(49, 55), bg = grey(0.8))
plot(st_geometry(shoreline), add = TRUE, col = "white", lwd = 0.8)
plot(st_geometry(regions_spat), col = paste0(colours_rg[regions_spat$region], 30), border = NA, add = TRUE)
plot(st_geometry(loc_NOTmonitored), col = colours_rg[loc_NOTmonitored$region], bg = "white", pch = 21, cex = 0.6, add = TRUE)
plot(st_geometry(loc_monitored), col = colours_rg[loc_monitored$region], pch = 19, cex = 0.5, add = TRUE)
legend("topright", pch = c(21, 18), pt.cex = c(0.8, 0.8), bty = "n", legend = c("Not monitored in 2013-2022", "Monitored at least once in 2013-2022"), cex = 0.8)

bbox_coords <- c(-133.3677,-117.6323, 48.7600,  55.2400)
names(bbox_coords) <- c("xmin","ymin","xmax","ymax")
bbp <- st_as_sfc(st_bbox(bbox_coords), crs = 4269)