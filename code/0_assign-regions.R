
#'******************************************************************************
#' The goal of the script is assign NuSEDS populations to PSE regions based on 
#' locations.
#' 
#' Files imported:
#' - nuseds_cuid_streamid_2024-11-25.csv                 # the cleaned NuSEDS data avaiable at: https://zenodo.org/records/14225367
#' - se_boundary_regions.shp                             # the shape files for the regions as defined in the PSE (https://www.salmonexplorer.ca/)
#' 
#' Files produced: 
#' - region_survey.csv                                   # the region - populations (field "streamid") associations
#' - map.jpeg                                            # Figure 1 in the main text
#' - populationAssessed_catches_data_",option_NAs.xlsx   # The summary files where only NAs counts were removed (results presented in the supporting information)
#' 
#'******************************************************************************

#'TODO TO CLEAN MORE


library(sf)
library(dplyr)
library(PNWColors)            # not needed to produce the final figure
source("code/functions.R")

wd_data_input <- paste0(getwd(),"/data_input")
wd_data_output <- paste0(getwd(),"/data_output")

#------------------------------------------------------------------------------
# Read in data and create spatial variables
#------------------------------------------------------------------------------

# Load region shapefile
regions_spat <- st_read(paste0(wd_data_input,"/se_boundary/se_boundary_regions.shp")) %>%
  st_transform(crs = 4269)

sf_use_s2(FALSE)

#' Import the cleaned NuSEDS data matched with PSF cuid and streamid
#' This is the clean version of the New Salmon Escapement Database (NuSEDS). It 
#' must be downloaded at https://zenodo.org/records/14194639 and placed in the
#' /data_input folder.
nuseds <- read.csv(paste0(wd_data_input,"/nuseds_cuid_streamid_2025-04-15.csv"), 
                   header = T)

nuseds$region[nuseds$region == "Northern Transboundary"] <- "Transboundary"

# there
sum(is.na(nuseds$cuid))     # 
sum(is.na(nuseds$streamid)) # 


# Create spatial variable of survey sites
nuseds_loc <- nuseds %>% 
  # filter(!is.na(cuid)) %>%
  # distinct(SPECIES, POP_ID, cuid, cu_name_pse, streamid, sys_nm, GFE_ID, Y_LAT, X_LONGT) %>%
  distinct(region,SPECIES, POP_ID, cuid, cu_name_pse, sys_nm, GFE_ID, Y_LAT, X_LONGT) %>%
  st_as_sf(coords = c("X_LONGT","Y_LAT"), crs = 4269)

dim(nuseds_loc) # 7031  6766 "populations" 

#------------------------------------------------------------------------------
# Assign region to each population based on location
#------------------------------------------------------------------------------

pop_regions <- st_intersection(nuseds_loc, regions_spat)
nrow(pop_regions) # 6171 6092

length(unique(nuseds$GFE_ID)) # 2361

# Match regions back to original NuSEDS data (called "region_survey")
# nuseds$region_survey <- pop_regions$region[match(nuseds$streamid, pop_regions$streamid)]
# nuseds_loc$region_survey <- pop_regions$region[match(nuseds_loc$streamid, pop_regions$streamid)]
nuseds$region_survey <- pop_regions$region[match(nuseds$GFE_ID, pop_regions$GFE_ID)]
nuseds_loc$region_survey <- pop_regions$region[match(nuseds_loc$GFE_ID, pop_regions$GFE_ID)]

cond <- nuseds$region != nuseds$region_survey
unique(nuseds[cond,c("region","region_survey","sys_nm","GFE_ID","Y_LAT","X_LONGT")])


# There are some that are not matched...
sum(is.na(pop_regions$region))
sum(is.na(nuseds$region_survey)) # 40765
sum(is.na(nuseds_loc$region_survey)) # 860
sum(is.na(nuseds_loc$region)) # 0
# length(unique(pop_regions$streamid))
# length(unique(nuseds_loc$streamid))

# There are some streamid's missing
# nuseds_loc[which(nuseds_loc$streamid %in% pop_regions$streamid == FALSE),]

plot(st_geometry(regions_spat), col = paste0(pnw_palette("Bay", n = 9), 30), 
     border = pnw_palette("Bay", n = 9))
plot(st_geometry(nuseds_loc), pch = 21, col = 1, lwd = 0.5, add = TRUE, cex =0.8)

plot(st_geometry(nuseds_loc), add =TRUE, 
     col = pnw_palette("Bay", n = 9)[match(nuseds_loc$region_survey, regions_spat$region)], 
     pch = 19, cex = 0.3)
#plot(st_geometry(nuseds_loc), add =TRUE, col = pnw_palette("Bay", n = 9)[match(nuseds_loc$region_cu, regions_spat$region)], pch = 19, cex = 0.5)

# plot(st_geometry(nuseds_loc[which(nuseds_loc$streamid %in% pop_regions$streamid == FALSE),]), 
#      add =TRUE, col = 1, pch = 19, cex = 0.5)

# Case of being too close to the boundary...
# plot(st_geometry(nuseds_loc[which(nuseds_loc$streamid %in% pop_regions$streamid == FALSE),]), col = 1)
plot(st_geometry(nuseds_loc[is.na(nuseds_loc$region_survey),]), col = 1)
plot(st_geometry(regions_spat), border = pnw_palette("Bay", n = 9), col = NA, add = TRUE)

# Assign those
# nuseds_loc$region_cu <- nuseds$region[match(nuseds_loc$streamid, nuseds$streamid)]
# plot(st_geometry(nuseds_loc[which(nuseds_loc$streamid %in% pop_regions$streamid == FALSE),]), 
#      col = pnw_palette("Bay", n = 9)[match(nuseds_loc$region_cu[which(nuseds_loc$streamid %in% pop_regions$streamid == FALSE)], regions_spat$region)], 
#      add = TRUE, pch = 19, cex = 0.5)

#------------------------------------------------------------------------------
# Some misassignments based on CU regions for VIMI vs CC
#------------------------------------------------------------------------------

# Try buffering out?
regions_buffed <- st_buffer(regions_spat, dist = 0.1)

# Assign new regions one-by-one
# Start with VIMI
rg <- "Vancouver Island & Mainland Inlets"
cond_NA <- is.na(nuseds_loc$region_survey)
cond_rg <- regions_buffed$region == rg
pop_rg <- st_intersection(nuseds_loc[cond_NA,],regions_buffed[cond_rg,])
nuseds_loc$region_survey[nuseds_loc$GFE_ID %in% pop_rg$GFE_ID] <- rg
nuseds$region_survey[nuseds$GFE_ID %in% pop_rg$GFE_ID] <- rg

rg <- "Fraser"
cond_NA <- is.na(nuseds_loc$region_survey)
cond_rg <- regions_buffed$region == rg
pop_rg <- st_intersection(nuseds_loc[cond_NA,],regions_buffed[cond_rg,])
nuseds_loc$region_survey[nuseds_loc$GFE_ID %in% pop_rg$GFE_ID] <- rg
nuseds$region_survey[nuseds$GFE_ID %in% pop_rg$GFE_ID] <- rg

rg <- "Haida Gwaii"
cond_NA <- is.na(nuseds_loc$region_survey)
cond_rg <- regions_buffed$region == rg
pop_rg <- st_intersection(nuseds_loc[cond_NA,],regions_buffed[cond_rg,])
nuseds_loc$region_survey[nuseds_loc$GFE_ID %in% pop_rg$GFE_ID] <- rg
nuseds$region_survey[nuseds$GFE_ID %in% pop_rg$GFE_ID] <- rg

rg <- "Central Coast"
cond_NA <- is.na(nuseds_loc$region_survey)
cond_rg <- regions_buffed$region == rg
pop_rg <- st_intersection(nuseds_loc[cond_NA,],regions_buffed[cond_rg,])
nuseds_loc$region_survey[nuseds_loc$GFE_ID %in% pop_rg$GFE_ID] <- rg
nuseds$region_survey[nuseds$GFE_ID %in% pop_rg$GFE_ID] <- rg

rg <- "Nass"
cond_NA <- is.na(nuseds_loc$region_survey)
cond_rg <- regions_buffed$region == rg
pop_rg <- st_intersection(nuseds_loc[cond_NA,],regions_buffed[cond_rg,])
nuseds_loc$region_survey[nuseds_loc$GFE_ID %in% pop_rg$GFE_ID] <- rg
nuseds$region_survey[nuseds$GFE_ID %in% pop_rg$GFE_ID] <- rg

rg <- "Skeena"
cond_NA <- is.na(nuseds_loc$region_survey)
cond_rg <- regions_buffed$region == rg
pop_rg <- st_intersection(nuseds_loc[cond_NA,],regions_buffed[cond_rg,])
nuseds_loc$region_survey[nuseds_loc$GFE_ID %in% pop_rg$GFE_ID] <- rg
nuseds$region_survey[nuseds$GFE_ID %in% pop_rg$GFE_ID] <- rg

# any left?
sum(is.na(nuseds_loc$region_survey)) # No!
sum(is.na(nuseds$region_survey)) # No!

# How many are mismatched?
plot(st_geometry(nuseds_loc[which(nuseds_loc$region != nuseds_loc$region_survey),]))
plot(st_geometry(regions_spat), border = pnw_palette("Bay", n = 9), col = paste0(pnw_palette("Bay", n = 9), 40), add = TRUE)

# Write CSV of streamid <-> region_survey ------

output <- nuseds_loc %>% 
  as.data.frame() %>% 
  select(region,region_survey,GFE_ID) %>%
  unique()

nrow(output) # 2454

write.csv(output, file = "data_output/region_survey.csv", row.names = FALSE)

# 
# Plot regions and monitoring locations --------


rivers_low <- readRDS("~/Salmon Watersheds Dropbox/Stephanie Peacock/X Drive/1_PROJECTS/1_Active/Climate Change/Data & Analysis/ccva/freshwater/data/spatial/layers/watercourse_lowRes.rds")

rivers_low <- "C:/Users/bcarturan/Salmon Watersheds Dropbox/Bruno Carturan/X Drive/1_Active/Climate Change/Data & Analysis/ccva/freshwater/data/spatial/layers/watercourse_lowRes.rds"

# Shoreline COMMENT OUT - TO CITE 
#' TODO add a comment for user to download the shape file
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