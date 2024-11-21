###############################################################################
# Assign NuSEDS populations to PSE regions based on locations
###############################################################################


library(sf)
library(dplyr)
library(PNWColors)
source("code/functions.R")

#------------------------------------------------------------------------------
# Read in data and create spatial variables
#------------------------------------------------------------------------------

# Load region shapefile
map_root <- "~/Salmon Watersheds Dropbox/Stephanie Peacock/X Drive/5_DATA/Mapping/"
regions_spat <- st_read(paste0(map_root, "study areas/shapefiles/PSE_regions/se_boundary_regions.shp")) %>%
  st_transform(crs = 4269)
sf_use_s2(FALSE)

# Load NuSEDS spawner surveys (incl. lat/lon)
wd_data_input_PSF <- "/Users/stephaniepeacock/Salmon\ Watersheds\ Dropbox/Stephanie\ Peacock/X\ Drive/1_PROJECTS/1_Active/Population\ Methods\ and\ Analysis/population-indicators/spawner-surveys/output"

nuseds <- import_mostRecent_file_fun(wd = paste0(wd_data_input_PSF,"/archive"),
                                      pattern = "nuseds_cuid_streamid")
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

write.csv(nuseds_loc %>% select(streamid, region_survey), file = "data_input/region_survey.csv", row.names = FALSE)

#------------------------------------------------------------------------------
# Plot regions and monitoring locations
#------------------------------------------------------------------------------

rivers_low <- readRDS("~/Salmon Watersheds Dropbox/Stephanie Peacock/X Drive/1_PROJECTS/1_Active/Climate Change/Data & Analysis/ccva/freshwater/data/spatial/layers/watercourse_lowRes.rds")

# Shoreline
shoreline <- st_read("~/Documents/Mapping/gshhg-shp-2.3.7/GSHHS_shp/f/GSHHS_f_L1.shp") %>%
  st_transform(crs = 4269) %>%
  st_crop(ymin = 40, ymax = 70, xmin = -160, xmax = -105)


# Bruno's palette:
# colours_rg <- paletteer_d("peRReo::planb", n = length(regions), type = "discrete", ) # Monet Panb 
colours_rg <- paletteer_d("ltc::crbhits", n = length(regions)) # SP: not in love with this one; feel free to change
# colours_rg <- c("#389CA7", "#8EBCB5", "#4D83AB", "#BF565D", "#CB7B26", "#1C6838", "#9E163C", "#27993C", "#CBC106")
colours_rg <- c("#CBC106", "#27993C", "#1C6838", "#8EBCB5", "#389CA7", "#4D83AB", "#CB7B26", "#BF565D", "#9E163C")
names(colours_rg) <- regions
jpeg("figures/map.jpeg",
     width = 660, height = 685, units = 'px')
par(bg = 'white', mar = c(4,4,1,1), oma = rep(0, 4))
  plot(st_geometry(regions_spat), col = NA, border = NA, axes = TRUE, xlim = c(-142, -115), ylim = c(48, 67), bg = grey(0.8))
plot(st_geometry(shoreline), add = TRUE, col = "white", lwd = 0.8)
plot(st_geometry(regions_spat), col = paste0(colours_rg[regions_spat$region], 30), border = NA, add = TRUE)
plot(st_geometry(nuseds_loc), col = colours_rg[nuseds_loc$region_survey], pch = 19, cex = 0.5, add = TRUE)
legend("topright", pch = 19, col = colours_rg, legend = names(colours_rg), bty = "n")

dev.off()