
#'******************************************************************************
#' The goal of the script is assign NuSEDS populations to PSE regions based on 
#' latitude and longitud coordinates.
#' 
#' Files imported:
#' - nuseds_cuid_streamid_2024-11-25.csv    # the cleaned NuSEDS data available at: https://zenodo.org/records/14225367
#' - se_boundary_regions.shp                # the shape files for the regions as defined in the PSE (https://www.salmonexplorer.ca/)
#' - GSHHS_f_L1.shp                         # the shape file for the shorelines, to download from OAA, Global Self-consistent, Hierarchical, High-resolution Geography Database (GSHHG)
#' 
#' Files produced: 
#' - region_survey.csv      # the "region" (i.e. CU-related region)  and region_survey (i.e. the region where the survey was conducted), the two differ for certain populations
#' - map.jpeg               # old Figure 1 in the main text
#' - map_full.jpeg          # Part of Figure 1 in the main text
#' - map_zoomed.jpeg        # Part of Figure 1 in the main text
#'  
#'******************************************************************************

rm(list = ls())
graphics.off() 

library(sf)
library(dplyr)
library(PNWColors)            # not needed to produce the final figure
source("code/functions.R")

wd_data_input <- paste0(getwd(),"/data_input")
wd_data_output <- paste0(getwd(),"/data_output")

figures_print <- F

# Import datasets -----

#'* Load region shapefile *
regions_spat <- st_read(paste0(wd_data_input,"/se_boundary/se_boundary_regions.shp")) %>%
  st_transform(crs = 4269)

sf_use_s2(FALSE)

#'* Shoreline *
# TODO: download the file and unzip it in /data_input:
# NOAA, Global Self-consistent, Hierarchical, High-resolution Geography Database (GSHHG) 2.3.7., 2018-03-02,
# https://www.ngdc.noaa.gov/mgg/shorelines/data/gshhg/latest/
shoreline <- st_read(paste0(wd_data_input,"/gshhg-shp-2.3.7/GSHHS_shp/f/GSHHS_f_L1.shp")) %>%
  st_transform(crs = 4269) %>%
  st_crop(ymin = 40, ymax = 70, xmin = -160, xmax = -105)

#'* Import the cleaned NuSEDS data matched with PSF cuid and streamid *
#' This is the clean version of the New Salmon Escapement Database (NuSEDS).
#' TODO: to download at https://zenodo.org/records/14194639 and place it in the
#' /data_input folder.
nuseds <- read.csv(paste0(wd_data_input,"/nuseds_cuid_streamid_2025-04-15.csv"), 
                   header = T)

nuseds$region[nuseds$region == "Northern Transboundary"] <- "Transboundary"

# Edit the field "streamid" by "population_id" to avoid confusion
# the field is a unique combination between a CU (cuid) and a stream location (GFE_ID)
# = a popualation
colnames(nuseds)[colnames(nuseds) == "streamid"] <- "population_id"

# Number of data points without a cuid and population_id:
sum(is.na(nuseds$cuid))          # 2020
sum(is.na(nuseds$population_id)) # 2020

#
# Assign region to each population based on coordinates ------
#

# Create spatial variable of survey sites
nuseds_loc <- nuseds %>% 
  distinct(region,SPECIES, POP_ID, cuid, cu_name_pse, sys_nm, GFE_ID, Y_LAT, X_LONGT) %>%
  st_as_sf(coords = c("X_LONGT","Y_LAT"), crs = 4269)

# Number of populations:
nrow(nuseds_loc) # 7031

# Intersect the populations with the regions:
pop_regions <- st_intersection(nuseds_loc,regions_spat)

# Number of locations:
length(unique(nuseds$GFE_ID)) # 2361

# Match regions back to original NuSEDS data (called "region_survey")
# region_survey = the region where the monitorng was done, which can differ from 
# the CU-related "region" 
nuseds$region_survey <- pop_regions$region[match(nuseds$GFE_ID, pop_regions$GFE_ID)]
nuseds_loc$region_survey <- pop_regions$region[match(nuseds_loc$GFE_ID, pop_regions$GFE_ID)]

# Check the data in NuSEDS where the CU-related region differ from the geographic region
cond <- nuseds$region != nuseds$region_survey
unique(nuseds[cond,c("region","region_survey","sys_nm","GFE_ID","Y_LAT","X_LONGT")])

# There are some locations not intersected:
sum(is.na(nuseds$region)) # 0
sum(is.na(nuseds$region_survey)) # 40765

# Number of locations without a region_survey:
sum(is.na(nuseds_loc$region_survey)) # 860
sum(is.na(nuseds_loc$region)) # 0

# Show the monitoring locations:
plot(st_geometry(regions_spat), col = paste0(pnw_palette("Bay", n = 9), 30), 
     border = pnw_palette("Bay", n = 9))
plot(st_geometry(nuseds_loc), pch = 21, col = 1, lwd = 0.5, add = TRUE, cex =0.8)
plot(st_geometry(nuseds_loc), add =TRUE, 
     col = pnw_palette("Bay", n = 9)[match(nuseds_loc$region_survey, regions_spat$region)], 
     pch = 19, cex = 0.3)

# Plot the locations falling outside the region boundaries:
plot(st_geometry(nuseds_loc[is.na(nuseds_loc$region_survey),]), col = 1)
plot(st_geometry(regions_spat), border = pnw_palette("Bay", n = 9), col = NA, add = TRUE)

# Try buffering out
regions_buffed <- st_buffer(regions_spat, dist = 0.1)

# Assign new regions one-by-one
for(rg in unique(nuseds_loc$region)){
  cond_NA <- is.na(nuseds_loc$region_survey)
  cond_rg <- regions_buffed$region == rg
  pop_rg <- st_intersection(nuseds_loc[cond_NA,],regions_buffed[cond_rg,])
  nuseds_loc$region_survey[nuseds_loc$GFE_ID %in% pop_rg$GFE_ID] <- rg
  nuseds$region_survey[nuseds$GFE_ID %in% pop_rg$GFE_ID] <- rg
}

# any left?
sum(is.na(nuseds_loc$region_survey)) # No!
sum(is.na(nuseds$region_survey)) # No!

# How many are mismatched between the CU-related region ("region") and the 
# actual location of the stream ("region_survey")?
sum(nuseds_loc$region != nuseds_loc$region_survey)
# 126

# Plot the monitoring locations not matching the CU-related region:
plot(st_geometry(nuseds_loc[which(nuseds_loc$region != nuseds_loc$region_survey),]))
plot(st_geometry(regions_spat), border = pnw_palette("Bay", n = 9), 
     col = paste0(pnw_palette("Bay", n = 9), 40), add = TRUE)

#
# Write CSV of population_id <-> region_survey ------
#

output <- nuseds_loc %>% 
  as.data.frame() %>% 
  select(region,region_survey,GFE_ID) %>%
  unique()

nrow(output) # 2454

write.csv(output, file = "data_output/region_survey.csv", row.names = FALSE)

# 
# Plot regions and monitoring locations --------
#

cond_NA <- is.na(nuseds$population_id)
pop_NA <- unique(nuseds[cond_NA,c("region","SPECIES_QUALIFIED","CU_NAME","POP_ID","GFE_ID")]) # same nb rows as unique(nuseds[cond_NA,c("POP_ID","GFE_ID")])
nrow(pop_NA)                   # corresponding to 82 populations
sum(is.na(nuseds$cuid))        # corresponding number of data points: 2020

# Give a population_id to populations without a cuid
val_max <- max(nuseds$population_id, na.rm = T)
for(r in 1:nrow(pop_NA)){
  # r <- 1
  POP_ID <- pop_NA$POP_ID[r]
  GFE_ID <- pop_NA$GFE_ID[r]
  
  cond <- nuseds$POP_ID == POP_ID & nuseds$GFE_ID == GFE_ID & is.na(nuseds$cuid)
  
  if(!all(is.na(nuseds$population_id[cond]))){
    print("population_id is not all NA - BREAK")
    break
  }else{
    val_max <- val_max + 1
    nuseds$population_id[cond] <- val_max
  }
}

sum(is.na(nuseds$population_id)) # 0

# region_survey <- read.csv("data_output/region_survey.csv", header = T)
# head(region_survey)

nrow(nuseds) # 312539

regions <- c("Yukon","Transboundary","Haida Gwaii","Nass","Skeena","Central Coast",
             "Vancouver Island & Mainland Inlets","Fraser","Columbia")

#
colours_rg <- c("#CBC106", "#27993C", "#1C6838", "#8EBCB5", "#389CA7", "#4D83AB", "#CB7B26", "#BF565D", "#9E163C")
names(colours_rg) <- regions

#
if(figures_print){
  size <- 18
  jpeg("figures/map.jpeg",width = size * .964, height = size, units = 'cm', res = 300)
}
par(bg = 'white', mar = c(4,4,1,1), oma = rep(0, 4))
plot(st_geometry(regions_spat), col = NA, border = NA, axes = TRUE, 
     xlim = c(-142, -115), ylim = c(48, 67), bg = grey(0.8))
plot(st_geometry(shoreline), add = TRUE, col = "white", lwd = 0.8)
plot(st_geometry(regions_spat), col = paste0(colours_rg[regions_spat$region], 30), 
     border = NA, add = TRUE)
plot(st_geometry(nuseds_loc), col = colours_rg[nuseds_loc$region_survey], 
     pch = 19, cex = 0.4, add = TRUE)
legend("topright", pch = 19, col = colours_rg, legend = names(colours_rg), bty = "n")
if(figures_print){
  dev.off()
}

# Separating monitored and not monitored in the past 10 years
xlim_bbox <- c(-132.7,-118.2)
ylim_bbox <- c(49.0,55.0)
bbox_coords <- c(xlim_bbox,ylim_bbox)
bbox_coords <- c(-133.3,-117.5, 48.7, 55.3)
names(bbox_coords) <- c("xmin","xmax","ymin","ymax")
bbp <- st_as_sfc(st_bbox(bbox_coords), crs = 4269)

if(figures_print){
  # jpeg("figures/map.jpeg",width = 660, height = 685, units = 'px')
  size <- 18
  coef <- 586 / 747  #
  jpeg("figures/map_full.jpeg",width = size * coef, height = size, units = 'cm', res = 300)
}
par(bg = 'white', mar = c(4,4,1,1), oma = rep(0, 4))
plot(st_geometry(regions_spat), col = NA, border = NA, axes = TRUE, 
     xlim = c(-140, -118), ylim = c(48.5, 66), bg = grey(0.8))
plot(st_geometry(shoreline), add = TRUE, col = "white", lwd = 0.8)
plot(st_geometry(regions_spat), col = paste0(colours_rg[regions_spat$region], 30), 
     border = NA, add = TRUE)
plot(st_geometry(loc_NOTmonitored), col = colours_rg[loc_NOTmonitored$region], 
     bg = "white", pch = 21, cex = 0.6, add = TRUE)
plot(st_geometry(loc_monitored), col = colours_rg[loc_monitored$region], 
     pch = 19, cex = 0.5, add = TRUE)
plot(st_geometry(bbp), col = NA, border = 1, add = TRUE)
if(figures_print){
  dev.off()
}

if(figures_print){
  # jpeg("figures/map.jpeg",width = 660, height = 685, units = 'px')
  size <- 8
  coef <- 586 / 747  #
  pdf("figures/map_legend.pdf",width = size * coef, height = size)
}
plot.new()
legend("topright",regions, pch = 16, col = colours_rg, bty = 'n')
if(figures_print){
  dev.off()
}

# Make a figure distinguishing location with data points in the most recent decade
cond <- nuseds$Year > 2013 & !is.na(nuseds$MAX_ESTIMATE)
mon_decade <- data.frame(population_id = unique(nuseds$population_id[cond]))
mon_decade$population_id <- unique(nuseds$population_id[cond])

length(mon_decade$population_id)/length(unique(nuseds$population_id)) # 39.7%

loc_monitored <- nuseds %>%
  filter(!is.na(MAX_ESTIMATE), population_id %in% mon_decade$population_id) %>%
  group_by(paste(Y_LAT, X_LONGT)) %>%
  reframe(lat = Y_LAT, lon = X_LONGT, region = region_survey) %>%
  distinct() %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4269)

loc_NOTmonitored <- nuseds %>%
  filter(!is.na(MAX_ESTIMATE), ! population_id %in% mon_decade$population_id) %>%
  group_by(paste(Y_LAT, X_LONGT)) %>%
  reframe(lat = Y_LAT, lon = X_LONGT, region = region_survey) %>%
  distinct() %>%
  filter(! `paste(Y_LAT, X_LONGT)` %in% loc_monitored$`paste(Y_LAT, X_LONGT)`) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4269)

if(figures_print){
  # jpeg("figures/map.jpeg",width = 660, height = 685, units = 'px')
  size <- 18
  coef <- 845 / 597  # .964
  jpeg("figures/map_zoomed.jpeg",width = size * coef, height = size, units = 'cm', res = 300)
}
# Inset
par(mar = c(3,3,1,1))
plot(st_geometry(regions_spat), col = NA, border = NA, axes = TRUE, 
     xlim = xlim_bbox, ylim = ylim_bbox, bg = grey(0.8))
plot(st_geometry(shoreline), add = TRUE, col = "white", lwd = 0.8)
plot(st_geometry(regions_spat), col = paste0(colours_rg[regions_spat$region], 30), 
     border = NA, add = TRUE)
plot(st_geometry(loc_NOTmonitored), col = colours_rg[loc_NOTmonitored$region], 
     bg = "white", pch = 21, cex = 0.6, add = TRUE)
plot(st_geometry(loc_monitored), col = colours_rg[loc_monitored$region], 
     pch = 19, cex = 0.5, add = TRUE)
legend("topright", pch = c(21, 18), pt.cex = c(0.8, 0.8), bty = "n", cex = 0.8,
       legend = c("Not monitored in 2014-2023", "Monitored at least once in 2014-2023"))
if(figures_print){
  dev.off()
}

# END
