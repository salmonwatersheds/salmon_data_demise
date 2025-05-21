
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

#'* Shoreline * COMMENT OUT - TO CITE 
# downloadd from:
# https://www.ngdc.noaa.gov/mgg/shorelines/data/gshhg/latest/
#' TODO add a comment for user to download the shape file
# shoreline <- st_read("~/Documents/Mapping/gshhg-shp-2.3.7/GSHHS_shp/f/GSHHS_f_L1.shp") %>%
shoreline <- st_read(paste0(wd_data_input,"/gshhg-shp-2.3.7/GSHHS_shp/f/GSHHS_f_L1.shp")) %>%
  st_transform(crs = 4269) %>%
  st_crop(ymin = 40, ymax = 70, xmin = -160, xmax = -105)


#'* Import the cleaned NuSEDS data matched with PSF cuid and streamid *
#' This is the clean version of the New Salmon Escapement Database (NuSEDS). It 
#' must be downloaded at https://zenodo.org/records/14194639 and placed in the
#' /data_input folder.
nuseds <- read.csv(paste0(wd_data_input,"/nuseds_cuid_streamid_2025-04-15.csv"), 
                   header = T)

nuseds$region[nuseds$region == "Northern Transboundary"] <- "Transboundary"

# edit the field streamid --> population_id to avoid confusion
# the field is a unique combination between a CU (cuid) and a stream location (GFE_ID)
# = a popualation
colnames(nuseds)[colnames(nuseds) == "streamid"] <- "population_id"

# there
sum(is.na(nuseds$cuid))          # 2020
sum(is.na(nuseds$population_id)) # 2020

#
# Assign region to each population based on location ------

# Create spatial variable of survey sites
nuseds_loc <- nuseds %>% 
  # filter(!is.na(cuid)) %>%
  # distinct(SPECIES, POP_ID, cuid, cu_name_pse, streamid, sys_nm, GFE_ID, Y_LAT, X_LONGT) %>%
  distinct(region,SPECIES, POP_ID, cuid, cu_name_pse, sys_nm, GFE_ID, Y_LAT, X_LONGT) %>%
  st_as_sf(coords = c("X_LONGT","Y_LAT"), crs = 4269)

dim(nuseds_loc) # 7031  6766 "populations" 

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

# There are some population_id's missing
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


# Try buffering out
regions_buffed <- st_buffer(regions_spat, dist = 0.1)

# Assign new regions one-by-one
# Start with VIMI
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

# How many are mismatched?
sum(nuseds_loc$region != nuseds_loc$region_survey)
# 126 104
plot(st_geometry(nuseds_loc[which(nuseds_loc$region != nuseds_loc$region_survey),]))
plot(st_geometry(regions_spat), border = pnw_palette("Bay", n = 9), 
     col = paste0(pnw_palette("Bay", n = 9), 40), add = TRUE)

#
# Write CSV of population_id <-> region_survey ------

output <- nuseds_loc %>% 
  as.data.frame() %>% 
  select(region,region_survey,GFE_ID) %>%
  unique()

nrow(output) # 2454

write.csv(output, file = "data_output/region_survey.csv", row.names = FALSE)

# 
# Plot regions and monitoring locations --------

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

# nuseds <- merge(x = nuseds,
#                 y = unique(region_survey[,c("region_survey","GFE_ID")]),
#                 by = "GFE_ID",all.x = T)

regions <- c("Yukon","Transboundary","Haida Gwaii","Nass","Skeena","Central Coast",
             "Vancouver Island & Mainland Inlets","Fraser","Columbia")

# What about adding open points for those monitored in the most recent decade?
cond <- nuseds$Year > 2013 & !is.na(nuseds$MAX_ESTIMATE)
mon_decade <- data.frame(population_id = unique(nuseds$population_id[cond]))
# mon_decade$population_id <- unique(nuseds$population_id[cond])

length(mon_decade$population_id)/length(unique(nuseds$population_id)) # 39.7% 37%

# Bruno's palette:
colours_rg <- c("#CBC106", "#27993C", "#1C6838", "#8EBCB5", "#389CA7", "#4D83AB", "#CB7B26", "#BF565D", "#9E163C")
names(colours_rg) <- regions

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

# Doesn't quite add up to 2300..perhaps because I didn't distinguish different SYS_name at the same lat/lon?
# Leav it for now; minor
if(figures_print){
  # jpeg("figures/map.jpeg",width = 660, height = 685, units = 'px')
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
# plot(st_geometry(nuseds_loc[nuseds_loc$streamid %in% mon_decade$streamid,]), col = colours_rg[nuseds_loc$region_survey[nuseds_loc$streamid %in% mon_decade$streamid]], pch = 19, cex = 0.5, add = TRUE)
legend("topright", pch = 19, col = colours_rg, legend = names(colours_rg), bty = "n")
if(figures_print){
  dev.off()
}


# Separating monitored and not monitored in the past 10 years
# bbox_coords <- c(-133.3677,-117.6323, 48.7600,  55.2400)
# names(bbox_coords) <- c("xmin","ymin","xmax","ymax")
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
plot(st_geometry(regions_spat), col = NA, border = NA, axes = TRUE, xlim = c(-140, -118), ylim = c(48.5, 66), bg = grey(0.8))
plot(st_geometry(shoreline), add = TRUE, col = "white", lwd = 0.8)
plot(st_geometry(regions_spat), col = paste0(colours_rg[regions_spat$region], 30), border = NA, add = TRUE)
plot(st_geometry(loc_NOTmonitored), col = colours_rg[loc_NOTmonitored$region], bg = "white", pch = 21, cex = 0.6, add = TRUE)
plot(st_geometry(loc_monitored), col = colours_rg[loc_monitored$region], pch = 19, cex = 0.5, add = TRUE)
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

if(figures_print){
  # jpeg("figures/map.jpeg",width = 660, height = 685, units = 'px')
  size <- 18
  coef <- 845 / 597  # .964
  jpeg("figures/map_zoomed.jpeg",width = size * coef, height = size, units = 'cm', res = 300)
}
# Inset
par(mar = c(3,3,1,1))
plot(st_geometry(regions_spat), col = NA, border = NA, axes = TRUE, xlim = xlim_bbox, ylim = ylim_bbox, bg = grey(0.8))
plot(st_geometry(shoreline), add = TRUE, col = "white", lwd = 0.8)
plot(st_geometry(regions_spat), col = paste0(colours_rg[regions_spat$region], 30), border = NA, add = TRUE)
plot(st_geometry(loc_NOTmonitored), col = colours_rg[loc_NOTmonitored$region], bg = "white", pch = 21, cex = 0.6, add = TRUE)
plot(st_geometry(loc_monitored), col = colours_rg[loc_monitored$region], pch = 19, cex = 0.5, add = TRUE)
legend("topright", pch = c(21, 18), pt.cex = c(0.8, 0.8), bty = "n", 
       legend = c("Not monitored in 2014-2023", "Monitored at least once in 2014-2023"), cex = 0.8)
if(figures_print){
  dev.off()
}





