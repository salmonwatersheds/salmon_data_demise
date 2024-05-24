


wd_figures <- paste0(getwd(),"/figures")
wd_data_input <- paste0(getwd(),"/data_input")
wd_data_input_PSF <- "C:/Users/bcarturan/Salmon Watersheds Dropbox/Bruno Carturan/X Drive/1_PROJECTS/1_Active/Population Methods and Analysis/population-indicators/spawner-surveys/output"


library(tidyr)
library(dplyr)

source("code/functions.R")
source("code/colours.R")

figures_print <- F

#' * Import the cleaned NuSEDS data matched with PSF cuid and streamid *
nuseds <- read.csv(paste0(wd_data_input_PSF,"/NuSEDS_escapement_data_collated_20240419.csv"),
                   header = T)

colToKeep <- c("POP_ID","SPECIES","CU_NAME","SYSTEM_SITE","GFE_ID","Year",
               "ESTIMATE_CLASSIFICATION","ESTIMATE_METHOD")

nuseds <- read.csv(paste0(wd_data_input_PSF,"/nuseds_cuid_streamid_20240419.csv"),
                   header = T)

head(nuseds)
sum(is.na(nuseds$MAX_ESTIMATE)) # 155984
nuseds <- nuseds[!is.na(nuseds$MAX_ESTIMATE),]

colToKeep <- c("region","SPECIES","cu_name_pse","cuid","SYSTEM_SITE","GFE_ID","Year",
               "ESTIMATE_CLASSIFICATION","ESTIMATE_METHOD","stream_survey_quality")

nuseds <- nuseds[,colToKeep]

#' * Import the definition of the different fields of these two datasets *
fields_def <- nuseds_fields_definitions_fun(wd_references = wd_data_input)

fields_def$cu_system_sites

#
# Figures DRAFT ------

#'* Number of survey per years (all) *

#' Nb of surveys per year:
nuseds_surveyYear <- nuseds %>%
  group_by(Year) %>%
  summarise(count = n()) %>%
  arrange(Year)

head(nuseds_surveyYear)
hist(nuseds_surveyYear$count)

coef <- 1.5
if(figures_print){
  jpeg(paste0(wd_figures,"/Number_surveys_perYear.jpeg"),
       width = 15 * coef, height = 10 * coef, units = 'cm', res = 300)
}
par(mar = c(4.5,4.5,1,.5))
plot(x = nuseds_surveyYear$Year, y = nuseds_surveyYear$count, type = "l", lwd = 2,
     ylab = "Number of populations", xlab = "Years")
#points(x = nuseds_surveyYear$Year, y = nuseds_surveyYear$count, pch = 16)
if(figures_print){
  dev.off()
}

#' COMMENT: can we find why it went up in ~1950 and down in ~2015?
#' 

#'* Number of survey per years per regions *

regions <- unique(nuseds$region)

x_max <- sapply(regions, function(reg){
  cond <- nuseds$region == reg
  nuseds_cut <- nuseds[cond,]
  nuseds_cut_surveyYear <- nuseds_cut %>%
    group_by(Year) %>%
    summarise(count = n()) %>%
    arrange(Year)
  return(max(nuseds_cut_surveyYear$count))
})

regions <- regions[rev(order(x_max))]

#' The declined is observed in each region where assessment has been important:

coef <- 1.5
if(figures_print){
  jpeg(paste0(wd_figures,"/Number_surveys_perRegions_perYear.jpeg"),
       width = 15 * coef, height = 10 * coef, units = 'cm', res = 300)
}
par(mar = c(4.5,4.5,1,.5))
plot(1, type = "l", lwd = 2, xlim = range(nuseds$Year), ylim = c(0,max(x_max)),
     ylab = "Number of populations", xlab = "Years")

colours <- rainbow(n = length(regions))
for(i in 1:length(regions)){
  reg <- regions[i]
  cond <- nuseds$region == reg
  nuseds_cut <- nuseds[cond,]
  nuseds_cut_surveyYear <- nuseds_cut %>%
    group_by(Year) %>%
    summarise(count = n()) %>%
    arrange(Year)
  lines(x = nuseds_cut_surveyYear$Year, y = nuseds_cut_surveyYear$count, 
        pch = 16, col = colours[i], lwd = 2)
  # points(x = nuseds_cut_surveyYear$Year, y = nuseds_cut_surveyYear$count,
  #        pch = 16, col = colours[i])
}
legend("topleft",regions, col = colours, lwd = 2, bty = "n")
if(figures_print){
  dev.off()
}

#'* Number of survey per years per species *

species <- unique(nuseds$SPECIES)

x_max <- sapply(species, function(sp){
  cond <- nuseds$SPECIES == sp
  nuseds_cut <- nuseds[cond,]
  nuseds_cut_surveyYear <- nuseds_cut %>%
    group_by(Year) %>%
    summarise(count = n()) %>%
    arrange(Year)
  return(max(nuseds_cut_surveyYear$count))
})

species <- species[rev(order(x_max))]

#' The declined is observed in each region where assessment has been important:

coef <- 1.5
if(figures_print){
  jpeg(paste0(wd_figures,"/Number_surveys_perSpecies_perYear.jpeg"),
       width = 15 * coef, height = 10 * coef, units = 'cm', res = 300)
}
par(mar = c(4.5,4.5,1,.5))
plot(1, type = "l", lwd = 2, xlim = range(nuseds$Year), ylim = c(0,max(x_max)),
     ylab = "Number of populations", xlab = "Years")

colours <- species_cols_dark[species]
colours <- species_cols_light[species]
for(i in 1:length(species)){
  sp <- species[i]
  cond <- nuseds$SPECIES == sp
  nuseds_cut <- nuseds[cond,]
  nuseds_cut_surveyYear <- nuseds_cut %>%
    group_by(Year) %>%
    summarise(count = n()) %>%
    arrange(Year)
  lines(x = nuseds_cut_surveyYear$Year, y = nuseds_cut_surveyYear$count, 
        pch = 16, col = colours[i], lwd = 2)
  # points(x = nuseds_cut_surveyYear$Year, y = nuseds_cut_surveyYear$count,
  #        pch = 16, col = colours[i])
}
legend("topleft",species, col = colours, lwd = 2, bty = "n")
if(figures_print){
  dev.off()
}

#'* Number of survey per years per regions > species *

# Still retain the order of the most surveyed regions at the gloval scale:
x_max <- sapply(regions, function(reg){
  cond <- nuseds$region == reg
  nuseds_cut <- nuseds[cond,]
  nuseds_cut_surveyYear <- nuseds_cut %>%
    group_by(Year) %>%
    summarise(count = n()) %>%
    arrange(Year)
  return(max(nuseds_cut_surveyYear$count))
})

regions <- regions[rev(order(x_max))]

# same for the species:
species <- unique(nuseds$SPECIES)

x_max <- sapply(species, function(sp){
  cond <- nuseds$SPECIES == sp
  nuseds_cut <- nuseds[cond,]
  nuseds_cut_surveyYear <- nuseds_cut %>%
    group_by(Year) %>%
    summarise(count = n()) %>%
    arrange(Year)
  return(max(nuseds_cut_surveyYear$count))
})

species <- species[rev(order(x_max))]
colours <- species_cols_light[species]
#

# Find the maximum number of surveys per region-species associations:
y_max <- c()
for(i in 1:length(regions)){
  reg <- regions[i]
  cond <- nuseds$region == reg
  nuseds_reg <- nuseds[cond,]
  
  species_reg <- species[species %in% unique(nuseds_reg$SPECIES)]

  x_max <- sapply(species_reg, function(sp){
    cond <- nuseds_reg$SPECIES == sp
    nuseds_cut <- nuseds_reg[cond,]
    nuseds_cut_surveyYear <- nuseds_cut %>%
      group_by(Year) %>%
      summarise(count = n()) %>%
      arrange(Year)
    return(max(nuseds_cut_surveyYear$count))
  })
  
  y_max <- c(y_max,max(x_max))
}

coef <- 1
if(figures_print){
  jpeg(paste0(wd_figures,"/Number_surveys_perRegions_perSpecies_perYear.jpeg"),
       width = 25 * coef, height = 20 * coef, units = 'cm', res = 300)
}
layout(matrix(1:length(regions), nrow = 3, byrow = T), 
       widths = c(1.15,1,1), heights = c(1,1,1.2))
for(i in 1:length(regions)){
  
  side1 <- .5
  side2 <- .5
  xaxt <- "n"
  yaxt <- "n"
  
  if(i %in% 7:9){
    side1 <- 4.5
    xaxt <- "s"
  }
  if(i %in% c(1,4,7)){
    side2 <- 4.5
    yaxt <- "s"
  }
  
  reg <- regions[i]
  cond <- nuseds$region == reg
  nuseds_reg <- nuseds[cond,]
  
  species_reg <- species[species %in% unique(nuseds_reg$SPECIES)]
  colours_reg <- colours[species_reg]
  
  par(mar = c(side1,side2,3,.5))
  plot(1, type = "l", lwd = 2, xlim = range(nuseds$Year), ylim = c(0,max(y_max)),
       ylab = "Number of populations", xlab = "Years", main = reg, xaxt = xaxt, yaxt = yaxt)
  
  for(j in 1:length(species_reg)){
    sp <- species_reg[j]
    cond <- nuseds_reg$SPECIES == sp
    nuseds_cut <- nuseds_reg[cond,]
    nuseds_cut_surveyYear <- nuseds_cut %>%
      group_by(Year) %>%
      summarise(count = n()) %>%
      arrange(Year)
    lines(x = nuseds_cut_surveyYear$Year, y = nuseds_cut_surveyYear$count, 
          pch = 16, col = colours_reg[j], lwd = 2)
    # points(x = nuseds_cut_surveyYear$Year, y = nuseds_cut_surveyYear$count,
    #        pch = 16, col = colours[i])
  }
  legend("topleft",species_reg, col = colours_reg, lwd = 2, bty = "n")
}
if(figures_print){
  dev.off()
}

#' --> does not reveal the sampling effort. 
#' Comment: show the proportion of streams surveyed vs. total number of streams?

#'* Number of survey per years per species > regions *

# Still retain the order of the most surveyed regions at the global scale:
x_max <- sapply(regions, function(reg){
  cond <- nuseds$region == reg
  nuseds_cut <- nuseds[cond,]
  nuseds_cut_surveyYear <- nuseds_cut %>%
    group_by(Year) %>%
    summarise(count = n()) %>%
    arrange(Year)
  return(max(nuseds_cut_surveyYear$count))
})

regions <- regions[rev(order(x_max))]

# same for the species:
species <- unique(nuseds$SPECIES)

x_max <- sapply(species, function(sp){
  cond <- nuseds$SPECIES == sp
  nuseds_cut <- nuseds[cond,]
  nuseds_cut_surveyYear <- nuseds_cut %>%
    group_by(Year) %>%
    summarise(count = n()) %>%
    arrange(Year)
  return(max(nuseds_cut_surveyYear$count))
})

species <- species[rev(order(x_max))]
colours <- species_cols_light[species]
#

# Find the maximum number of surveys per region-species associations:
y_max <- c()
for(i in 1:length(species)){
  sp <- species[i]
  cond <- nuseds$SPECIES == sp
  nuseds_sp <- nuseds[cond,]
  
  region_sp <- regions[regions %in% unique(nuseds_sp$region)]
  
  x_max <- sapply(region_sp, function(reg){
    cond <- nuseds_sp$region == reg
    nuseds_cut <- nuseds_sp[cond,]
    nuseds_cut_surveyYear <- nuseds_cut %>%
      group_by(Year) %>%
      summarise(count = n()) %>%
      arrange(Year)
    return(max(nuseds_cut_surveyYear$count))
  })
  
  y_max <- c(y_max,max(x_max))
}

colours <- rainbow(n = length(regions))
names(colours) <- regions

coef <- 1
if(figures_print){
  jpeg(paste0(wd_figures,"/Number_surveys_perSpecies_perRegions_perYear.jpeg"),
       width = 30 * coef, height = 20 * coef, units = 'cm', res = 300)
}
layout(matrix(1:(length(species)+1), nrow = 2, byrow = T), 
       widths = c(1.15,1,1), heights = c(1,1.2))
for(i in 1:length(species)){
  # i <- 1
  side1 <- .5
  side2 <- .5
  xaxt <- "n"
  yaxt <- "n"
  
  if(i %in% 4:6){
    side1 <- 4.5
    xaxt <- "s"
  }
  if(i %in% c(1,4)){
    side2 <- 4.5
    yaxt <- "s"
  }
  
  sp <- species[i]
  cond <- nuseds$SPECIES == sp
  nuseds_sp <- nuseds[cond,]
  
  regions_sp <- regions[regions %in% unique(nuseds_sp$region)]
  colours_reg <- colours[regions_sp]
  
  par(mar = c(side1,side2,3,.5))
  plot(1, type = "l", lwd = 2, xlim = range(nuseds$Year), ylim = c(0,max(y_max)),
       ylab = "Number of populations", xlab = "Years", main = sp, xaxt = xaxt, yaxt = yaxt)
  
  for(j in 1:length(regions_sp)){
    reg <- regions_sp[j]
    cond <- nuseds_sp$region == reg
    nuseds_cut <- nuseds_sp[cond,]
    nuseds_cut_surveyYear <- nuseds_cut %>%
      group_by(Year) %>%
      summarise(count = n()) %>%
      arrange(Year)
    lines(x = nuseds_cut_surveyYear$Year, y = nuseds_cut_surveyYear$count, 
          pch = 16, col = colours_reg[j], lwd = 2)
    # points(x = nuseds_cut_surveyYear$Year, y = nuseds_cut_surveyYear$count,
    #        pch = 16, col = colours[i])
  }
  legend("topleft",regions_sp, col = colours_reg, lwd = 2, bty = "n")
  
}
if(figures_print){
  dev.off()
}

#'* Number of survey per years per stream_survey_quality *

dataset <- nuseds

var <- "stream_survey_quality"
var_values <- unique(dataset[,var])
var_values[is.na(var_values)] <- "NA"

x_max <- c()
for(i in 1:length(var_values)){
  v <- var_values[i]
  if(v == 'NA'){
    cond <- is.na(dataset[,var])
    dataset_cut <- dataset[cond,]
    dataset_cut[,var] <- "NA"
  }else{
    cond <- dataset[,var] == v
    dataset_cut <- dataset[cond,]
  }
  dataset_cut_surveyYear <- dataset_cut %>%
    group_by(Year) %>%
    summarise(count = n()) %>%
    arrange(Year)
  
  x_max <- c(x_max,max(dataset_cut_surveyYear$count))
  names(x_max)[i] <- v
}

x_max

# order var_values by abundance
var_values <- var_values[rev(order(x_max))]

#' The declined is observed in each region where assessment has been important:

coef <- 1.5
if(figures_print){
  jpeg(paste0(wd_figures,"/Number_surveys_stream_survey_quality_perYear.jpeg"),
       width = 15 * coef, height = 10 * coef, units = 'cm', res = 300)
}
par(mar = c(4.5,4.5,3,.5))
plot(1, type = "l", lwd = 2, xlim = range(nuseds$Year), ylim = c(0,max(x_max)),
     ylab = "Number of populations", xlab = "Years", main = var)

colours <- rainbow(n = length(var_values))
names(colours) <- var_values
for(i in 1:length(var_values)){
  v <- var_values[i]
  if(v == 'NA'){
    cond <- is.na(dataset[,var])
    dataset_cut <- dataset[cond,]
    dataset_cut[,var] <- "NA"
  }else{
    cond <- dataset[,var] == v
    dataset_cut <- dataset[cond,]
  }
  dataset_cut_surveyYear <- dataset_cut %>%
    group_by(Year) %>%
    summarise(count = n()) %>%
    arrange(Year)
  lines(x = dataset_cut_surveyYear$Year, y = dataset_cut_surveyYear$count, 
        pch = 16, col = colours[i], lwd = 2)
  # points(x = nuseds_cut_surveyYear$Year, y = nuseds_cut_surveyYear$count,
  #        pch = 16, col = colours[i])
}
legend("topleft",var_values, col = colours, lwd = 2, bty = "n")
if(figures_print){
  dev.off()
}


#'* Number of survey per years per ESTIMATE_METHOD *

dataset <- nuseds

var <- "ESTIMATE_METHOD"
var_values <- unique(dataset[,var])
var_values[is.na(var_values)] <- "NA"

x_max <- c()
for(i in 1:length(var_values)){
  v <- var_values[i]
  if(v == 'NA'){
    cond <- is.na(dataset[,var])
    dataset_cut <- dataset[cond,]
    dataset_cut[,var] <- "NA"
  }else{
    cond <- dataset[,var] == v
    dataset_cut <- dataset[cond,]
  }
  dataset_cut_surveyYear <- dataset_cut %>%
    group_by(Year) %>%
    summarise(count = n()) %>%
    arrange(Year)
  
  x_max <- c(x_max,max(dataset_cut_surveyYear$count))
  names(x_max)[i] <- v
}

x_max

# order var_values by abundance
var_values <- var_values[rev(order(x_max))]

#' The declined is observed in each region where assessment has been important:

coef <- 2.5
if(figures_print){
  jpeg(paste0(wd_figures,"/Number_surveys_ESTIMATE_METHOD_perYear.jpeg"),
       width = 15 * coef, height = 10 * coef, units = 'cm', res = 300)
}
par(mar = c(4.5,4.5,3,.5))
plot(1, type = "l", lwd = 2, xlim = range(nuseds$Year), ylim = c(0,max(x_max)),
     ylab = "Number of populations", xlab = "Years", main = var)

colours <- rainbow(n = length(var_values))
names(colours) <- var_values
for(i in 1:length(var_values)){
  v <- var_values[i]
  if(v == 'NA'){
    cond <- is.na(dataset[,var])
    dataset_cut <- dataset[cond,]
    dataset_cut[,var] <- "NA"
  }else{
    cond <- dataset[,var] == v
    dataset_cut <- dataset[cond,]
  }
  dataset_cut_surveyYear <- dataset_cut %>%
    group_by(Year) %>%
    summarise(count = n()) %>%
    arrange(Year)
  lines(x = dataset_cut_surveyYear$Year, y = dataset_cut_surveyYear$count, 
        pch = 16, col = colours[i], lwd = 2)
  # points(x = nuseds_cut_surveyYear$Year, y = nuseds_cut_surveyYear$count,
  #        pch = 16, col = colours[i])
}
legend("topleft",var_values, col = colours, lwd = 2, bty = "n")
if(figures_print){
  dev.off()
}

#
#' Long format:
m <- matrix(1:(length(regions)+1), ncol = 1)
widths <- 1
heights <- c(rep(1,5, 1.2))
side1_1 <- 0.5
xaxt_1 <- 'n'
xlab_1 <- ""
layout(m, widths = widths, heights = heights)
#' Total nb of population surveyed per year:
nuseds_surveyYear <- nuseds %>%
  group_by(Year) %>%
  summarise(count = n()) %>%
  arrange(Year)

par(mar = c(side1_1,4.5,1,.5))
plot(x = nuseds_surveyYear$Year, y = nuseds_surveyYear$count, type = "l", lwd = 2,
     ylab = "Number of populations", xlab =  xlab_1, xaxt = xaxt_1)
legend("topleft","Total", col = "black", lwd = 2, bty = "n")

# Nb of population surveyed per year for each region:
for(rg in regions){
  # rg <- regions[1]
  side1 <- .5
  xlab <- ""
  xaxt <- "n"
  if(which(rg == regions) == length(regions)){
    side1 <- 4.5
    xlab <- "Year"
    xaxt <- "s"
  }
  cond <- nuseds$region == rg
  plot_countsYear_oneVar_fun(dataset = nuseds[cond,], varCount = "SPECIES", varX = "Year", 
                             ylab =  "Number of populations", xlab = xlab, xaxt = xaxt,
                             colours = species_cols_dark, 
                             side1 = side1, side3 = .5, main = "", y_max = 350)
  
  legend("topright",rg, col = "black", bty = "n")
}

#
# Figures SELECTED ------------

#'* 1st, order regions and species by abundance *

regions <- unique(nuseds$region)
species <- unique(nuseds$SPECIES)

# Still retain the order of the most surveyed regions at the global scale:
x_max <- sapply(regions, function(reg){
  cond <- nuseds$region == reg
  nuseds_cut <- nuseds[cond,]
  nuseds_cut_surveyYear <- nuseds_cut %>%
    group_by(Year) %>%
    summarise(count = n()) %>%
    arrange(Year)
  return(max(nuseds_cut_surveyYear$count))
})

regions <- regions[rev(order(x_max))]

# same for the species:
x_max <- sapply(species, function(sp){
  cond <- nuseds$SPECIES == sp
  nuseds_cut <- nuseds[cond,]
  nuseds_cut_surveyYear <- nuseds_cut %>%
    group_by(Year) %>%
    summarise(count = n()) %>%
    arrange(Year)
  return(max(nuseds_cut_surveyYear$count))
})

species <- species[rev(order(x_max))]
colours_sp <- species_cols_light[species]

#'* Total nb populations/yr + per refion ; Wide format *
#' 
figures_print <- T

coef <- 0.8
if(figures_print){
  jpeg(paste0(wd_figures,"/Number_surveys_perYear_total_Regions_species.jpeg"),
       width = 21.59 * coef, height = 27.94 * coef, units = 'cm', res = 300)
}
m <- matrix(c(1,1,1:(length(regions)+1)), ncol = 3, byrow = T)
layout(m, widths =  c(1.25,1,1), heights = c(1.75,1,1,1.29))
#' Total nb of population surveyed per year:
nuseds_surveyYear <- nuseds %>%
  group_by(Year) %>%
  summarise(count = n()) %>%
  arrange(Year)

par(mar = c(4.5,4.5,1,.5))
plot(x = nuseds_surveyYear$Year, y = nuseds_surveyYear$count, type = "l", lwd = 2,
     ylab = "Number of populations", xlab =  "Year")
legend("topleft","Total", col = "black", lwd = 2, bty = "n")

# Nb of population surveyed per year for each region:
for(rg in regions){
  # rg <- regions[1]
  i <- which(rg == regions)
  side1 <- .5
  side2 <- .5
  xlab <- ""
  xaxt <- "n"
  yaxt <- "n"
  legend_show <- F
  if(i %in% length(regions):(length(regions) - 2)){
    side1 <- 4.5
    xlab <- "Year"
    xaxt <- "s"
  }
  if(i %in% c(1,4,7)){
    side2 <- 4.5
    ylab <- "Number of populations"
    yaxt <- "s"
  }
  if(i == 4){
    legend_show <- T
  }
  cond <- nuseds$region == rg
  plot_countsYear_oneVar_fun(dataset = nuseds[cond,], varCount = "SPECIES", varX = "Year",
                             xlab = xlab, xaxt = xaxt, ylab = ylab, yaxt = yaxt,
                             colours = colours_sp, legend_show = legend_show,
                             side1 = side1, side2 = side2, side3 = .5, side4 = .5,
                             main = "", y_max = 400, legend_imposed = T)
  legend("topright",rg, col = "black", bty = "n")
}
if(figures_print){
  dev.off()
}

#
#'* Nb of populations surveyed per year species > regions *
#'

# Define the colours of the regions
colours_rg <- c("clay3",
                "tidal3",
                "soil2",
                "stone3",
                "seafoam2", # "lichen1"
                "stone1",
                "clay1",
                "tidal2",
                "soil1")
colours_rg <- SWP_cols[colours_rg]

# Or rainbow
colours_rg <- rainbow(n = length(regions))

# Or hand made 
# colours_rg <- colorRampPalette(colors = c("firebrick3","cadetblue3","aquamarine3","darkslategray4","bisque4","goldenrod3","gray40","deeppink3","darkorchid3"))
colours_rg <- c("firebrick3","goldenrod3","darkslategray4","bisque4","darkorchid3",
                "cadetblue3","gray40","aquamarine3","deeppink3")

names(colours_rg) <- regions

figures_print <- T

coef <- .9
if(figures_print){
  jpeg(paste0(wd_figures,"/Number_surveys_perYear_Species_Regions.jpeg"),
       width = 21.59 * coef, height = 14 * coef, units = 'cm', res = 300)
}
m <- matrix(1:(length(species) + 1), ncol = 3, byrow = T)
layout(m, widths =  c(1.22,1,1), heights = c(1,1.24))
for(sp in species){
  # sp <- species[1]
  i <- which(sp == species)
  side1 <- .5
  side2 <- .5
  xlab <- ""
  xaxt <- "n"
  yaxt <- "n"
  legend_show <- F
  if(i %in% 4:6){
    side1 <- 4.5
    xlab <- "Year"
    xaxt <- "s"
  }
  if(i %in% 3){
    xlab <- "Year"
    xaxt <- "s"
  }
  if(i %in% c(1,4)){
    side2 <- 4.5
    ylab <- "Number of populations"
    yaxt <- "s"
  }
  cond <- nuseds$SPECIES == sp
  plot_countsYear_oneVar_fun(dataset = nuseds[cond,], varCount = "region", varX = "Year",
                             xlab = xlab, xaxt = xaxt, ylab = ylab, yaxt = yaxt,
                             colours = colours_rg, legend_show = F,
                             side1 = side1, side2 = side2, side3 = .5, side4 = .5,
                             main = "", y_max = 350, legend_imposed = T)
  legend("topright",sp, col = "black", bty = "n")
  
}
plot.new()
mtext("Year", side = 3, line = -2.7, cex = .7)
legend("bottom",names(colours_rg), col = colours_rg, lwd = 2, bty = "n")
if(figures_print){
  dev.off()
}



nuseds$region






















