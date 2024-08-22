
#'******************************************************************************
#' The goal of the script is to 
#' 
#' Files imported:
#' - 
#' 
#' Files produced: 
#' - 
#' 
#'******************************************************************************



wd_figures <- paste0(getwd(),"/figures")
wd_data_input <- paste0(getwd(),"/data_input")
wd_data_output <- paste0(getwd(),"/data_output")

# Bruno Dropbox
wd_data_input_PSF <- "C:/Users/bcarturan/Salmon Watersheds Dropbox/Bruno Carturan/X Drive/1_PROJECTS/1_Active/Population Methods and Analysis/population-indicators/spawner-surveys/output"

# Steph Dropbox
wd_data_input_PSF <- "/Users/stephaniepeacock/Salmon\ Watersheds\ Dropbox/Stephanie\ Peacock/X\ Drive/1_PROJECTS/1_Active/Population\ Methods\ and\ Analysis/population-indicators/spawner-surveys/output"


library(tidyr)
library(dplyr)
library(paletteer) # https://r-graph-gallery.com/color-palette-finder
library(readxl)


source("code/functions.R")
source("code/colours.R")

figures_print <- F


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

# Or hand made 
# colours_rg <- colorRampPalette(colors = c("firebrick3","cadetblue3","aquamarine3","darkslategray4","bisque4","goldenrod3","gray40","deeppink3","darkorchid3"))
# colours_rg <- c("firebrick3","goldenrod3","darkslategray4","bisque4","darkorchid3",
#                 "cadetblue3","gray40","aquamarine3","deeppink3")

regions <- c("Vancouver Island & Mainland Inlets","Central Coast","Fraser",
             "Haida Gwaii","Skeena","Nass","Transboundary","Yukon","Columbia")

# Or rainbow
colours_rg <- rainbow(n = length(regions))

colours_rg <- paletteer_d("peRReo::planb", n = length(regions), type = "discrete", ) # Monet Panb 

names(colours_rg) <- regions

# Transparency
# https://gist.github.com/lopspower/03fb1cc0ac9f32ef38f4
colours_rg_05 <- gsub("FF","80",colours_rg)
colours_rg_03 <- gsub("FF","4D",colours_rg)
colours_rg_07 <- gsub("FF","B3",colours_rg)

# Colours for the species:

#
# Import datasets ------
#

#'* Import the cleaned NuSEDS data matched with PSF cuid and streamid *
#' Note: the choice was made to not use the cleaned and combined nuseds file 
#' NuSEDS_escapement_data_collated_DATE.csv because it does not contain the region.
#' Prodivinding the region requires to match ciud with is a pain and was done to 
#' produce nuseds_cuid_streamid_DATE.csv, which is the file that is used instead.
#' It contains extra fields not needed.
nuseds <- import_mostRecent_file_fun(wd = wd_data_input_PSF,
                                     pattern = "nuseds_cuid_streamid")
head(nuseds)

# Remove rows with NAs
sum(is.na(nuseds$MAX_ESTIMATE)) # 155984
nuseds <- nuseds[!is.na(nuseds$MAX_ESTIMATE),]

colToKeep <- c("region","SPECIES","cu_name_pse","cuid","POP_ID","cu_name_dfo","CU_NAME",
               "SYSTEM_SITE","GFE_ID","X_LONGT","Y_LAT",
               "Year","MAX_ESTIMATE",
               "ESTIMATE_CLASSIFICATION","ESTIMATE_METHOD","stream_survey_quality")

nuseds <- nuseds[,colToKeep]

#'* Import the definition of the different fields of these two datasets *
fields_def <- nuseds_fields_definitions_fun(wd_references = wd_data_input)


#'* Import the PSE CUs decoder *
cu_decoder <- read.csv(paste0(wd_data_input,"/conservationunits_decoder.csv"),
                       header = T)

# # Comment from Steph: The conservationunits_decoder.csv wasn't in salmon_data_demise/data_input; sourced from PSF folders:
# cu_decoder <- read.csv("/Users/stephaniepeacock/Salmon Watersheds Dropbox/Stephanie Peacock/X Drive/1_PROJECTS/1_Active/Population Methods and Analysis/population-indicators/data-input/conservationunits_decoder.csv")

#'* Import the catch data from the NPAFC Statistics *
# Metadata: 
# https://www.npafc.org/wp-content/uploads/Statistics/Statistics-Metadata-Report-June2024.pdf
catch <- read_xlsx(paste0(wd_data_input,"/NPAFC_Catch_Stat-1925-2023.xlsx")) |> as.data.frame()
colnames(catch) <- catch[1,]
catch <- catch[-1,]
head(catch)

catch$Country |> unique()
catch$`Whole Country/Province/State` |> unique()
catch$`Reporting Area` |> unique()
catch$Species |> unique()
catch$`Catch Type` |> unique()
catch$`Data Type` |> unique()  # "Number (000's)" "Round wt (MT)" = round weight of fish, which is the weight of the whole fish before processing

# Only keep Canada
cond <- catch$Country == "Canada"
catch <- catch[cond,]

# Only keep Commercial
cond <- catch$`Catch Type` == "Commercial"
catch <- catch[cond,]


# Compare "Number (000's)" vs. "Round wt (MT)"
# Nesting: region > Species (there is no "Whole country for each species but there is "Total" for each region)
unique(catch[,c("Reporting Area","Species")])

layout(matrix(1:2,nrow = 1))
cond <- colnames(catch) %in% 1900:2050
years <- colnames(catch)[cond]
for(dt in unique(catch$`Data Type`)){
  # dt <- unique(catch$`Data Type`)[1]
  cond_dt <- catch$`Data Type` == dt
  data_dt <- catch[cond_dt,]
  
  rg <- unique(data_dt$`Reporting Area`)
  rg <- rg[rg != "Whole country"]
  
  counts <- sapply(X = rg,FUN = function(r){
    # r <- rg[1]
    cond_r <- data_dt$`Reporting Area` == r
    out <- data_dt[cond_r,years] |> 
      as.matrix() |>
      as.numeric() |>
      max(na.rm = T) 
    return(out)
  })
  
  ymax <- max(counts, na.rm = T)
  
  plot(NA,xlim = range(as.numeric(years)),ylim = c(0,ymax), 
       ylab = dt, xlab = "Years", 
       main = dt)
  
  for(r in rg){
    # r <- rg[1]
    cond_r <- data_dt$`Reporting Area` == r
    cond_sp <- data_dt$Species == "Total"
    lines(x = as.numeric(years), y = data_dt[cond_r & cond_sp,years])
  }
}


# Only keep number of fish (discard weights)
cond <- catch$`Data Type` == "Number (000's)"
catch <- catch[cond,]

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


# write.csv(nuseds_surveyYear[,c("Year","count")],paste0(wd_data_output,"/Number_populationsAssessed_total.csv"),
#           row.names = F)

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
#
#
# Figures: (1) number surveys: total & region > species ; (2) Species > region ------------

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

#'* Total nb populations/yr + per region ; Wide format *
#' 

coef <- 0.8
if(figures_print){
  jpeg(paste0(wd_figures,"/Number_surveys_perYear_total_Regions_species_totalNo.jpeg"),
       width = 21.59 * coef, height = 27.94 * coef, units = 'cm', res = 300)
}
m <- matrix(c(1,1,1:(length(regions)+1)), ncol = 3, byrow = T)
layout(m, widths =  c(1.25,1,1), heights = c(1.75,1.12,1,1.32))
#' Total nb of population surveyed per year:
nuseds_surveyYear <- nuseds %>%
  group_by(Year) %>%
  summarise(count = n()) %>%
  arrange(Year)

par(mar = c(4.5,4.5,1,.5))
plot(NA, xlim = range(nuseds_surveyYear$Year), ylim = c(0,max(nuseds_surveyYear$count)),
     type = "l", lwd = 2, ylab = "Number of populations", xlab =  "")
mtext("Year",side = 1, line = 2, cex = .7)
segments(x0 = nuseds_surveyYear$Year[nuseds_surveyYear$Year %% 10 == 0], 
         x1 = nuseds_surveyYear$Year[nuseds_surveyYear$Year %% 10 == 0], 
         y0 = 0, y1 = max(nuseds_surveyYear$count), 
         col = "grey70")
lines(x = nuseds_surveyYear$Year, y = nuseds_surveyYear$count, lwd = 2)
legend("topleft","Total", col = "black", lwd = 2, bty = "n")

# Nb of population surveyed per year for each region:
y_max <- c(400,200,50)
y_max_count <- 0
for(rg in regions){
  # rg <- regions[1]
  i <- which(rg == regions)
  side1 <- side2 <- side3 <- .5
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
    y_max_count <- y_max_count + 1 
  }
  if(i %in% 1:3){
    side3 <- 2
  }
  if(i == 4){
    legend_show <- T
  }
  cond <- nuseds$region == rg
  plot_countsYear_oneVar_fun(dataset = nuseds[cond,], varCount = "SPECIES", varX = "Year",
                             xlab = xlab, xaxt = xaxt, ylab = ylab, yaxt = yaxt,
                             colours = colours_sp, legend_show = legend_show,
                             side1 = side1, side2 = side2, side3 = side3, side4 = .5,
                             main = "", y_max = y_max[y_max_count], legend_imposed = T, 
                             x_max = max(nuseds_surveyYear$Year),
                             x_min = min(nuseds_surveyYear$Year), 
                             add_linesVertical = T, lwd_vertical = .5)
  if(i %in% 1:3){
    axis(side = 3)
  }
  legend("topright",rg, col = "black", bty = "n")
}
if(figures_print){
  dev.off()
}

#'* nb populations/yr per region ; Wide format *
#' 

coef <- 0.8
if(figures_print){
  jpeg(paste0(wd_figures,"/Number_surveys_perYear_total_Regions_species.jpeg"),
       width = 21.59 * coef, height = 21.59 * coef, units = 'cm', res = 300)
}
m <- matrix(c(1:length(regions)), ncol = 3, byrow = T)
layout(m, widths =  c(1.25,1,1), heights = c(1.1,1,1.27))

# Nb of population surveyed per year for each region:
y_max <- c(400,200,50)
y_max_count <- 0
for(rg in regions){
  # rg <- regions[1]
  i <- which(rg == regions)
  side1 <- side2 <- side3 <- .5
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
    y_max_count <- y_max_count + 1 
  }
  if(i %in% 1:3){
    side3 <- 2
  }
  if(i == 9){
    legend_show <- T
  }
  cond <- nuseds$region == rg
  plot_countsYear_oneVar_fun(dataset = nuseds[cond,], varCount = "SPECIES", varX = "Year",
                             xlab = xlab, xaxt = xaxt, ylab = ylab, yaxt = yaxt,
                             colours = colours_sp, legend_show = legend_show,
                             side1 = side1, side2 = side2, side3 = side3, side4 = .5,
                             main = "", y_max = y_max[y_max_count], legend_imposed = T, 
                             x_max = max(nuseds_surveyYear$Year),
                             x_min = min(nuseds_surveyYear$Year), 
                             add_linesVertical = T, lwd_vertical = .5)
  if(i %in% 1:3){
    axis(side = 3)
  }
  legend("topright",rg, col = "black", bty = "n")
}
if(figures_print){
  dev.off()
}


#
#'* Nb of populations surveyed per year species > regions *
#'

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
                             colours = colours_rg_07, 
                             legend_show = F,
                             side1 = side1, side2 = side2, side3 = .5, side4 = .5,
                             main = "", legend_imposed = T, lwd_vertical = .75,
                             y_max = 350, x_max = max(nuseds$Year), x_min = min(nuseds$Year))
  legend("topright",sp, col = "black", bty = "n")
  
}
plot.new()
mtext("Year", side = 3, line = -2.7, cex = .7)
legend("bottom",names(colours_rg), col = colours_rg_07, lwd = 2, bty = "n")
if(figures_print){
  dev.off()
}

#
# Find cuid to POP_ID that do not have one (nuseds_filled, NOT USED) -----
#
sum(is.na(nuseds$GFE_ID)) # 0
sum(is.na(nuseds$POP_ID)) # 0
sum(is.na(nuseds$cu_name_pse)) # 0
sum(is.na(nuseds$cu_name_dfo)) # 0
sum(is.na(nuseds$CU_NAME)) # 0
sum(is.na(nuseds$region)) # 0

nuseds$cu_name_pse |> unique() |> length() # 361
nuseds$CU_NAME |> unique() |> length()     # 365

# Check if certain POP_ID without a cuid have a CU_NAME with a cuid:
cond <- is.na(nuseds$cuid)
POP_IDs <- nuseds$POP_ID[cond] |> unique()
CU_NAMEs <- nuseds$CU_NAME[cond] |> unique()
sapply(CU_NAMEs,function(cu_n){
  cond <- nuseds$CU_NAME == cu_n
  out <- nuseds$cuid[cond] |> unique()
  return(out)
})


# There are populations (POP_ID) without cuid --> try to find the cuid using the 
# distance.
sum(is.na(nuseds$cuid)) # 1011

# populations without a cuid:
cond <- is.na(nuseds$cuid)
POP_IDs_noCUID <- unique(nuseds$POP_ID[cond])
length(POP_IDs_noCUID)     # 82
sum(is.na(POP_IDs_noCUID)) # 0

sapply(POP_IDs_noCUID,function(pop){
  cond <- nuseds$POP_ID == pop
  out <- unique(nuseds$cuid[cond])
  names(out) <- pop
  return(out)
}) |> unlist()

#' Issue 1: there are two data points of a Fraser PKE in nuseds and there is no 
#' cuid for it --> to remove 
cond <- nuseds$POP_ID == 47215
cond <- nuseds$region == "Fraser" & nuseds$SPECIES == "Pink" & nuseds$Year %% 2 == 0
nuseds[cond,]

cond <- cu_decoder$cuid == 710
cond <- grepl("Fraser River",cu_decoder$cu_name_pse) & grepl("Pink",cu_decoder$species_name)
cu_decoder[cond,]

cond <- nuseds$region == "Fraser" & nuseds$SPECIES == "Pink" & nuseds$Year %% 2 == 0
nuseds <- nuseds[!cond,]

cond <- is.na(nuseds$cuid)
POP_IDs_noCUID <- unique(nuseds$POP_ID[cond])

#' For each POP_ID in POP_IDs_noCUID, look if there is in nuseds another POP_ID
#' of the same species in the same region and near by
cond <- is.na(nuseds$cuid)
colselected <- c("region","SPECIES","POP_ID","cuid","GFE_ID","X_LONGT","Y_LAT")
nuseds_popid_gfeid <- unique(nuseds[cond,colselected])
nrow(nuseds_popid_gfeid) # 81
length(POP_IDs_noCUID)   # 81

unique(nuseds_popid_gfeid$SPECIES) #  "Chum"    "Chinook" "Sockeye"
unique(nuseds_popid_gfeid$region)  #  "Fraser" "Vancouver Island & Mainland Inlets" "Haida Gwaii"  "Central Coast"
nuseds_popid_gfeid$dist <- NA
nuseds_popid_gfeid$cuidDist_other <- NA

nuseds_filled <- nuseds

for(i in 1:nrow(nuseds_popid_gfeid)){
  # i <- 2
  pop <- POP_IDs_noCUID[i]
  cond_nuseds <- nuseds$POP_ID == pop
  SPECIES <- nuseds$SPECIES[cond_nuseds] |> unique()
  region <- nuseds$region[cond_nuseds] |> unique()
  X_LONGT <- nuseds$X_LONGT[cond_nuseds] |> unique()
  Y_LAT <- nuseds$Y_LAT[cond_nuseds] |> unique()
  
  # find eventual alternative populations
  cond_nuseds_2 <- nuseds$region == region &
    nuseds$SPECIES == SPECIES &
    !is.na(nuseds$cuid)
  
  nuseds_cut <- nuseds[cond_nuseds_2,colselected] |> unique()
  # value of 0.1 ~ 10km
  nuseds_cut$dist <- distance_Euclidean_fun(x_ref = X_LONGT, y_ref = Y_LAT,
                                            x = nuseds_cut$X_LONGT,
                                            y = nuseds_cut$Y_LAT)
  
  # for each cuid return the minimum distance
  distMin_cuids <- sapply(X = unique(nuseds_cut$cuid), FUN = function(cuid){
    cond <- nuseds_cut$cuid == cuid
    valMin <- min(nuseds_cut$dist[cond])
    names(valMin) <- cuid
    return(valMin)
  })
  
  # order the cuids per distances
  distMin_cuids <- distMin_cuids[order(distMin_cuids)]
  
  cuid_selected <- names(distMin_cuids)[1]
  distMin_cuids_rejected <- distMin_cuids[names(distMin_cuids) != cuid_selected]
  if(length(distMin_cuids_rejected) > 0){
    cuidDist_rejected <- paste0(names(distMin_cuids_rejected)," (dist = ",round(distMin_cuids_rejected,2),")") |>
      paste0(collapse = " ; ")
  }else{
    cuidDist_rejected <- "none"
  }
  
  nuseds_popid_gfeid$cuid[i] <- cuid_selected
  nuseds_popid_gfeid$dist[i] <- min(distMin_cuids) |> round(3)
  nuseds_popid_gfeid$cuidDist_other[i] <- cuidDist_rejected
  
  # fill nuseds 
  nuseds_filled$cuid[cond_nuseds] <- cuid_selected
  cu_name_pse <- cu_decoder$cu_name_pse[cu_decoder$cuid == cuid_selected]
  nuseds_filled$cu_name_pse[cond_nuseds] <- cu_name_pse
}

View(nuseds_popid_gfeid)
#' TODO : There are lots of cases where the distances are very similar so need to
#'  look into other fields?


sum(is.na(nuseds_filled$cuid))
sum(is.na(nuseds_filled$cu_name_pse))

#
# Figure proportions of CUs with at least one population assessed (NOT USED) ---------

# Remove populations that do not have a 
# it was decided to not attribute cuid to the POP_ID that are missing one (May 31 2024)
# dataset <- nuseds_filled 
cond <- !is.na(nuseds$cuid)
dataset <- nuseds[cond,]

#' Keep the same order of species and regions as in the main figure (i.e., with 
#' the number of populations assessed per year)

regions <- unique(dataset$region)
species <- unique(dataset$SPECIES)

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

#'* Plot all data and the each Region > species  *

coef <- 0.8
if(figures_print){
  jpeg(paste0(wd_figures,"/Proportion_CUs_surveyed_perYear_total_Regions_species.jpeg"),
       width = 21.59 * coef, height = 27.94 * coef, units = 'cm', res = 300)
}
m <- matrix(c(1,1,1:(length(regions)+1)), ncol = 3, byrow = T)
layout(m, widths =  c(1.25,1,1), heights = c(1.75,1,1,1.29))

#' Proportion of the total number of CUs with at least one population surveyed:
cuids <- unique(dataset$cuid)
CUs_totNb <- length(cuids)
CUs_totNb # 389

years <- unique(dataset$Year)
years <- years[order(years)]

CUs_totNb_yr <- sapply(years, function(yr){
  cond <- dataset$Year == yr
  cuids_here <- dataset$cuid[cond] |> unique()
  return(length(cuids_here))
})

par(mar = c(4.5,4.5,1,.5))
plot(NA, las = 1, ylim = c(0,1), xlim = range(years),
     ylab = "Proportion of CUs assessed", xlab = "Year")
segments(x0 = years[years %% 10 == 0], x1 = years[years %% 10 == 0], 
         y0 = 0, y1 = 1.2, 
         col = "grey70")
lines(x = years, y = CUs_totNb_yr / CUs_totNb, lwd = 2)
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
    ylab <- "Proportion of CUs assessed"
    yaxt <- "s"
  }
  if(i == 4){
    legend_show <- T
  }
  par(mar = c(side1,side2,1,.5))
  plot(NA, type = "l", lwd = 2, las = 1, ylim = c(0,1.2), xlim = range(years), 
       ylab = ylab, xlab =  xlab, xaxt = xaxt, yaxt = yaxt)
  
  segments(x0 = years[years %% 10 == 0], x1 = years[years %% 10 == 0], 
           y0 = 0, y1 = 1.2, lwd = .5,
           col = "grey70")
  
  cond <- dataset$region == rg
  dataset_rg <- dataset[cond,]
  sp <- unique(dataset_rg$SPECIES)
  
  # for each species
  for(s in sp){
    # s <- sp[1]
    cond <- dataset_rg$SPECIES == s
    dataset_rg_sp <- dataset_rg[cond,]
    cuids <- unique(dataset_rg_sp$cuid)
    CUs_totNb <- length(cuids)
    years_here <- unique(dataset_rg_sp$Year)
    years_here <- years_here[order(years_here)]
    
    dataset_totNbyr <- sapply(years_here, function(yr){
      cond <- dataset_rg_sp$Year == yr
      cuids_here <- dataset_rg_sp$cuid[cond] |> unique()
      return(length(cuids_here))
    })
    
    y <- dataset_totNbyr / CUs_totNb
    
    lines(x = years_here, y = y, lwd = 2, col = colours_sp[s])
  }
  legend("topright",rg, col = "black", bty = "n")
}
if(figures_print){
  dev.off()
}


#'* Plot all data then Region then species  *

coef <- 0.8
if(figures_print){
  jpeg(paste0(wd_figures,"/Proportion_CUs_surveyed_perYear_total_Regions_species_2.jpeg"),
       width = 21.59 * coef, height = 27.94 * coef, units = 'cm', res = 300)
}
m <- matrix(1:3, ncol = 1)
layout(m, heights = c(1,1,1.2))

#' Proportion of the total number of CUs with at least one population surveyed:
cuids <- unique(dataset$cuid)
CUs_totNb <- length(cuids)
CUs_totNb # 389

years <- unique(dataset$Year)
years <- years[order(years)]

CUs_totNb_yr <- sapply(years, function(yr){
  cond <- dataset$Year == yr
  cuids_here <- dataset$cuid[cond] |> unique()
  return(length(cuids_here))
})

par(mar = c(.5,4.5,1,.5))
plot(NA, las = 1, ylim = c(0,1), xlim = range(years),
     ylab = "Proportion of CUs assessed", xlab = "", xaxt = 'n')
segments(x0 = years[years %% 10 == 0], x1 = years[years %% 10 == 0], 
         y0 = 0, y1 = 1.2, col = "grey70")
lines(x = years, y = CUs_totNb_yr / CUs_totNb, lwd = 2)
legend("topleft","Total", col = "black", lwd = 2, bty = "n")

# Proportion of CUs surveyed per year for each region:
plot(NA, las = 1, ylim = c(0,1), xlim = range(years),
     ylab = "Proportion of CUs assessed", xlab = "", xaxt = 'n')
segments(x0 = years[years %% 10 == 0], x1 = years[years %% 10 == 0], 
         y0 = 0, y1 = 1.2, col = "grey70")
for(rg in regions){
  cond <- dataset$region == rg
  dataset_rg <- dataset[cond,]
  
  CUs_totNb <- length(unique(dataset_rg$cuid))
  
  CUs_totNb_yr <- sapply(years, function(yr){
    cond <- dataset_rg$Year == yr
    cuids_here <- dataset_rg$cuid[cond] |> unique()
    return(length(cuids_here))
  })
  lines(x = years, y = CUs_totNb_yr / CUs_totNb, lwd = 2, col = colours_rg_07[rg])
}
legend("topleft",regions, col = colours_rg[regions], lwd = 2, bty = "n")

# Proportion of CUs surveyed per year for each species:
par(mar = c(4.5,4.5,1,.5))
plot(NA, las = 1, ylim = c(0,1), xlim = range(years),
     ylab = "Proportion of CUs assessed", xlab = "Year")
segments(x0 = years[years %% 10 == 0], x1 = years[years %% 10 == 0], 
         y0 = 0, y1 = 1.2, col = "grey70")
for(sp in species){
  cond <- dataset$SPECIES == sp
  dataset_sp <- dataset[cond,]
  
  CUs_totNb <- length(unique(dataset_sp$cuid))
  
  CUs_totNb_yr <- sapply(years, function(yr){
    cond <- dataset_sp$Year == yr
    cuids_here <- dataset_sp$cuid[cond] |> unique()
    return(length(cuids_here))
  })
  
  lines(x = years, y = CUs_totNb_yr / CUs_totNb, lwd = 2, col = colours_sp[sp])
}
legend("topleft",species, col = colours_sp[species], lwd = 2, bty = "n")
if(figures_print){
  dev.off()
}

#
# Figure proportions of populations assessed -------- 
#

# Remove populations that do not have a cuid
# it was decided to not attribute cuid to the POP_ID that are missing one (May 31 2024)
# dataset <- nuseds_filled 
cond <- !is.na(nuseds$cuid)
dataset <- nuseds[cond,]

#' Keep the same order of species and regions as in the main figure (i.e., with 
#' the number of populations assessed per year)

regions <- unique(dataset$region)
species <- unique(dataset$SPECIES)

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


coef <- 0.8
if(figures_print){
  jpeg(paste0(wd_figures,"/Proportion_populations_surveyed_perYear_total_Regions_species.jpeg"),
       width = 21.59 * coef, height = 27.94 * coef, units = 'cm', res = 300)
}
m <- matrix(1:3, ncol = 1)
layout(m, heights = c(1,1,1.2))

#' Proportion of the total number of populations
POP_IDs <- unique(dataset$POP_ID)
POP_ID_totNb <- length(POP_IDs)
POP_ID_totNb # 5928

years <- unique(dataset$Year)
years <- years[order(years)]

POP_ID_totNb_yr <- sapply(years, function(yr){
  cond <- dataset$Year == yr
  POP_IDs_here <- dataset$POP_ID[cond] |> unique()
  return(length(POP_IDs_here))
})

par(mar = c(.5,4.5,1,.5))
plot(NA, las = 1, ylim = c(0,1), xlim = range(years),
     ylab = "Proportion of populations assessed", xlab = "", xaxt = 'n')
segments(x0 = years[years %% 10 == 0], x1 = years[years %% 10 == 0], 
         y0 = 0, y1 = 1.2, 
         col = "grey70")
lines(x = years, y = POP_ID_totNb_yr / POP_ID_totNb, lwd = 2)
legend("topleft","Total", col = "black", lwd = 2, bty = "n")

# Nb of population surveyed per year for each region:
plot(NA, las = 1, ylim = c(0,1), xlim = range(years),
     ylab = "Proportion of populations assessed", xlab = "", xaxt = 'n')
segments(x0 = years[years %% 10 == 0], x1 = years[years %% 10 == 0], 
         y0 = 0, y1 = 1.2, col = "grey70")
for(rg in regions){
  # rg <- regions[1]
  cond <- dataset$region == rg
  dataset_rg <- dataset[cond,]

  POP_ID_totNb <- length(unique(dataset_rg$POP_ID))
  
  POP_ID_totNb_yr <- sapply(years, function(yr){
    cond <- dataset_rg$Year == yr
    POP_IDs_here <- dataset_rg$POP_ID[cond] |> unique()
    return(length(POP_IDs_here))
  })
  
  lines(x = years, y = POP_ID_totNb_yr / POP_ID_totNb, lwd = 2, col = colours_rg_07[rg])
}
legend("topleft",regions, col = colours_rg_07[regions], lwd = 2, bty = "n")

# Nb of population surveyed per year for each species:
par(mar = c(4.5,4.5,1,.5))
plot(NA, las = 1, ylim = c(0,1), xlim = range(years),
     ylab = "Proportion of populations assessed", xlab = "Year")
segments(x0 = years[years %% 10 == 0], x1 = years[years %% 10 == 0], 
         y0 = 0, y1 = 1.2, col = "grey70")
for(sp in species){
  cond <- dataset$SPECIES == sp
  dataset_sp <- dataset[cond,]
  
  POP_ID_totNb <- length(unique(dataset_sp$POP_ID))
  
  POP_ID_totNb_yr <- sapply(years, function(yr){
    cond <- dataset_sp$Year == yr
    POP_IDs_here <- dataset_sp$POP_ID[cond] |> unique()
    return(length(POP_IDs_here))
  })
  
  lines(x = years, y = POP_ID_totNb_yr / POP_ID_totNb, lwd = 2, col = colours_sp[sp])
}
legend("topleft",species, col = colours_sp[species], lwd = 2, bty = "n")
if(figures_print){
  dev.off()
}

#
# TRY ADDING TIME STEPS RELATED TO LAW CHANGES IN THE FIGURES -------
#

# EMMA'S Time line figure: 
# https://docs.google.com/presentation/d/1sDjHGAVQU6vzHVpgouG6ucd0EZEtLuGOTqDhSTWBWRA/edit?usp=sharing

# Create a table for the time lines of the different events

events <- list()
i <- 1

#'* BC16 reports *
#' https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/40872774.pdf
#' 
events[[i]] <- data.frame(event = "BC16 reports",
                     year_start = "1927 - 1932",
                     details = "",
                     effect = "positive")
i <- i + 1

#'* Start of most monitoring programs *
#' https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/40872774.pdf
#' 
events[[i]] <- data.frame(event = "Start of most monitoring programs",
                          year_start = "1950 - 1960",
                          details = "",
                          effect = "positive")
i <- i + 1

events[[i]] <- data.frame(event = "Pacific Salmon Treaty signed",
                          year_start = 1985,
                          details = "",
                          effect = "positive")
i <- i + 1

events[[i]] <- data.frame(event = "Coho collapse",
                          year_start = 1998,
                          details = "",
                          effect = "negative")    # because the fisheries closed 
i <- i + 1

events[[i]] <- data.frame(event = "Fraser sockeye & Cohen Inquiry",
                          year_start = 2009,
                          details = "",
                          effect = "negative")
i <- i + 1

#'* Paul Martin's deficit budget cut *
#' https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/40872774.pdf
#' 
events[[i]] <- data.frame(event = "Paul Martin's deficit budget cut",
                     year_start = 1995,
                     details = "",
                     effect = "negative")

i <- i + 1

#'* Wild Salmon Policy is introduced *
#' https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/40872774.pdf
#' 
events[[i]] <- data.frame(event = "Wild Salmon Policy",
                     year_start = 2005,
                     details = "",
                     effect = "positive")

i <- i + 1

#'* Change in Fisheries Act *
#' https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/40872774.pdf
#' 
events[[i]] <- data.frame(event = "Change in Fisheries Act",
                     year_start = 2012,
                     details = "",
                     effect = "negative")

i <- i + 1


#'* Number of survey per years (all) *

#' Nb of surveys per year:
nuseds_surveyYear <- nuseds %>%
  group_by(Year) %>%
  summarise(count = n()) %>%
  arrange(Year)

head(nuseds_surveyYear)
hist(nuseds_surveyYear$count)

years <- min(nuseds_surveyYear$Year):max(nuseds_surveyYear$Year)

coef <- 1.5
if(figures_print){
  jpeg(paste0(wd_figures,"/Number_surveys_perYear_events.jpeg"),
       width = 15 * coef, height = 10 * coef, units = 'cm', res = 300)
}
par(mar = c(4.5,4.5,1,.5))
plot(x = nuseds_surveyYear$Year, y = nuseds_surveyYear$count, type = "l", lwd = 2,
     ylab = "Number of populations", xlab = "Years")
segments(x0 = years[years %% 10 == 0], x1 = years[years %% 10 == 0], 
         y0 = 0, y1 = max(nuseds_surveyYear$count), col = "grey70")
for(i in 1:length(events)){
  date <- events[[i]]$year_start
  if(grepl(" - ",date)){
    yrs <- strsplit(split = " - ", x = date)[[1]] |> as.numeric()
  }else{
    yrs <- c(date - .5, date + .5)
  }
  col <- "red"
  if(events[[i]]$effect == "positive"){
    col <- 'blue'
  }
  polygon(x = c(yrs,rev(yrs)),
          y = c(0,0,max(nuseds_surveyYear$count),max(nuseds_surveyYear$count)), 
          border = NA, col = colour_transparency_fun(col, alpha = .2))
}
#points(x = nuseds_surveyYear$Year, y = nuseds_surveyYear$count, pch = 16)
if(figures_print){
  dev.off()
}




















# Catch vs. number of surveys per species --------

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

# Order species by number of population surveyed per species.
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

# Find the maximum nb of fish caught
c_max <- sapply(species, function(sp){
  cond_sp <- catch$Species == sp
  cond_ra <- catch$`Reporting Area` == "Whole country"
  data <- catch[cond_sp & cond_ra,]
  data[,years] <- as.numeric(data[,years])
  return(max(data[1,years]))
})

cond <- colnames(data_dt) %in% 1900:2050
years <- colnames(data_dt)[cond]
year_cut <- 1960

figures_print <- F

plot_correlation <- T
show_trendline <- F
pval_show <- T

coef <- .9
if(figures_print){
  jpeg(paste0(wd_figures,"/Correlation_Number_surveys_vs_Catches.jpeg"),
       width = 21.59 * coef, height = 14 * coef, units = 'cm', res = 300)
}
layout(matrix(1:(length(species)+1), nrow = 2, byrow = T),  
       widths =  c(1.18,1,1), heights = c(1,1.15))
i <- 1
for(sp in species){
  # sp <- species[2]
  
  # Catch
  cond_sp <- catch$Species == sp
  cond_ra <- catch$`Reporting Area` == "Whole country"
  data <- catch[cond_sp & cond_ra,]
  data[,years] <- as.numeric(data[,years])
  cacth_total <- data.frame(Year = years,
                            catch = data[1,years] |> as.numeric())
  
  # Surveys
  cond_sp <- nuseds$SPECIES == sp
  survey_total <- nuseds[cond_sp,] %>%
    group_by(Year) %>%
    summarise(count = n()) %>%
    arrange(Year) %>%
    as.data.frame()
  
  colnames(survey_total)[2] <- "surveys"
  
  # merge the two
  data <- merge(x = cacth_total, y = survey_total, by = "Year", all = T)
  
  # ONly select data points after 1960
  cond <- data$Year > year_cut
  c_max <- max(data$catch[cond])
  
  if(plot_correlation){
    # plot
    
    x <- data$catch[cond]
    y <- data$surveys[cond]
    y_max <- max(x_max)
    if(sp %in% c("Sockeye","Chinook")){
      y_max <- y_max * 2/3
    }
    
    n <- min(c(sum(!is.na(x)),sum(!is.na(y)))) # number of data points
    
    side1 <- 2
    side2 <- .5
    xlab <- ylab <- ""
    yaxt <- "n"
    if(i > 3){
      side1 <- 4.5
      xlab <- "Catches (number of fishes in thousands)"
    }
    if(i %in% c(1,4)){
      side2 <- 4.5
      ylab <- "Number of populations"
      yaxt <- 's'
    }
    
    par(mar = c(side1,side2,.5,.5))
    plot(x = x, y = y, pch = 16, col = colour_transparency_fun("grey30", alpha = .2),
         cex = 2, yaxt = yaxt,
         xlim = c(0,c_max), ylim = c(0,y_max),
         xlab = xlab, ylab = ylab, main = "")
    #abline(a = 0, b = 1)
    if(i == 3){
      mtext("Catches (number of fishes in thousands)",side = 1,line = 3,cex = .7)
    }
    
    # regression line
    # m <- loess(surveys ~ catch, data = data[cond,], span = .5)
    if(show_trendline){
      m <- lm(surveys ~ catch, data = data[cond,])
      y_m <- predict(m, newdata = data[cond,])
      lines(x = x, y = y_m, lwd = 2)
    }
    cor_spear <- cor.test(x = x, y = y, method = "spearman")
    pval <- ""
    if(pval_show){
      if(cor_spear$p.value < 0.001){
        pval <- "***"
      }else if(cor_spear$p.value < 0.01){
        pval <- "**"
      }else if(cor_spear$p.value < 0.05){
        pval <- "*"
      }
    }
    legend("topleft",sp,bty='n')
    legend("bottomright",
           legend = bquote(rho~"="~.(round(cor_spear$estimate,2))~" "~.(pval)~" "), 
           bty = "n")
    # legend("bottomleft", legend = paste("n =",n), bty = "n") # it is the same number for all species
    
  }else{
    plot(x = data$Year, y = data$surveys, pch = 16, col = "gray", lwd = 2, 
         xlab = "Years", ylab = "Number of populations", main = sp, type = "l")
    par(new=TRUE)
    plot(x = data$Year, y = data$catch, pch = 16, col = "black", lwd = 2,
         xlab = "Years", ylab = "", 
         main = "", type = "l", yaxt = 'n')
    axis(side = 4)
    mtext("Catches (number of fishes in thousands)",side = 4, cex = .8, line = 2)
  }
  i <- i + 1
  
  print(paste("***",sp,"***"))
  print(cor_spear)
  
}
if(figures_print){
  dev.off()
}


