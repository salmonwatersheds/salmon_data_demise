

#'******************************************************************************
#' The goal of the script is to generate the figures and extra summary statistics.
#' 
#' Files imported:
#' 
#' - nuseds_cuid_streamid_2024-11-25.csv                # the cleaned NuSEDS data avaiable at: https://zenodo.org/records/14225367
#' - region_survey.csv                                  # the region - populations (field "streamid") associations; created in 0_assign-regions.R
#' - populationAssessed_catches_data_remove_0s_NAs.xlsx # The summary files where both 0s and NAs counts were removed (results presented in the main text); created in 1_datasets.R
#' - populationAssessed_catches_data_",option_NAs.xlsx  # The summary files where only NAs counts were removed (results presented in the supporting information); created in 1_datasets.R
#' 
#' Files produced: 
#' - all the figures except FIGURE 1: spawner survey map
#' 
#'******************************************************************************

# From CJFAS: Supply figures sized for publication: 
#.   1-column width is 8.84 cm, (3.48 in)
#.   2-column is 18.2 cm with a  (7.165 in)
#.   max. height of 23.7 cm and (9.33 in)
#.   a resolution of 300 dpi

rm(list = ls())
graphics.off()

wd_figures <- paste0(getwd(),"/figures")
wd_data_input <- paste0(getwd(),"/data_input")
wd_data_output <- paste0(getwd(),"/data_output")

library(tidyr)
library(dplyr)
library(paletteer) # https://r-graph-gallery.com/color-palette-finder
library(readxl)
# library(here)
library(scales)
library(MASS)   # for glm.nb
library(car)    # for vif()

source("code/functions.R")
source("code/colours.R")

figures_print <- T

#
# Import files ------

#'* Counts and proportions of populations and CUs assessed across regions and species *
filename <- "populationAssessed_catches_data_remove_0s_NAs"

data_total <- read_xlsx(paste0(wd_data_output,"/",filename,".xlsx"), 
                      sheet = "populations_total") |> as.data.frame()
head(data_total)

#'* Counts and proportions of populations and CUs assessed per regions
data_rg <- read_xlsx(paste0(wd_data_output,"/",filename,".xlsx"), 
                     sheet = "populations_regions") |> as.data.frame()

head(data_rg)

#'* Counts and proportions of populations and CUs assessed per species
data_sp <-  read_xlsx(paste0(wd_data_output,"/",filename,".xlsx"), 
                      sheet = "populations_species") |> as.data.frame()

head(data_sp)

#'* Counts and proportions of populations and CUs assessed per regions and species
data_rg_sp <-  read_xlsx(paste0(wd_data_output,"/",filename,".xlsx"), 
                         sheet = "populations_regions_species") |> as.data.frame()
head(data_rg_sp)

# Counts and proportions of populations and CUs assessed per species and stream survey quality
data_sp_ssq <-  read_xlsx(paste0(wd_data_output,"/",filename,".xlsx"),
                         sheet = "populations_species_ssq") |> as.data.frame()

#'* Import the catch data
catch  <-  read_xlsx(paste0(wd_data_output,"/",filename,".xlsx"), 
                     sheet = "catches_species_total") |> as.data.frame()
head(catch)

#'* Counts and proportions of populations and CUs assessed per regions and species WITH Os *
filename <- "populationAssessed_catches_data_remove_NAs"

data_total_0 <- read_xlsx(paste0(wd_data_output,"/",filename,".xlsx"), 
                          sheet = "populations_total") |> as.data.frame()

data_rg_sp_0 <-  read_xlsx(paste0(wd_data_output,"/",filename,".xlsx"), 
                         sheet = "populations_regions_species") |> as.data.frame()

data_sp_0 <-  read_xlsx(paste0(wd_data_output,"/",filename,".xlsx"), 
                        sheet = "populations_species") |> as.data.frame()

data_rg_0 <- read_xlsx(paste0(wd_data_output,"/",filename,".xlsx"), 
                       sheet = "populations_regions") |> as.data.frame()

data_sp_ssq_0 <-  read_xlsx(paste0(wd_data_output,"/",filename,".xlsx"),
                          sheet = "populations_species_ssq") |> as.data.frame()

#'* Import the landing value per species per kg *
# https://www.pac.dfo-mpo.gc.ca/analyses-econom-analysis/analyses/econ-perspective-salmon-saumon-eng.html

value_sp <- read.csv(paste0(wd_data_input,"/landed-value-valeur-debarquement-eng.csv"), 
                     header = T)
head(value_sp)
colnames(value_sp)[1] <- "Year"
for(c in 2:ncol(value_sp)){
  value_sp[,c] <- as.numeric(gsub("\\$","",value_sp[,c]))
}

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

#
# Define the order of the regions and species -------
#
# regions <- unique(data_rg$region)
species <- unique(data_sp$species)

regions <- c("Yukon","Transboundary","Haida Gwaii","Nass","Skeena","Central Coast",
             "Vancouver Island & Mainland Inlets","Fraser","Columbia")

species <- species[order(species)]

#' create a different order for displaying the curves --> place the pink 1st 
#' because they fluctuate the most and hide other species' lines
species_lines <- species
species_lines <- species_lines[species_lines != "Pink"]
species_lines <- c("Pink",species_lines)

#
# Colours and other figure settings -----

# colours_sp <- species_cols_light[species] # PSE colours

colours_sp <- c(
  Chinook = "#000000",
  Chum = "#859d32",# "#1C6838",
  Coho = "#1962A0",
  Pink = "#D89CA9",
  Sockeye = "#9E163C"
) # building on intuitive palette suggested by Eric

colours_rg <- paletteer_d("ltc::crbhits",n = length(regions)) # SP: not in love with this one; feel free to change
names(colours_rg) <- regions

# Backgrond segments parameters
colours_seg <- "grey50"
alpha_seg <- .5
lty_seg <- 2

#
#
# FIGURE 1: spawner survey map: produced in 0_assign-regions.R -------
#
# NA
# 
# FIGURE 2: Number populations monitored vs catches -------
#

if(figures_print){
  jpeg(paste0(wd_figures,"/Number_populations_monitored_catches.jpeg"), 
      width = 20.32,height = 12.7,units = "cm",res = 300)
}

par(mar=c(4,4,3,4))
plot(data_total$year, data_total$count_pop, bty="n", xlab="Year", ylab="", bty = "u",
     ylim=c(0,3000),xlim=c(1915,2025), xaxt="n", yaxt="n",type="l",col="white")
axis(1, at=seq(1915,2025,10), labels=TRUE)
axis(2, at=seq(0,3000,500), labels=TRUE, col.ticks ="#1962A0", col.axis="#1962A0")
mtext("Number of populations monitored", side=2, col="#1962A0", line=2.5)

key.years=c(1934,1954,1985,2005)
lines(data_total$year, data_total$count_pop, lwd=4, col=alpha("#1962A0",0.7))
points(x = data_total[data_total$year %in% key.years,]$year,
       y = data_total[data_total$year %in% key.years,]$count_pop, 
       lwd=3, pch=1,cex=2, col="black")

par(new = TRUE)
cond_total <- catch$species == "Total"
plot(x = catch$year[cond_total], y = catch$count[cond_total]/1000000, type="l", lwd=3, col = alpha("#9E6a5A", 0.7), 
     axes = FALSE, bty = "n",xlab = "", ylab = "", ylim = c(0,45),lty = 1)
axis(4, at = seq(0,45,5), labels = seq(0,45,5), col.ticks = "#9E6a5A", 
     col.axis = "#9E6a5A")
mtext("Commercial fisheries catch (millions of fish)", side=4, col="#9E6a5A", line=2.5)

if(figures_print){
  dev.off()
}

#
# FIGURE 3: Number populations monitored per years per region > species --------
#

lwd <- .5 # for the grid segments

if(figures_print){
  jpeg(paste0(wd_figures,"/Number_populations_monitored_regions_species.jpeg"),
       width = 21.59 * 1, height = 21.59 * .8, units = 'cm', res = 300)
  # quartz(width = 7.165, height = 7, pointsize = 10)
}
m <- matrix(c(1:length(regions)), ncol = 3, byrow = T)
layout(m, widths =  c(1.12,1,1), heights = c(1.1,1,1.27))
for(rg in regions){
  # rg <- regions[3]
  i <- which(rg == regions)
  side1 <- side3 <- .5
  side2 <- 2
  xlab <- ylab <- ""
  xaxt <- "n"
  yaxt <- "s"
  if(i %in% length(regions):(length(regions) - 2)){ # bottom plots
    side1 <- 4.5
    xlab <- "Year"
    xaxt <- "s"
  }
  if(i %in% c(1,4,7)){ # left side plot
    side2 <- 4.5
    ylab <- "Number of populations monitored"
    yaxt <- "s"
    #y_max_count <- y_max_count + 1 
  }
  if(i %in% 1:3){ # top plots
    side3 <- 2
  }

  y_max <- sapply(species,function(sp){
    cond <- data_rg_sp$region == rg & data_rg_sp$species == sp
    return(max(data_rg_sp$count_pop[cond],na.rm = T))
  }) |> max()
  
  y_max <- y_max * 1.1
  
  par(mar = c(side1,side2,side3,.5))
  plot(NA,xlim = range(data_rg$year), ylim = c(0,y_max), 
       ylab = ylab, xlab = xlab, xaxt = xaxt, yaxt = yaxt, yaxs = "i")
  ## vertical segments
  xs <- min(data_rg$year):max(range(data_rg$year))
  segments(x0 = xs[xs %% 10 == 0], x1 = xs[xs %% 10 == 0],
           y0 = 0, y1 = y_max, 
           lty = lty_seg, lwd = lwd,
           col = colour_transparency_fun(colours = colours_seg, alpha = alpha_seg))
  # horizontal segments
  nb_lines <- 5
  if(rg %in% c("Haida Gwaii","Vancouver Island & Mainland Inlets","Skeena")){ # ,"Vancouver Island & Mainland Inlets"
    nb_lines <- 4
  }
  # if(rg %in% c()){ 
  #   nb_lines <- 6
  # }
  segments_horizontal_fun(y_range = c(0,y_max), 
                          x_range = c(1900,2030), 
                          lty = lty_seg, lwd = lwd, 
                          nb_lines = nb_lines,
                          colour = colours_seg, alpha = alpha_seg)

  # plot species counts
  for(sp in species_lines){
    # sp <- species[1]
    cond <- data_rg_sp$region == rg & data_rg_sp$species == sp
    lines(x = data_rg_sp$year[cond], y = data_rg_sp$count_pop[cond], lwd = 1.5, # SP: too thick IMO with so many lines. hard to see
          col = colours_sp[sp])
  }

  if(i %in% 1:3){
    axis(side = 3)
  }
  legend("topleft",paste0(letters[i],") ",rg), col = "black", 
         bty = "n", box.lwd = 0, box.col = "white")
  
  if(i == 9){
    legend("left",species, col = colours_sp, lwd = 2, 
           bty = "n", box.lwd = 0, box.col = "white")
  }
} # end rg
if(figures_print){
  dev.off()
}

# FIGURE 4: trends 1986 to 2022 per regions > species ---------
#

year_min <- 1986

# What is the linear trend from 1986 to present in number of streams monitored by
# each region and species?
trend <- data.frame(
  region = rep(regions, each = 5),
  species = rep(species, 9),
  slope = NA, 
  se = NA
)

cond_yr <- data_rg_sp$year >= 1986

for(rg in regions){
  cond_rg <- data_rg_sp$region == rg
  species_here <- unique(data_rg_sp$species[cond_rg])
  for(sp in species_here){
    cond_rg_sp <- data_rg_sp$region == rg & data_rg_sp$species == sp
    fit <- lm(data_rg_sp$count_pop[cond_rg_sp & cond_yr] ~ data_rg_sp$year[cond_rg_sp & cond_yr])
    
    cond_rg_sp_trend <- trend$region == rg & trend$species == sp
    trend$slope[cond_rg_sp_trend] <- fit$coefficients[2]
    trend$se[cond_rg_sp_trend] <- summary(fit)$coefficients[2,"Std. Error"]
  } # end sp
} # end rg

# Plot

y_rg <- as.numeric(factor(trend$region, levels = rev(regions)))
y_rg_yr <- y_rg +  as.numeric(factor(trend$species, levels = rev(species)))/6

x_min <- min(trend$slope - 1.96*trend$se, na.rm = T)
x_max <- max(trend$slope + 1.96*trend$se, na.rm = T)

coef <- .9

if(figures_print){
  jpeg(paste0(wd_figures,"/Trends_populations_monitored_regions_species.jpeg"),
       width = 21.59 * coef, height = 21.59 * coef, units = 'cm', res = 300)
}
par(mfrow = c(1,1), mar = c(4.5,9,4,1), oma = rep(0,4))
plot(x = trend$slope, y = y_rg_yr, col = colours_sp[trend$species], yaxt = "n", ylab = "",
     pch = 19, cex = 1.5, ylim = c(1,10), xlim = c(x_min,x_max), yaxs = "i",
     xlab = paste("Average annual change in number of populations monitored since",year_min))
segments(x0 = trend$slope - 1.96*trend$se, x1 = trend$slope + 1.96*trend$se, 
         y0 = y_rg_yr, y1 = y_rg_yr, col = colours_sp[trend$species], lwd = 1.2)
abline(v = 0, lty = 3)     
abline(h = 1:9)
# text(-6.7, 1:9+0.8, rev(regions), adj = 0)
regions_here <- regions
regions_here <- gsub("& ","&\n",regions_here)
axis(side = 2,labels = regions_here, at = unique(y_rg) + .5, las = 1)
legend(-7, 10.8, pch = 19, pt.cex = 1.5, col = colours_sp, legend = species, ncol = 5, xpd = NA, bty = "n")
if(figures_print){
  dev.off()
}

#
#
# FIGURE 6: Proportion of CUs monitored (i.e. at least 1 population) --------
#

alpha <- .7

lwd <- .5

y_min <- min(data_total$year) - 6

coef <- 0.8
if(figures_print){
  jpeg(paste0(wd_figures,"/Proportion_CUs_monitored_total_regions_species.jpeg"),
       width = 21.59 * coef, height = 27.94 * coef, units = 'cm', res = 300)
}
m <- matrix(1:3, ncol = 1)
layout(m, heights = c(1,1,1.2))
#' Proportion of the total number of CUs with at least one population monitored:
par(mar = c(0.5,4.5,0.5,.5))
plot(NA, las = 1, ylim = c(0,1.1), xlim = c(y_min, max(data_total$year)),
     ylab = "Proportion of CUs monitored", xlab = "", xaxt = 'n')
# vertical segments
segments(x0 = (y_min:max(data_total$year))[(y_min:max(data_total$year)) %% 10 == 0], 
         x1 = (y_min:max(data_total$year))[(y_min:max(data_total$year)) %% 10 == 0], 
         y0 = 0, y1 = 1.2, lwd = lwd, lty = lty_seg,
         col = colour_transparency_fun(colours = colours_seg, alpha = alpha_seg))
# horizontal segments
segments(x0 = 1900, x1 = 2030, 
         y0 = (0:10)[(0:10) %% 2 == 0]/10, y1 = (0:10)[(0:10) %% 2 == 0]/10, 
         lwd = lwd, lty = lty_seg,
         col = colour_transparency_fun(colours = colours_seg, alpha = alpha_seg))

#
lines(x = data_total$year, y = data_total$proportion_CU, lwd = 2)
legend("topleft","a)", bty = "n")
legend("topleft",c("","Total"), col = c(NA,"black"), lwd = 2, bty = "n")

# Proportion of CUs monitored per year for each region:
plot(NA, las = 1, ylim = c(0,1.1), xlim =  c(y_min, max(data_total$year)),
     ylab = "Proportion of CUs monitored", xlab = "", xaxt = 'n')
#
segments(x0 = (y_min:max(data_total$year))[(y_min:max(data_total$year)) %% 10 == 0], 
         x1 = (y_min:max(data_total$year))[(y_min:max(data_total$year)) %% 10 == 0], 
         y0 = 0, y1 = 1.2, lwd = lwd, lty = lty_seg,
         col = colour_transparency_fun(colours = colours_seg, alpha = alpha_seg))
# horizontal segments
segments(x0 = 1900, x1 = 2030, 
         y0 = (0:10)[(0:10) %% 2 == 0]/10, y1 = (0:10)[(0:10) %% 2 == 0]/10, 
         lwd = lwd, lty = lty_seg,
         col = colour_transparency_fun(colours = colours_seg, alpha = alpha_seg))
#
regions_here <- regions[regions != "Vancouver Island & Mainland Inlets"]
regions_here <- c("Vancouver Island & Mainland Inlets",regions_here)
for(rg in regions_here){
  cond <- data_rg$region == rg
  lines(x = data_rg$year[cond], y = data_rg$proportion_CU[cond], 
        lwd = 2, col = colour_transparency_fun(colours = colours_rg[rg], alpha = alpha))
}
legend("topleft","b)", bty = "n")
legend("topleft",c("",regions_here), col = c(NA,colours_rg[regions_here]), lwd = 2, bty = "n")

# Proportion of CUs monitored per year for each species:
par(mar = c(4.5,4.5,0.5,0.5))
plot(NA, las = 1, ylim = c(0,1.1), xlim =  c(y_min, max(data_total$year)),
     ylab = "Proportion of CUs monitored", xlab = "Year")
#
segments(x0 = (y_min:max(data_total$year))[(y_min:max(data_total$year)) %% 10 == 0], 
         x1 = (y_min:max(data_total$year))[(y_min:max(data_total$year)) %% 10 == 0], 
         y0 = 0, y1 = 1.2, lwd = lwd, lty = lty_seg,
         col = colour_transparency_fun(colours = colours_seg, alpha = alpha_seg))
# horizontal segments
segments(x0 = 1900, x1 = 2030, 
         y0 = (0:10)[(0:10) %% 2 == 0]/10, y1 = (0:10)[(0:10) %% 2 == 0]/10, 
         lwd = lwd, lty = lty_seg,
         col = colour_transparency_fun(colours = colours_seg, alpha = alpha_seg))
#
for(sp in species_lines){
  cond <- data_sp$species == sp
  lines(x = data_sp$year[cond], y = data_sp$proportion_CU[cond],
        lwd = 2, col = colour_transparency_fun(colours = colours_sp[sp], alpha = alpha))
}
legend("topleft","c)", bty = "n")
legend("topleft",c("",species), col = c(NA,colours_sp[species]), lwd = 2, 
       bty = "n")
if(figures_print){
  dev.off()
}

#
# FIGURE S1: Number populations monitored per years per species > region ------
#

if(figures_print){
  jpeg(paste0(wd_figures,"/Number_populations_monitored_species_regions.jpeg"),
       width = 21.59 * 1, height = 21.59 * .8 * 2/3, units = 'cm', res = 300)
}
m <- matrix(c(1:(length(species) + 1)), ncol = 3, byrow = T)
layout(m, widths =  c(1.12,1,1), heights = c(1.1,1.27))
for(sp in species){
  # sp <- species[1]
  i <- which(sp == species)
  side1 <- side3 <- .5
  side2 <- 2
  xlab <- ""
  xaxt <- "n"
  yaxt <- "s"
  if(i %in% (length(species)+1):(length(species) - 1)){ # bottom plots
    side1 <- 4.5
    xlab <- "Year"
    xaxt <- "s"
  }
  if(i %in% c(1,4)){ # left side plot
    side2 <- 4.5
    ylab <- "Number of populations monitored"
    yaxt <- "s"
  }
  if(i %in% 1:3){ # top plots
    side3 <- 2
  }

  y_max <- sapply(regions,function(rg){
    cond <- data_rg_sp$region == rg & data_rg_sp$species == sp
    return(max(data_rg_sp$count_pop[cond],na.rm = T))
  }) |> max()
  
  y_max <- y_max + y_max /10
  
  par(mar = c(side1,side2,side3,.5))
  plot(NA,xlim = range(data_sp$year), ylim = c(0,y_max), 
       ylab = ylab, xlab = xlab, xaxt = xaxt, yaxt = yaxt)
  # vertical segments
  xs <- min(data_sp$year):max(range(data_sp$year))
  segments(x0 = xs[xs %% 10 == 0], x1 = xs[xs %% 10 == 0], 
           y0 = 0, y1 = y_max, lwd = lwd, lty = lty_seg,
           col = colour_transparency_fun(colours = colours_seg, alpha = alpha_seg))
  # horizontal segments
  nb_lines <- 5
  if(sp %in% c("Chum","Sockeye")){ # ,"Vancouver Island & Mainland Inlets"
    nb_lines <- 4
  }
  segments_horizontal_fun(y_range = c(0,y_max), 
                          x_range = c(1900,2030), 
                          lty = lty_seg, lwd = lwd, 
                          nb_lines = nb_lines,
                          colour = colours_seg, alpha = alpha_seg)
  # plot species counts
  for(rg in regions){
    # rg <- regions[1]
    cond <- data_rg_sp$region == rg & data_rg_sp$species == sp
    lines(x = data_rg_sp$year[cond], y = data_rg_sp$count_pop[cond], lwd = 1.5, 
          col = colours_rg[rg])
  }
  
  if(i %in% 1:3){
    axis(side = 3)
  }
  legend("topleft",paste0(letters[i],") ",sp), col = "black", bty = "n")
}
par(mar=c(5,2,1.1,5))
plot(NA,xaxt='n',yaxt="n",xlab="",ylab="",bty="n",ylim=c(0,1),xlim=c(0,1))
legend("topleft","Regions:",bty="n",cex=1.2)
legend("bottomleft",regions, col = colours_rg, lwd = 2, bty = "n")
if(figures_print){
  dev.off()
}

#
# FIGURE S2: Proportion of populations monitored ------
#

alpha <- .7

y_min <- min(data_total$year) - 6

coef <- 0.8
if(figures_print){
  jpeg(paste0(wd_figures,"/Proportion_populations_monitored_total_regions_species.jpeg"),
       width = 21.59 * coef, height = 27.94 * coef, units = 'cm', res = 300)
}
m <- matrix(1:3, ncol = 1)
layout(m, heights = c(1,1,1.2))
#' Proportion of the total number of CUs with at least one population monitored:
par(mar = c(0.5,4.5,0.5,.5))
plot(NA, las = 1, ylim = c(0,1), xlim = c(y_min,max(data_total$year)),
     ylab = "Proportion of populations monitored", xlab = "", xaxt = 'n')
# vertical segments
segments(x0 = (y_min:max(data_total$year))[(y_min:max(data_total$year)) %% 10 == 0], 
         x1 = (y_min:max(data_total$year))[(y_min:max(data_total$year)) %% 10 == 0], 
         y0 = 0, y1 = 1.2, lwd = lwd, lty = lty_seg,
         col = colour_transparency_fun(colours = colours_seg, alpha = alpha_seg))
# horizontal segments
segments(x0 = 1900, x1 = 2030, 
         y0 = (0:10)[(0:10) %% 2 == 0]/10, y1 = (0:10)[(0:10) %% 2 == 0]/10, 
         lwd = lwd, lty = lty_seg,
         col = colour_transparency_fun(colours = colours_seg, alpha = alpha_seg))
#
lines(x = data_total$year, y = data_total$proportion_pop, lwd = 2)
legend("topleft","a)", bty = "n")
legend("topleft",c(NA,"Total"), col = c(NA,"black"), lwd = 2, bty = "n")

# Proportion of CUs monitored per year for each region:
plot(NA, las = 1, ylim = c(0,1), xlim = c(y_min,max(data_total$year)),
     ylab = "Proportion of populations monitored", xlab = "", xaxt = 'n')
# vertical segments
segments(x0 = (y_min:max(data_total$year))[(y_min:max(data_total$year)) %% 10 == 0], 
         x1 = (y_min:max(data_total$year))[(y_min:max(data_total$year)) %% 10 == 0], 
         y0 = 0, y1 = 1.2, lwd = lwd, lty = lty_seg,
         col = colour_transparency_fun(colours = colours_seg, alpha = alpha_seg))
# horizontal segments
segments(x0 = 1900, x1 = 2030, 
         y0 = (0:10)[(0:10) %% 2 == 0]/10, y1 = (0:10)[(0:10) %% 2 == 0]/10, 
         lwd = lwd, lty = lty_seg,
         col = colour_transparency_fun(colours = colours_seg, alpha = alpha_seg))
#
for(rg in regions){
  cond <- data_rg$region == rg
  lines(x = data_rg$year[cond], y = data_rg$proportion_pop[cond], 
        lwd = 2, col = colour_transparency_fun(colours = colours_rg[rg], alpha = alpha))
}
legend("topleft","b)", bty = "n")
legend("topleft",c("",regions), col = c(NA,colours_rg[regions]), lwd = 2, bty = "n")

# Proportion of CUs monitored per year for each species:
par(mar = c(4.5,4.5,0.5,.5))
plot(NA, las = 1, ylim = c(0,1), xlim = c(y_min,max(data_total$year)),
     ylab = "Proportion of populations monitored", xlab = "Year")
# vertical segments
segments(x0 = (y_min:max(data_total$year))[(y_min:max(data_total$year)) %% 10 == 0], 
         x1 = (y_min:max(data_total$year))[(y_min:max(data_total$year)) %% 10 == 0], 
         y0 = 0, y1 = 1.2, lwd = lwd, lty = lty_seg,
         col = colour_transparency_fun(colours = colours_seg, alpha = alpha_seg))
# horizontal segments
segments(x0 = 1900, x1 = 2030, 
         y0 = (0:10)[(0:10) %% 2 == 0]/10, y1 = (0:10)[(0:10) %% 2 == 0]/10, 
         lwd = lwd, lty = lty_seg,
         col = colour_transparency_fun(colours = colours_seg, alpha = alpha_seg))
#
for(sp in species_lines){
  cond <- data_sp$species == sp
  lines(x = data_sp$year[cond], y = data_sp$proportion_pop[cond],
        lwd = 2, col = colour_transparency_fun(colours = colours_sp[sp],alpha = alpha))
}
legend("topleft","c)", bty = "n")
legend("topleft",c("",species), col = c(NA,colours_sp[species]), lwd = 2, bty = "n")
if(figures_print){
  dev.off()
}

#

# FIGURE S3: Even/odd pink monitoring across regions: which is dominant? -----
#

data_pk <- data_rg_sp %>% filter(species == "Pink")

# To deal with odd and even Pink CUs
years <- sort(unique(data_pk$year))
years_odd <- years[years %% 2 == 1]
years_even <- years[years %% 2 == 0]

coef <- .9
if(figures_print){
  jpeg(paste0(wd_figures,"/Number_Pink_populations_regions.jpeg"),
       width = 21.59 * 1, height = 21.59 * .8, units = 'cm', res = 300)
}
par(mfrow = c(3,2), mar = c(4,4,2,1), oma = c(0,2,1,0))
layout(matrix(1:6,nrow = 3, byrow = T), widths = c(1.08,1), heights = c(1.1,1,1.3))
count <- 1
for(rg in regions){
  # rg <- regions[3]
  cond_rg <- data_pk$region == rg
  if(any(cond_rg) & rg != "Transboundary"){
    
    xaxt <- "n"
    side1 <- side3 <- .5
    side2 <- 2
    ylab <- xlab <- ""
    if(count %in% 1:2){ # top plots
      side3 <- 2
    }
    if(count %in% c(1,3,5)){ # left side plots
      side2 <- 4.5
      ylab <- "Number of populations monitored"
    }
    if(count %in% 5:6){ # bottom plots
      xaxt <- "s"
      xlab <- "Year"
      side1 <- 4.5
    }
    
    par(mar = c(side1,side2,side3,.5))
    plot(x = data_pk$year[cond_rg],y = data_pk$count_pop[cond_rg], "l", xaxt = xaxt,
         col = colours_rg[rg], xlab = xlab, ylab = ylab, xlim = c(1950, 2023))
    abline(h = pretty(data_pk$count_pop[cond_rg]), lty = 2, col = "#00000030")
    abline(v = seq(1950, 2023, 10), lty = 2, col = "#00000030")
    points(x = data_pk$year[cond_rg],y = data_pk$count_pop[cond_rg], 
           pch = ifelse(data_pk$year[cond_rg] %in% years_even, 19, 21), 
           col = colours_rg[rg], bg = "white")
    # mtext(side = 3, line = 0.5, paste0(letters[count], ") ",rg), adj = 0)
    legend("topleft",paste0(letters[count], ") ",rg), bty = "n")
    if(count %in% 1:2){
      axis(side = 3)
    }
    if(count == 6){
      legend("topright",c("even","odd"), pch = c(19,21), bg = "white", bty = 'n')
    }
    count <- count + 1
  }
}

if(figures_print){
  dev.off()
}

#
# FIGURE S4: Number populations monitored per years per region > species WITH Os --------
#
lwd <- .5 # for the grid segments

if(figures_print){
  jpeg(paste0(wd_figures,"/Number_populations_monitored_regions_species_WITH_0s.jpeg"),
       width = 21.59 * 1, height = 21.59 * .8, units = 'cm', res = 300)
  # quartz(width = 7.165, height = 7, pointsize = 10)
}
m <- matrix(c(1:length(regions)), ncol = 3, byrow = T)
layout(m, widths =  c(1.12,1,1), heights = c(1.1,1,1.27))
for(rg in regions){
  # rg <- regions[9]
  i <- which(rg == regions)
  side1 <- side3 <- .5
  side2 <- 2
  xlab <- ""
  xaxt <- "n"
  yaxt <- "s"
  if(i %in% length(regions):(length(regions) - 2)){ # bottom plots
    side1 <- 4.5
    xlab <- "Year"
    xaxt <- "s"
  }
  if(i %in% c(1,4,7)){ # left side plot
    side2 <- 4.5
    ylab <- "Number of populations monitored"
    yaxt <- "s"
    #y_max_count <- y_max_count + 1 
  }
  if(i %in% 1:3){ # top plots
    side3 <- 2
  }
  
  y_max <- sapply(species,function(sp){
    cond <- data_rg_sp_0$region == rg & data_rg_sp_0$species == sp
    return(max(data_rg_sp_0$count_pop[cond],na.rm = T))
  }) |> max()
  
  y_max <- y_max + y_max /10
  
  par(mar = c(side1,side2,side3,.5))
  plot(NA,xlim = range(data_rg$year), ylim = c(0,y_max), 
       ylab = ylab, xlab = xlab, xaxt = xaxt, yaxt = yaxt, yaxs = "i")
  ## vertical segments
  xs <- min(data_rg$year):max(range(data_rg$year))
  segments(x0 = xs[xs %% 10 == 0], x1 = xs[xs %% 10 == 0],
           y0 = 0, y1 = y_max, 
           lty = lty_seg, lwd = lwd,
           col = colour_transparency_fun(colours = colours_seg, alpha = alpha_seg))
  # horizontal segments
  nb_lines <- 5
  if(rg %in% c("Haida Gwaii","Vancouver Island & Mainland Inlets","Skeena")){ # ,"Vancouver Island & Mainland Inlets"
    nb_lines <- 4
  }
  # if(rg %in% c()){ 
  #   nb_lines <- 6
  # }
  segments_horizontal_fun(y_range = c(0,y_max), 
                          x_range = c(1900,2030), 
                          lty = lty_seg, lwd = lwd, 
                          nb_lines = nb_lines,
                          colour = colours_seg, alpha = alpha_seg)
  
  # plot species counts
  for(sp in species_lines){
    # sp <- species[1]
    cond <- data_rg_sp_0$region == rg & data_rg_sp_0$species == sp
    lines(x = data_rg_sp_0$year[cond], y = data_rg_sp_0$count_pop[cond], lwd = 1.5, # SP: too thick IMO with so many lines. hard to see
          col = colours_sp[sp])
  }
  
  if(i %in% 1:3){
    axis(side = 3)
  }
  legend("topleft",paste0(letters[i],") ",rg), col = "black", 
         bty = "n", box.lwd = 0, box.col = "white")
  
  if(i == 9){
    legend("left",species, col = colours_sp, lwd = 2, 
           bty = "n", box.lwd = 0, box.col = "white")
  }
} # end rg
if(figures_print){
  dev.off()
}

#
# FIGURE S5: trends 1986 to 2022 per regions > species WITH 0s ---------
#

year_min <- 1986

# What is the linear trend from 1986 to present in number of streams monitored by
# each region and species?
trend <- data.frame(
  region = rep(regions, each = 5),
  species = rep(species, 9),
  slope = NA, 
  se = NA
)

cond_yr <- data_rg_sp_0$year >= 1986

for(rg in regions){
  cond_rg <- data_rg_sp_0$region == rg
  species_here <- unique(data_rg_sp_0$species[cond_rg])
  for(sp in species_here){
    cond_rg_sp <- data_rg_sp_0$region == rg & data_rg_sp_0$species == sp
    fit <- lm(data_rg_sp_0$count_pop[cond_rg_sp & cond_yr] ~ data_rg_sp_0$year[cond_rg_sp & cond_yr])
    
    cond_rg_sp_trend <- trend$region == rg & trend$species == sp
    trend$slope[cond_rg_sp_trend] <- fit$coefficients[2]
    trend$se[cond_rg_sp_trend] <- summary(fit)$coefficients[2,"Std. Error"]
  } # end sp
} # end rg

# Plot

y_rg <- as.numeric(factor(trend$region, levels = rev(regions)))
y_rg_yr <- y_rg +  as.numeric(factor(trend$species, levels = rev(species)))/6

x_min <- min(trend$slope - 1.96*trend$se, na.rm = T)
x_max <- max(trend$slope + 1.96*trend$se, na.rm = T)

coef <- .9

if(figures_print){
  jpeg(paste0(wd_figures,"/Trends_populations_monitored_regions_species_WITH_0s.jpeg"),
       width = 21.59 * coef, height = 21.59 * coef, units = 'cm', res = 300)
}
par(mfrow = c(1,1), mar = c(4.5,9,4,1), oma = rep(0,4))
plot(x = trend$slope, y = y_rg_yr, col = colours_sp[trend$species], yaxt = "n", ylab = "",
     pch = 19, cex = 1.5, ylim = c(1,10), xlim = c(x_min,x_max), yaxs = "i",
     xlab = paste("Average annual change in number of populations monitored since",year_min))
segments(x0 = trend$slope - 1.96*trend$se, x1 = trend$slope + 1.96*trend$se, 
         y0 = y_rg_yr, y1 = y_rg_yr, col = colours_sp[trend$species], lwd = 1.2)
abline(v = 0, lty = 3)     
abline(h = 1:9)
# text(-6.7, 1:9+0.8, rev(regions), adj = 0)
regions_here <- regions
regions_here <- gsub("& ","&\n",regions_here)
axis(side = 2,labels = regions_here, at = unique(y_rg) + .5, las = 1)
legend(-7, 10.8, pch = 19, pt.cex = 1.5, col = colours_sp, legend = species, ncol = 5, xpd = NA, bty = "n")
if(figures_print){
  dev.off()
}

#
# FIGURE S6: Correlation btw Number population monitored vs. catch WITH 0s ---------
#

years <- min(data_total$year):max(data_total$year)
year_cut <- 1960

plot_correlation <- T
show_trendline <- F
pval_show <- T

# colour gradient for years
colfunc_yr <- colorRampPalette(c("#1962A0","#9E163C"))
data_yr_col <- data.frame(year =  year_cut:max(years),
                          colours = colfunc_yr(length(year_cut:max(years))))
data_yr_col$colours_trans <- colour_transparency_fun(data_yr_col$colours, alpha = .7)

coef <- .9
if(figures_print){
  jpeg(paste0(wd_figures,"/Correlation_Number_surveys_vs_Catches_WITH_0s.jpeg"),
       width = 21.59 * coef, height = 14 * coef, units = 'cm', res = 300)
}
layout(matrix(1:(length(species)+1), nrow = 2, byrow = T),  
       widths =  c(1.13,1,1), heights = c(1,1.15))
i <- 1
for(sp in species){
  # sp <- species[2]
  
  # Catch
  cond_sp_c <- catch$species == sp
  data_c <- catch[cond_sp_c,c("year","count")]
  colnames(data_c)[colnames(data_c) == "count"] <- "catch"
  data_c$catch <- data_c$catch / 1000
  
  # surveys
  cond_sp <- data_sp_0$species == sp
  data_s <- data_sp_0[cond_sp,c("year","count_pop")]
  
  # merge the two
  data <- merge(x = data_c, y = data_s, by = "year", all = T)
  
  # merge with data_yr_col
  data <- merge(x = data, y = data_yr_col, by = "year", all = T)
  
  # Only select data points after 1960
  cond <- data$year > year_cut
  
  x <- data$catch[cond]
  y <- data$count_pop[cond]
  
  n <- min(c(sum(!is.na(x)),sum(!is.na(y)))) # number of data points
  
  side1 <- 2
  side2 <- 2
  xlab <- ylab <- ""
  yaxt <- "s"
  if(i > 3){
    side1 <- 4.5
    xlab <- "Catches (in thousands)"
  }
  if(i %in% c(1,4)){
    side2 <- 4.5
    ylab <- "Number of populations monitored"
    # yaxt <- 's'
  }
  
  par(mar = c(side1,side2,.5,.5))
  plot(x = x, y = y, pch = 16, col = data$colours_trans[cond],
       cex = 2, yaxt = yaxt, xlab = xlab, ylab = ylab, main = "")
  #abline(a = 0, b = 1)
  if(i == 3){
    mtext("Catches (in thousands)",side = 1,line = 3,cex = .65)
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
  if(sp == "Sockeye"){
    legend("topright",paste0(letters[i],") ",sp),bty='n')
  }else{
    legend("topleft",paste0(letters[i],") ",sp),bty='n')
  }
  legend("bottomright",
         legend = bquote(rho~"="~.(round(cor_spear$estimate,2))~" "~.(pval)~" "), 
         bty = "n")
  # legend("bottomleft", legend = paste("n =",n), bty = "n") # it is the same number for all species
  
  i <- i + 1
  
  print(paste("***",sp,"***"))
  print(cor_spear)
  
}
# plot legend
par(mar = c(10,3,8,3))
plot(NA, ylim = c(0,1), xlim = range(data_yr_col$year), xaxt = 's', yaxt = 'n', 
     xlab = "Year", ylab = "", xaxs = "i", yaxs = "i")
for(yr in data_yr_col$year){
  cond <- data_yr_col$year == yr
  polygon(x = c(yr,yr + 1, yr + 1, yr),y = c(0,0,1,1),border = NA, col = data_yr_col$colours_trans[cond])
}
if(figures_print){
  dev.off()
}

#
# FIGURE S7: Patterns in zero counts for Fraser sockeye ------
#

#'* Import the cleaned NuSEDS data matched with PSF cuid and streamid *
#' This is the clean version of the New Salmon Escapement Database (NuSEDS). It 
#' must be downloaded at https://zenodo.org/records/14194639 and placed in the
#' /data_input folder.
nuseds <- read.csv(paste0(wd_data_input,"/nuseds_cuid_streamid_2025-04-15.csv"), 
                   header = T)

cond <- nuseds$region == "Northern Transboundary"
nuseds$region[cond] <- "Transboundary"

# edit the field streamid --> population_id to avoid confusion
# the field is a unique combination between a CU (cuid) and a stream location (GFE_ID)
# = a popualation
colnames(nuseds)[colnames(nuseds) == "streamid"] <- "population_id"

# select the Fraser Sockeye
nuseds_FrSE <- nuseds %>% filter(region == "Fraser", SPECIES == "Sockeye")

n_pop <- length(unique(nuseds_FrSE$population_id)) # 351 335 populations
yr_range <- range(nuseds_FrSE$Year)

pch <- 16
size <- 30

if(figures_print){
  jpeg(paste0(wd_figures,"/Fraser_Sockeye_NAs_0s.jpeg"),
       width = size * .6, height = size , units = 'cm', res = 300)
}
layout(mat = matrix(1))
par(mar = c(4, 4, 1, 1))
plot(yr_range, c(0.5, n_pop+0.5), "n", yaxs = "i", ylab = "Population", xlab = "Year", bty = "l")
polygon(x = c(1998.5, 2023, 2023, 1998.5), y = c(0.5, 0.5, 360, 360), col = grey(0.8), border = NA, xpd = NA)
abline(v = seq(1940, 2020, 10), lty = 3, col = grey(0.6))
abline(h = seq(20,330,20), lty = 3, col = grey(0.6))
for(i in 1:335){
  dat.i <- nuseds_FrSE %>% filter(population_id == unique(nuseds_FrSE$population_id)[i])
  points(dat.i$Year, rep(i, nrow(dat.i)), pch = pch, 
         col = ifelse(dat.i$MAX_ESTIMATE == 0, 2, 1), cex = 0.5, xpd = NA)
}
text(2010, 350, "Zeroes appear starting\n in 1999", xpd = NA, cex = 0.8)
legend(1940, 350, pch = pch, col = c(1,2), bty = "n",
       title = "MAX_ESTIMATE", c("Non-zero", "Zero"), xpd = NA, cex = 0.8)
if(figures_print){
  dev.off()
}

#'* check for each region and species *

# Attribute a population_id to the populations not associated with a cuid
cond_popid_NA <- is.na(nuseds$population_id)
length(unique(nuseds$CU_NAME[cond_popid_NA])) # 22
length(unique(nuseds$POP_ID[cond_popid_NA])) # 82
nrow(unique(nuseds[cond_popid_NA,c("IndexId","GFE_ID")])) # 82
val <- max(nuseds$population_id,na.rm = T) + 1
for(popid in unique(nuseds$POP_ID[cond_popid_NA])){
  # popid <- unique(nuseds$POP_ID[cond_popid_NA])[1]
  cond <- cond_popid_NA & nuseds$POP_ID == popid
  nuseds$population_id[cond] <- val
  val <- val + 1
}
sum(is.na(nuseds$population_id))  # 0


# sum(is.na(nuseds$MAX_ESTIMATE))  # 0
# cond_NA <- is.na(nuseds$MAX_ESTIMATE)
# nuseds <- nuseds[!cond_NA,]

for(rg in unique(nuseds$region)){
  # rg <- unique(nuseds$region)[1]
  cond_rg <- nuseds$region == rg
  species_here <- unique(nuseds$SPECIES[cond_rg])
  for(sp in species_here){
    # sp <- species_here[1]
    cond_rg_sp <- cond_rg & nuseds$SPECIES == sp
    
    n_pop <- length(unique(nuseds$population_id[cond_rg_sp])) #
    yr_range <- range(nuseds$Year[cond_rg_sp])
    
    layout(mat = matrix(1))
    par(mar = c(4, 4, 4, 1))
    plot(yr_range, c(0.5, n_pop+0.5), "n", yaxs = "i", ylab = "Population", xlab = "Year", bty = "l", 
         main = paste(rg,sp,sep=" - "))
    # polygon(x = c(1998.5, 2023, 2023, 1998.5), y = c(0.5, 0.5, 360, 360), col = grey(0.8), border = NA, xpd = NA)
    abline(v = seq(1940, 2020, 10), lty = 3, col = grey(0.6))
    abline(h = seq(20,330,20), lty = 3, col = grey(0.6))
    
    
    for(popid in unique(nuseds$population_id[cond_rg_sp])){
      # popid <- unique(nuseds$population_id[cond_rg_sp])[1]
      cond_popid <- cond_rg_sp & nuseds$population_id == popid
      i <- which(popid == unique(nuseds$population_id[cond_rg_sp]))
      
      col_here <- sapply(nuseds$MAX_ESTIMATE[cond_popid],function(me){
        if(is.na(me)){
          col <- "grey"
        }else if(me == 0){
          col <- "red"
        }else{
          col <- "black"
        }
      })
      
      points(x = nuseds$Year[cond_popid],y = rep(i, sum(cond_popid)), pch = pch, 
             col = col_here, cex = 1, xpd = NA)
    }
    # text(2010, 350, "Zeroes appear starting\n in 1999", xpd = NA, cex = 0.8)
    legend("topleft", pch = pch, col = c("black","red","grey"), bty = "n",
           title = "MAX_ESTIMATE", c("Non-zero", "Zero","NA"), xpd = NA, cex = 0.8)
  }
}



#
# FIGURE S NOT IN PAPER: Proportion of populations monitored WITH Os ------
#

alpha <- .7

lwd <- .5 # for the grid segments

y_min <- min(data_total$year) - 6

coef <- 0.8
if(figures_print){
  jpeg(paste0(wd_figures,"/Proportion_populations_monitored_total_regions_species_WITH_0s.jpeg"),
       width = 21.59 * coef, height = 27.94 * coef, units = 'cm', res = 300)
}
m <- matrix(1:3, ncol = 1)
layout(m, heights = c(1,1,1.2))
#' Proportion of the total number of CUs with at least one population monitored:
par(mar = c(0.5,4.5,0.5,.5))
plot(NA, las = 1, ylim = c(0,1), xlim = c(y_min,max(data_total_0$year)),
     ylab = "Proportion of populations monitored", xlab = "", xaxt = 'n')
# vertical segments
segments(x0 = (y_min:max(data_total_0$year))[(y_min:max(data_total_0$year)) %% 10 == 0], 
         x1 = (y_min:max(data_total_0$year))[(y_min:max(data_total_0$year)) %% 10 == 0], 
         y0 = 0, y1 = 1.2, lwd = lwd, lty = lty_seg,
         col = colour_transparency_fun(colours = colours_seg, alpha = alpha_seg))
# horizontal segments
segments(x0 = 1900, x1 = 2030, 
         y0 = (0:10)[(0:10) %% 2 == 0]/10, y1 = (0:10)[(0:10) %% 2 == 0]/10, 
         lwd = lwd, lty = lty_seg,
         col = colour_transparency_fun(colours = colours_seg, alpha = alpha_seg))
#
lines(x = data_total_0$year, y = data_total_0$proportion_pop, lwd = 2)
legend("topleft","a)", bty = "n")
legend("topleft",c(NA,"Total"), col = c(NA,"black"), lwd = 2, bty = "n")

# Proportion of CUs monitored per year for each region:
plot(NA, las = 1, ylim = c(0,1), xlim = c(y_min,max(data_total_0$year)),
     ylab = "Proportion of populations monitored", xlab = "", xaxt = 'n')
# vertical segments
segments(x0 = (y_min:max(data_total_0$year))[(y_min:max(data_total_0$year)) %% 10 == 0], 
         x1 = (y_min:max(data_total_0$year))[(y_min:max(data_total_0$year)) %% 10 == 0], 
         y0 = 0, y1 = 1.2, lwd = lwd, lty = lty_seg,
         col = colour_transparency_fun(colours = colours_seg, alpha = alpha_seg))
# horizontal segments
segments(x0 = 1900, x1 = 2030, 
         y0 = (0:10)[(0:10) %% 2 == 0]/10, y1 = (0:10)[(0:10) %% 2 == 0]/10, 
         lwd = lwd, lty = lty_seg,
         col = colour_transparency_fun(colours = colours_seg, alpha = alpha_seg))
#
for(rg in regions){
  cond <- data_rg_0$region == rg
  lines(x = data_rg_0$year[cond], y = data_rg_0$proportion_pop[cond], 
        lwd = 2, col = colour_transparency_fun(colours = colours_rg[rg], alpha = alpha))
}
legend("topleft","b)", bty = "n")
legend("topleft",c("",regions), col = c(NA,colours_rg[regions]), lwd = 2, bty = "n")

# Proportion of CUs monitored per year for each species:
par(mar = c(4.5,4.5,0.5,.5))
plot(NA, las = 1, ylim = c(0,1), xlim = c(y_min,max(data_total_0$year)),
     ylab = "Proportion of populations monitored", xlab = "Year")
# vertical segments
segments(x0 = (y_min:max(data_total_0$year))[(y_min:max(data_total_0$year)) %% 10 == 0], 
         x1 = (y_min:max(data_total_0$year))[(y_min:max(data_total_0$year)) %% 10 == 0], 
         y0 = 0, y1 = 1.2, lwd = lwd, lty = lty_seg,
         col = colour_transparency_fun(colours = colours_seg, alpha = alpha_seg))
# horizontal segments
segments(x0 = 1900, x1 = 2030, 
         y0 = (0:10)[(0:10) %% 2 == 0]/10, y1 = (0:10)[(0:10) %% 2 == 0]/10, 
         lwd = lwd, lty = lty_seg,
         col = colour_transparency_fun(colours = colours_seg, alpha = alpha_seg))
#
for(sp in species_lines){
  cond <- data_sp_0$species == sp
  lines(x = data_sp_0$year[cond], y = data_sp_0$proportion_pop[cond],
        lwd = 2, col = colour_transparency_fun(colours = colours_sp[sp],alpha = alpha))
}
legend("topleft","c)", bty = "n")
legend("topleft",c("",species), col = c(NA,colours_sp[species]), lwd = 2, bty = "n")
if(figures_print){
  dev.off()
}

#
# FIGURE S???: barplot ESTIMATE_CLASSIFICATION ------

# 
cond_NA <- is.na(nuseds$MAX_ESTIMATE)
cond_0 <- nuseds$MAX_ESTIMATE == 0 & !cond_NA

nuseds$stream_survey_quality |> unique()
nuseds$ENUMERATION_METHODS |> unique()
nuseds$ESTIMATE_METHOD |> unique()
nuseds$ESTIMATE_CLASSIFICATION[!cond_NA] |> unique()
table(nuseds$ESTIMATE_CLASSIFICATION[!cond_NA])
unique(nuseds[!cond_NA,c("stream_survey_quality","ESTIMATE_CLASSIFICATION")])
unique(nuseds[!cond_NA & !cond_0,c("stream_survey_quality","ESTIMATE_CLASSIFICATION")])

ec_levels <- c("TRUE ABUNDANCE (TYPE-1)",
               "TRUE ABUNDANCE (TYPE-2)",
               "RELATIVE ABUNDANCE (TYPE-3)",
               "RELATIVE ABUNDANCE (TYPE-4)",
               "RELATIVE ABUNDANCE (TYPE-5)",
               "PRESENCE-ABSENCE (TYPE-6)",
               "RELATIVE: CONSTANT MULTI-YEAR METHODS",
               "RELATIVE: VARYING MULTI-YEAR METHODS",
               "NO SURVEY THIS YEAR",
               "UNKNOWN")

years <- min(nuseds$Year):max(nuseds$Year)

# without 0s:
ec_table <- matrix(NA,ncol = length(years), nrow = length(ec_levels))
colnames(ec_table) <- years
rownames(ec_table) <- ec_levels
for(yr in years){
  # yr <- years[1]
  cond_yr <- nuseds$Year == yr
  vals_yr <- table(factor(nuseds$ESTIMATE_CLASSIFICATION[cond_yr & !cond_NA & !cond_0], 
                          levels = ec_levels))
  ec_table[,as.character(yr)] <- vals_yr
}

# With 0s:
ec_table_0 <- matrix(NA,ncol = length(years), nrow = length(ec_levels))
colnames(ec_table_0) <- years
rownames(ec_table_0) <- ec_levels
for(yr in years){
  # yr <- years[1]
  cond_yr <- nuseds$Year == yr
  vals_yr <- table(factor(nuseds$ESTIMATE_CLASSIFICATION[cond_yr & !cond_NA], 
                          levels = ec_levels))
  ec_table_0[,as.character(yr)] <- vals_yr
}

estCol <- colorRampPalette(c("forest green", "gold", "red", "black"))(n = length(ec_levels))

max_val <- max(colSums(ec_table)) * 1.15

coef <- 1
if(figures_print){
  jpeg(paste0(wd_figures,"/Barplot_EstimateClassification.jpeg"),
       width = 25 * coef, height = 15 * coef, units = 'cm', res = 300)
}
par(mar = c(4.5,4.5,5,1))
bp <- barplot(ec_table, beside = FALSE, las = 1, border = NA, xaxt = "n", col = estCol,
              ylab = "", ylim = c(0,max_val))
axis(side = 1, at = bp[seq(1, length(bp), 2)], labels = FALSE, tck = -0.01)
axis(side = 1, at = bp[seq(1,length(bp),10)], labels = years[seq(1,length(bp),10)],
     tck = -0.01, tck = -0.03)
mtext(text = "Year",side = 1, line = 2.5)
mtext(text = "Number of populations",side = 2, line = 3.5)
legend(x = -1, y = max_val * 1.2, ncol = 2, fill = estCol, legend = ec_levels, 
       bg = "white", border = NA, xpd = NA)
if(figures_print){
  dev.off()
}

if(figures_print){
  jpeg(paste0(wd_figures,"/Barplot_EstimateClassification_WITH_0s.jpeg"),
       width = 25 * coef, height = 15 * coef, units = 'cm', res = 300)
}
par(mar = c(4.5,4.5,5,1))
bp <- barplot(ec_table_0, beside = FALSE, las = 1, border = NA, xaxt = "n", col = estCol,
              ylab = "", ylim = c(0,max_val))
axis(side = 1, at = bp[seq(1, length(bp), 2)], labels = FALSE, tck = -0.01)
axis(side = 1, at = bp[seq(1,length(bp),10)], labels = years[seq(1,length(bp),10)],
     tck = -0.01, tck = -0.03)
mtext(text = "Year",side = 1, line = 2.5)
mtext(text = "Number of populations",side = 2, line = 3.5)
legend(x = -1, y = max_val * 1.2, ncol = 2, fill = estCol, legend = ec_levels, 
       bg = "white", border = NA, xpd = NA)
if(figures_print){
  dev.off()
}
#
# IS_INDICATOR vs data quality score -------
#

cond_NA <- !is.na(nuseds$MAX_ESTIMATE)

sum(is.na(nuseds$IS_INDICATOR[cond_NA]))
cond_II_Y <- !is.na(nuseds$IS_INDICATOR[cond_NA]) & nuseds$IS_INDICATOR[cond_NA] == "Y"
cond_II_N <- !is.na(nuseds$IS_INDICATOR[cond_NA]) & nuseds$IS_INDICATOR[cond_NA] == "N"
table(nuseds$stream_survey_quality[cond_NA][cond_NA][cond_II_Y])
round(table(nuseds$stream_survey_quality[cond_NA][cond_II_Y])/sum(cond_II_Y),2)
# High         Low      Medium Medium-High  Medium-Low     Unknown 
#    4         966          11           4          22        4710
# 0.00        0.24        0.00        0.00        0.01        0.75

table(nuseds$stream_survey_quality[cond_NA][cond_II_N])
round(table(nuseds$stream_survey_quality[cond_NA][cond_II_N])/sum(cond_II_N),2)
# High         Low      Medium Medium-High  Medium-Low     Unknown 
#   85       21108         274          52         599      118385 
# 0.00        0.15        0.00        0.00        0.00        0.84


years <- sort(unique(nuseds$Year[cond_NA]))

counts_II <- data.frame(year = years,
                        indicator = NA,
                        not_indicator = NA,
                        blank = NA,
                        NAN = NA)

counts_II_m <- matrix(data = NA, 
                      nrow = 4, 
                      ncol = length(years))

colnames(counts_II_m) <- years
rownames(counts_II_m) <- c("indicator","not_indicator","blank","NA")

for(yr in years){
  # yr <- 2000
  cond_yr <- nuseds$Year[cond_NA] == yr
  cond_NA_II <- is.na(nuseds$IS_INDICATOR[cond_NA][cond_yr])
  cond_II_Y <- !cond_NA_II & nuseds$IS_INDICATOR[cond_NA][cond_yr] == "Y"
  cond_II_N <- !cond_NA_II & nuseds$IS_INDICATOR[cond_NA][cond_yr] == "N"
  cond_II_b <- !cond_NA_II & nuseds$IS_INDICATOR[cond_NA][cond_yr] == ""
  
  i <- which(yr == years)
  counts_II$indicator[i] <- sum(cond_II_Y)
  counts_II$not_indicator[i] <- sum(cond_II_N)
  counts_II$blank[i] <- sum(cond_II_b)
  counts_II$NAN[i] <- sum(cond_NA_II)
  
  counts_II_m["indicator",i] <- sum(cond_II_Y)
  counts_II_m["not_indicator",i] <- sum(cond_II_N)
  counts_II_m["blank",i] <- sum(cond_II_b)
  counts_II_m["NA",i] <- sum(cond_NA_II)
}

counts_II$total <- rowSums(counts_II[,c("indicator","not_indicator","blank","NAN")])
counts_II$not_indicator_prop <- counts_II$not_indicator / counts_II$total
cond_1933 <- counts_II$year > 1933
counts_II$not_indicator_prop[!cond_1933] <- NA

par(mar = c(5,5,1,5))
estCol <- colorRampPalette(c("forest green", "gold", "red", "black"))(n = 4)
bp <- barplot(counts_II_m, col = estCol, xaxt = "n", ylim = c(0,3000))
axis(side = 1, at = bp[seq(1, length(bp), 2)], labels = FALSE, tck = -0.01)
axis(side = 1, at = bp[seq(1,length(bp),10)], labels = years[seq(1,length(bp),10)],
     tck = -0.02)
mtext(text = "Year",side = 1, line = 3)
mtext(text = "Number of populations",side = 2, line = 3)
legend("topleft", c("indicator","not_indicator","blank","NA"), 
       fill = estCol,bg = "white", bty = "n")

# Add proportion of indicator vs. non-indicator
par(new=TRUE)
# lines(x = counts_II$year, y = counts_II$ind_dividedby_nonind * 2500, lwd = 1.5)
plot(x = bp, y = counts_II$not_indicator_prop, ylim = c(0,1), type = "l",
     xaxt = "n", yaxt = "n", lwd = 2, xlab = "", ylab = "")
axis(side = 4, at = c(0,0.5,1))
mtext(text = "Proprotion of non-indicator populations",side = 4, line = 3)



#
# Figure S???: Number pop monitored per species and stream survey quality since 1998 ----

data_l <- list(data_sp_ssq,data_sp_ssq_0)
names(data_l) <- c("without_0s","with_0s") 

stream_survey_quality <- c("Low","Medium-Low","Medium","Medium-High","High")

for(nm in c("without_0s","with_0s")){
  # nm <- c("without_0s","with_0s")[1]
  
  data_here <- data_l[nm][[1]]
  
  data_here <- data_here[data_here$year >= 1998,]
  data_here <- data_here[!is.na(data_here$stream_survey_quality),]
  data_here <- data_here[data_here$stream_survey_quality != "Unknown",]
  
  years <- sort(unique(data_here$year))
  
  colfunc <- colorRampPalette(c("firebrick3", "dodgerblue3"))
  cols_ssq <- colfunc(length(stream_survey_quality))
  names(cols_ssq) <- stream_survey_quality
  
  suffix <- ""
  if(nm == "with_0s"){
    suffix <- "_WITH_0s"
  }
  
  if(figures_print){
    jpeg(paste0(wd_figures,"/Number_populations_monitored_species_rssq",suffix,".jpeg"),
         width = 20, height = 15, units = 'cm', res = 300)
  }
  m <- matrix(c(1:(length(species) + 1)), ncol = 3, byrow = T)
  layout(m, widths =  c(1.17,1,1), heights = c(1,1.23))
  count <- 1
  for(sp in species){
    # sp <- species[1]
    cond_sp <- data_here$species == sp
    
    y_max <- sapply(stream_survey_quality,function(ssq){
      cond <- data_here$stream_survey_quality == ssq
      out <- max(data_here$count_pop[cond_sp & cond])
      return(out)
    }) |> max() * 1.1
    
    xlab <- ylab <- ""
    xaxt <- "n"
    side1 <- .5
    side2 <- 2
    if(count %in% 3:6){
      xlab <- "Year"
      if(count != 3){
        side1 <- 5
      }
      xaxt <- "s"
    }
    if(count %in% c(1,4)){
      ylab <- "Number populations monitored"
      side2 <- 5
    }
    
    par(mar = c(side1,side2,1,.5))
    plot(NA, xlim = range(years), ylim = c(0,y_max), xlab = xlab, ylab = ylab, 
         xaxt = xaxt)
    legend("topleft",sp,bty = "n")
    
    nb_lines <- 5
    if(sp %in% c("Pink")){ # ,"Vancouver Island & Mainland Inlets"
      nb_lines <- 4
    }
    if(sp %in% c("Chinook")){ # ,"Vancouver Island & Mainland Inlets"
      nb_lines <- 3
    }
    segments_horizontal_fun(y_range = c(0,y_max), 
                            x_range = c(1998,2030), 
                            lty = lty_seg, lwd = lwd, 
                            nb_lines = nb_lines,
                            colour = colours_seg, alpha = alpha_seg)
    
    xs <- min(data_here$year):max(range(data_here$year))
    segments(x0 = xs[xs %% 10 == 0], x1 = xs[xs %% 10 == 0],
             y0 = 0, y1 = y_max, 
             lty = lty_seg, lwd = lwd,
             col = colour_transparency_fun(colours = colours_seg, alpha = alpha_seg))
    
    for(ssq in stream_survey_quality){
      # ssq <- stream_survey_quality[1]
      cond_ssq <- data_here$stream_survey_quality == ssq
      d <- data_here[cond_sp & cond_ssq,]
      d <- d[order(d$year),]
      lines(x = d$year, y = d$count_pop, col = cols_ssq[ssq], lwd = 1.5)
    }
    count <- count + 1
  }
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
  legend("center",legend = rev(c("Low","Medium-Low","Medium","Medium-High","High")),
         col = rev(cols_ssq), lwd = 2, bty = "n", title = "Stream survey quality")
  mtext("Year",side = 3,line = -2.5, cex = .7)
  if(figures_print){
    dev.off()
  }
}

#
# Reported Statistics ------
#

#'* Proportion of 0s ~ region and species *
# 

#' This is the clean version of the New Salmon Escapement Database (NuSEDS). It 
#' must be downloaded at https://zenodo.org/records/14194639 and placed in the
#' /data_input folder.
nuseds <- read.csv(paste0(wd_data_input,"/nuseds_cuid_streamid_2025-04-15.csv"), 
                   header = T)

nrow(nuseds) # 312539

cond <- nuseds$region == "Northern Transboundary"
nuseds$region[cond] <- "Transboundary"

# edit the field streamid --> population_id to avoid confusion
# the field is a unique combination between a CU (cuid) and a stream location (GFE_ID)
# = a popualation
colnames(nuseds)[colnames(nuseds) == "streamid"] <- "population_id"

# remove rows with cuid = NA NOT ANYMORE
# cond_remove <- is.na(nuseds$cuid)
# sum(cond_remove) # 2020 1998
# nuseds <- nuseds[!cond_remove,]

# Attribute a population_id to the populations not associated with a cuid
cond_popid_NA <- is.na(nuseds$population_id)
length(unique(nuseds$CU_NAME[cond_popid_NA])) # 22
length(unique(nuseds$POP_ID[cond_popid_NA])) # 82
nrow(unique(nuseds[cond_popid_NA,c("IndexId","GFE_ID")])) # 82
val <- max(nuseds$population_id,na.rm = T) + 1
for(popid in unique(nuseds$POP_ID[cond_popid_NA])){
  # popid <- unique(nuseds$POP_ID[cond_popid_NA])[1]
  cond <- cond_popid_NA & nuseds$POP_ID == popid
  nuseds$population_id[cond] <- val
  val <- val + 1
}
sum(is.na(nuseds$population_id))  # 0

# bring region_survey
region_survey <- read.csv("data_output/region_survey.csv")
head(region_survey)

# Merge region_survey assignment from 0_assign-regions.R that assigns PSE region
# to survey sites based on exact location (rather than CU assignment)
# nuseds <- nuseds %>% left_join(region_survey, by = "streamid")
nuseds <- merge(x = nuseds,
                y = unique(region_survey[,c("region_survey","GFE_ID")]),
                by = "GFE_ID", all.x = T)

nrow(nuseds) # 312539

cond_NA <- is.na(nuseds$MAX_ESTIMATE)
sum(cond_NA) # 156507 152096
cond_0 <- nuseds$MAX_ESTIMATE == 0 & !cond_NA
sum(cond_0)  # 3449 2901

# most recent data for Fraser Chum
cond <- nuseds$region[!cond_NA] == "Fraser" & nuseds$SPECIES[!cond_NA] == "Chum"
max(nuseds[!cond_NA,]$Year[cond]) # 2020

# same but for Harrison sockeye
cond <- nuseds$region[!cond_NA] == "Fraser" & 
  nuseds$SPECIES[!cond_NA] == "Sockeye" &
  grepl("Harrison",nuseds$cu_name_pse[!cond_NA])
show <- unique(nuseds[!cond_NA,][cond,c("region","SPECIES","cuid","cu_name_pse","POPULATION")])
show <- show[order(show$cu_name_pse),]
show

cond <- nuseds$region[!cond_NA] == "Fraser" & 
  nuseds$SPECIES[!cond_NA] == "Sockeye" &
  grepl("Harrison",nuseds$POPULATION[!cond_NA])
show <- unique(nuseds[!cond_NA,][cond,c("region","SPECIES","cuid","cu_name_pse","POPULATION")])
show <- show[order(show$cu_name_pse),]
show

cond <- nuseds$region[!cond_NA] == "Fraser" & 
  nuseds$SPECIES[!cond_NA] == "Sockeye" &
  nuseds$cu_name_pse[!cond_NA] == "Harrison River (river-type)" &
  !is.na(nuseds$cu_name_pse[!cond_NA])
max(nuseds[!cond_NA,]$Year[cond]) # 2020
nuseds[!cond_NA,][cond,c("region","SPECIES","cuid","cu_name_pse","POPULATION","Year","MAX_ESTIMATE")]



# Per regions and species:
rg_sp_0 <- matrix(NA,nrow = length(regions), ncol = length(species))
colnames(rg_sp_0) <- species
rownames(rg_sp_0) <- regions
for(rg in regions){   # rg <- regions[8]
  for(sp in species){ # sp <- species[5]
    cond_rg_sp <- nuseds$region_survey == rg & nuseds$SPECIES == sp
    if(any(cond_rg_sp)){
      out <- round(sum(cond_rg_sp & cond_0) / sum(cond_0) * 100,1)
    }else{
      out <- NA
    }
    rg_sp_0[which(rg == regions),which(sp == species)] <- out
  }
}

total_rg <- rowSums(rg_sp_0, na.rm = T)
rg_sp_0 <- cbind(rg_sp_0,total_rg)
total_sp <- colSums(rg_sp_0, na.rm = T)
total_sp <- rbind(rg_sp_0,total_sp)
total_sp

#                                    Chinook Chum Coho Pink Sockeye total_rg
# Yukon                                  0.0  0.1   NA   NA      NA      0.1
# Transboundary                          0.0   NA  0.1  0.0     0.5      0.6
# Haida Gwaii                            0.0  0.0  0.0  0.0     0.0      0.0
# Nass                                   0.0  0.0  0.0  0.0     0.0      0.0
# Skeena                                 0.0  0.0  0.0  0.0     0.0      0.0
# Central Coast                          0.0  0.2  0.1  0.2     0.0      0.5
# Vancouver Island & Mainland Inlets     2.8  4.6  4.3  4.1     3.7     19.5
# Fraser                                 6.9  1.4 11.8  2.8    56.3     79.2
# Columbia                               0.0   NA   NA   NA     0.0      0.0
# total_sp                               9.7  6.3 16.3  7.1    60.5     99.9

# PREVIOUS VALUE FROM SUBMISSION 1:
#                                    Chinook Chum Coho Pink Sockeye total_rg
# Yukon                                  0.0  0.1  0.0  0.0     0.0      0.1
# Transboundary                          0.0  0.0  0.1  0.0     0.6      0.7
# Haida Gwaii                            0.0  0.0  0.0  0.0     0.0      0.0
# Nass                                   0.0  0.0  0.0  0.0     0.0      0.0
# Skeena                                 0.0  0.0  0.0  0.0     0.0      0.0
# Central Coast                          0.0  0.2  0.1  0.1     0.0      0.4
# Vancouver Island & Mainland Inlets     2.7  3.7  3.8  3.8     3.1     17.1
# Fraser                                 7.6  1.0 12.8  3.3    57.1     81.8
# Columbia                               0.0  0.0  0.0  0.0     0.0      0.0
# total_sp                              10.3  5.0 16.8  7.2    60.8    100.1


#'* Number of populations and streams per year until 1990 *

# remove the NAs and 0s
nuseds <- nuseds[!cond_NA & !cond_0,]
nrow(nuseds) # 152583 150839

years <- 1980:1990
populations <- sapply(X = years, function(yr){
  cond_y <- nuseds$Year == yr
  out <- length(unique(nuseds$population_id[cond_y]))
  names(out) <- yr
  return(out)
})
populations
# 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 
# 2326 2340 2326 2335 2358 2682 2773 2474 2449 2461 2461

# PROEVIOUS VALUE FROM SUBMISSION 1:
# 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 
# 2308 2323 2311 2316 2341 2653 2746 2460 2437 2441 2442 

streams <- sapply(X = years, function(yr){
  cond_y <- nuseds$Year == yr
  out <- length(unique(nuseds$GFE_ID[cond_y]))
  names(out) <- yr
  return(out)
})
streams
# 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 
# 1198 1227 1214 1190 1222 1366 1384 1296 1302 1348 1313 

# PROEVIOUS VALUE FROM SUBMISSION 1:
# 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 
# 1195 1224 1211 1187 1218 1363 1381 1294 1299 1346 1310

#'* Proportion of populations monitored in the most recent decade *
#'

# Number population total
n_pop_total <- length(unique(nuseds$population_id))
n_pop_total # 6973

cond_2014_2023 <- nuseds$Year > 2013
cond_1980_1989 <- nuseds$Year > 1979 & nuseds$Year < 1990

n_pop_2014 <- length(unique(nuseds$population_id[cond_2014_2023]))
n_pop_2014 # 2602
n_pop_2014/n_pop_total * 100 # 37.32

n_pop_1980 <- length(unique(nuseds$population_id[cond_1980_1989]))
n_pop_1980 # 4793
n_pop_1980/n_pop_total * 100 # 68.74



#'*  Average loss of populations per year since 1986 *
#'

cond_min <- 1986 <= data_total$year
lm <- lm(data_total$count_pop[cond_min] ~ data_total$year[cond_min])
lm
# Coefficients:
#   (Intercept)  data_total$year[cond_min]  
#       83296.0                      -40.7 

# PROEVIOUS VALUE FROM SUBMISSION 1:
# Coefficients:
#   (Intercept)  data_total$year[cond_min]  
#      86889.25                     -42.51  

#'* Monitoring of indicator stocks *

# Filter NuSEDS to VIMI Chinook
nuseds_vimiCK <- nuseds %>% 
  filter(region == "Vancouver Island & Mainland Inlets", SPECIES == "Chinook")
length(unique(nuseds_vimiCK$population_id)) # 272 268 264
ctc_indicators %in% nuseds_vimiCK$population_id
# TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

# Indicator systems and associated population_id for the Pacific Salmon Comission's Chinook Technical Committee:
# https://www.psc.org/publications/technical-reports/technical-committee-reports/chinook/ctc-data-sets/
# = /document/TCCHINOOK-24-01-Appendix-B-Escapement-Detailed.xlsx
# Appendix B: Table 4, 5:
# And in CTC 2024: section 2.3.3.3.1.

# NWVI indicators:
# Table 5:
#- Colonial- Cayeagle, 1438
#- Tashish *Tahsis, 1542
#- Artlish, and 1541
#- Kaouk 1540

# SWVI indicators:
# Table 5:
#- Bedwell-Ursus, 2017
#- Megin,  1497
#- Moyeha 1494
# Table 4:
#- Phillips River 1948
#- Cowichan 1443
#- Nanaimo (fall) 1978
# from CTC 2024: section 2.3.3.3.1:
#- Marble (Area 27); 1440
#- Leiner, 1518
#- Burman 1504
#- Tahsis (Area 25); 1519
#- Sarita, 1463
#- Nahmint (Area 23); 1474
#- San Juan (Area 20).1452

pop_indicators <- c("Phillips River (Campbell River Area) Chinook Run 1",
                    "Nanaimo River Fall Chinook","Cowichan River Chinook",
                    "San Juan River Chinook","Sarita River Chinook",
                    "Nahmint River Chinook","Bedwell River Chinook",
                    "Moyeha River Chinook","Megin River Chinook",
                    "Burman River Chinook","Leiner River Chinook",
                    "Tahsis River Chinook","Kaouk River Chinook",
                    "Artlish River Chinook","Tahsish River Chinook",
                    "Colonial Creek Chinook","Marble River Chinook")

ctc_indicators <- sapply(pop_indicators,function(p){
  cond <- nuseds_vimiCK$POPULATION == p
  return(unique(nuseds_vimiCK$population_id[cond][1])) # there are two locations associated with Phillips River (Campbell River Area) Chinook Run 1
})
# ctc_indicators <- c(1438, 1542, 1541, 1540, 2017, 1497, 1494, 1440, 1518, 1504, 1519, 1463, 1474, 1452, 1948, 1443, 1978)

# Summarize monitoring 
summary_vimiCK <- nuseds_vimiCK %>% 
  group_by(population_id) %>%
  summarise(nYearsData = length(population_id), 
            meanSpawners = round(exp(mean(log(MAX_ESTIMATE), na.rm = TRUE)))) %>%
  arrange(-nYearsData)

# Visualize monitoring of each stream through time (points = monitored, red = indicator)
layout(mat = matrix(1))
plot(c(1926,2023), c(1,100), "n", ylab = "Population", xlab = "Year")
for(i in 1: nrow(summary_vimiCK)){
  z <- nuseds_vimiCK %>% filter(population_id == summary_vimiCK$population_id[i])
  points(x = z$Year, y = rep(i, length(z$Year)), 
         col = ifelse(summary_vimiCK$population_id[i] %in% ctc_indicators, 2, 1), 
         pch = ifelse(summary_vimiCK$population_id[i] %in% ctc_indicators, 19, 1), cex = 0.5)
}
abline(h = seq(0, 100, 10))
summary_vimiCK$ctc_indicator <- summary_vimiCK$population_id %in% ctc_indicators

nrow(summary_vimiCK)
summary_vimiCK[1:30,]

# How many indicator streams are in top 30 monitored
sum(summary_vimiCK$ctc_indicator[1:30])
# 14


# How many years of data for indicator stocks?
mean(summary_vimiCK$nYearsData[summary_vimiCK$ctc_indicator == TRUE])
# 63.58824

# numner of indicator stocks
sum(summary_vimiCK$ctc_indicator)
# 17

# How many population total?
nrow(summary_vimiCK)
# 272 268

# nb of populations not indicators
sum(!summary_vimiCK$ctc_indicator)
# 255

# Mean number of years for non indicators
mean(summary_vimiCK$nYearsData[summary_vimiCK$ctc_indicator == FALSE])
# 17.14118 17.41434

# How many non-indicators have been monitored in the past decade?
maxYr <- nuseds_vimiCK %>% filter(population_id %in% ctc_indicators == FALSE) %>%
  group_by(population_id) %>%
  summarise(max(Year))

# What percent have not been monitored in the past decade?
length(which(maxYr$`max(Year)` < 2013))/length(maxYr$`max(Year)`) * 100
# 65.88235 66.13546

#'*  Where is there a potential reporting bias? *

nuseds_YT <- nuseds %>% filter(region == "Yukon") %>%
  group_by(population_id) %>%
  summarise(maxYr = max(Year), SYSTEM_SITE = unique(SYSTEM_SITE))

max(nuseds_YT$maxYr)
# 2008

#'* ADULT_PRESENCE for NA vs. 0s MAX_ESTIMATE *
cond_NA <- is.na(nuseds$MAX_ESTIMATE)
cond_0s <- !cond_NA & nuseds$MAX_ESTIMATE == 0
table(nuseds$ADULT_PRESENCE[cond_NA])
#    NONE OBSERVED NOT INSPECTED       PRESENT       UNKNOWN 
# 38         36500         99526         17067          3376 
table(nuseds$ADULT_PRESENCE[cond_0s])
# NONE OBSERVED NOT INSPECTED       PRESENT       UNKNOWN 
#          3298            10           139             2 

table(nuseds$ADULT_PRESENCE[!cond_0s & !cond_NA])
# NONE OBSERVED NOT INSPECTED       PRESENT       UNKNOWN 
#            36            10        152534             3 



#
# FIGURE 5 OLD: Correlation btw Number population monitored vs. catch ---------
#

years <- min(data_total$year):max(data_total$year)
year_cut <- 1960

plot_correlation <- T
show_trendline <- F
pval_show <- T

# colour gradient for years
colfunc_yr <- colorRampPalette(c("#1962A0","#9E163C"))
data_yr_col <- data.frame(year =  year_cut:max(years),
                          colours = colfunc_yr(length(year_cut:max(years))))
data_yr_col$colours_trans <- colour_transparency_fun(data_yr_col$colours, alpha = .7)

coef <- .9
if(figures_print){
  jpeg(paste0(wd_figures,"/Correlation_Number_surveys_vs_Catches.jpeg"),
       width = 21.59 * coef, height = 14 * coef, units = 'cm', res = 300)
}
layout(matrix(1:(length(species)+1), nrow = 2, byrow = T),  
       widths =  c(1.13,1,1), heights = c(1,1.15))
i <- 1
for(sp in species){
  # sp <- species[2]
  
  # Catch
  cond_sp_c <- catch$species == sp
  data_c <- catch[cond_sp_c,c("year","count")]
  colnames(data_c)[colnames(data_c) == "count"] <- "catch"
  data_c$catch <- data_c$catch / 1000
  
  # surveys
  cond_sp <- data_sp$species == sp
  data_s <- data_sp[cond_sp,c("year","count_pop")]
  
  # merge the two
  data <- merge(x = data_c, y = data_s, by = "year", all = T)
  
  # merge with data_yr_col
  data <- merge(x = data, y = data_yr_col, by = "year", all = T)
  
  # Only select data points after 1960
  cond <- data$year > year_cut
  
  x <- data$catch[cond]
  y <- data$count_pop[cond]
  
  n <- min(c(sum(!is.na(x)),sum(!is.na(y)))) # number of data points
  
  side1 <- 2
  side2 <- 2
  xlab <- ylab <- ""
  yaxt <- "s"
  if(i > 3){
    side1 <- 4.5
    xlab <- "Catches (in thousands)"
  }
  if(i %in% c(1,4)){
    side2 <- 4.5
    ylab <- "Number of populations monitored"
    # yaxt <- 's'
  }
  
  par(mar = c(side1,side2,.5,.5))
  plot(x = x, y = y, pch = 16, col = data$colours_trans[cond],
       cex = 2, yaxt = yaxt, xlab = xlab, ylab = ylab, main = "")
  #abline(a = 0, b = 1)
  if(i == 3){
    mtext("Catches (in thousands)",side = 1,line = 3,cex = .65)
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
  if(sp == "Sockeye"){
    legend("topright",paste0(letters[i],") ",sp),bty='n')
  }else{
    legend("topleft",paste0(letters[i],") ",sp),bty='n')
  }
  legend("bottomright",
         legend = bquote(rho~"="~.(round(cor_spear$estimate,2))~" "~.(pval)~" "), 
         bty = "n")
  # legend("bottomleft", legend = paste("n =",n), bty = "n") # it is the same number for all species
  
  i <- i + 1
  
  print(paste("***",sp,"***"))
  print(cor_spear)
  
}
# plot legend
par(mar = c(10,3,8,3))
plot(NA, ylim = c(0,1), xlim = range(data_yr_col$year), xaxt = 's', yaxt = 'n', 
     xlab = "Year", ylab = "", xaxs = "i", yaxs = "i")
for(yr in data_yr_col$year){
  cond <- data_yr_col$year == yr
  polygon(x = c(yr,yr + 1, yr + 1, yr),y = c(0,0,1,1),border = NA, col = data_yr_col$colours_trans[cond])
}
if(figures_print){
  dev.off()
}

#