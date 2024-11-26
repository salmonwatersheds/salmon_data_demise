

#'******************************************************************************
#' The goal of the script is to 
#' 
#' Files imported:
#' - Number_Prop_populationsAssessed_total.csv            # created in 1_dataset.R
#' - Number_Prop_populationsAssessed_regions.csv          # created in 1_dataset.R
#' - Number_Prop_populationsAssessed_species.csv          # created in 1_dataset.R
#' - Number_Prop_populationsAssessed_regions_species.csv  # created in 1_dataset.R
#' 
#' Files produced: 
#' 
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

source("code/functions.R")
source("code/colours.R")

figures_print <- T

#
# Import files ------

# Counts and proportions of populations and CUs assessed across regions and species

filename <- "populationAssessed_catches_data_remove_0s_NAs"

data_total <- read_xlsx(paste0(wd_data_output,"/",filename,".xlsx"), 
                      sheet = "populations_total") |> as.data.frame()
head(data_total)

# Counts and proportions of populations and CUs assessed per regions
data_rg <- read_xlsx(paste0(wd_data_output,"/",filename,".xlsx"), 
                     sheet = "populations_regions") |> as.data.frame()

head(data_rg)

# Counts and proportions of populations and CUs assessed per species
data_sp <-  read_xlsx(paste0(wd_data_output,"/",filename,".xlsx"), 
                      sheet = "populations_species") |> as.data.frame()

head(data_sp)

# Counts and proportions of populations and CUs assessed per regions and species
data_rg_sp <-  read_xlsx(paste0(wd_data_output,"/",filename,".xlsx"), 
                         sheet = "populations_regions_species") |> as.data.frame()
head(data_rg_sp)

# Import the catch data
catch  <-  read_xlsx(paste0(wd_data_output,"/",filename,".xlsx"), 
                     sheet = "catches_species_total") |> as.data.frame()
head(catch)


# Counts and proportions of populations and CUs assessed per regions and species WITH Os

filename <- "populationAssessed_catches_data_remove_NAs"

data_rg_sp_0 <-  read_xlsx(paste0(wd_data_output,"/",filename,".xlsx"), 
                         sheet = "populations_regions_species") |> as.data.frame()

data_sp_0 <-  read_xlsx(paste0(wd_data_output,"/",filename,".xlsx"), 
                        sheet = "populations_species") |> as.data.frame()

#
# Define the order of the regions and species -------
#
# regions <- unique(data_rg$region)
species <- unique(data_sp$species)

# Retain the order of the most monitored regions at the global scale: NOT ANYMORE
# x_max <- sapply(regions, function(rg){
#   cond <- data_rg$region == rg
#   return(max(data_rg$count[cond]))
# })
# 
# regions <- regions[rev(order(x_max))]

regions <- c("Yukon","Transboundary","Haida Gwaii","Nass","Skeena","Central Coast",
             "Vancouver Island & Mainland Inlets","Fraser","Columbia")

# for the species: alphabetic order is good
# x_max <- sapply(species, function(sp){
#   cond <- data_sp$species == sp
#   return(max(data_sp$count[cond]))
# })
# species <- species[rev(order(x_max))]

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

# Bruno's palette:
# colours_rg <- paletteer_d("peRReo::planb", n = length(regions), type = "discrete", ) # Monet Panb 
colours_rg <- paletteer_d("ltc::crbhits",n = length(regions)) # SP: not in love with this one; feel free to change
names(colours_rg) <- regions


# Backgrond segments parameters
colours_seg <- "grey50"
alpha_seg <- .5
lty_seg <- 2

#
# FIGURE 1: Number populations monitored vs catches ------
#

year_min <- 1915
year_max <- max(data_total$year)

coef <- 1
if(figures_print){
  jpeg(paste0(wd_figures,"/Number_populations_monitored_catches.jpeg"),
       width = 21.59 * coef, height = 21.59 * coef * 2/3, units = 'cm', res = 300)
  # quartz(width = 7.615, height = 4, pointsize = 10)
}
layout(mat = matrix(1))
par(mar = c(4.5,4.5,3,4.5))
plot(NA, type = "l", lwd = 2, bty = "u",
     xlim = c(year_min,year_max), #ylim = c(range(data_total$count)),
     ylab = "", xlab = "Year",
     yaxs = "i", ylim = c(0, max(data_total$count)))
mtext(side = 2, line = 3,  "Number of populations monitored", col = "#1962A0")
# catches

segments_horizontal_fun(y_range = range(data_total$count), x_range = c(1900,2030), 
                        lty = lty_seg, colour = colours_seg, alpha = alpha_seg, lwd = 0.8)
segments(x0 = (year_min:year_max)[(year_min:year_max) %% 10 == 0],
         x1 = (year_min:year_max)[(year_min:year_max) %% 10 == 0],
         y0 = -100, y1 = max(data_total$count),
         lwd = 0.8, lty = lty_seg, col = colour_transparency_fun(colours_seg,alpha = alpha_seg))
lines(x = data_total$year, y = data_total$count, lwd = 2, xpd = NA, col = "#1962A0")
#
par(new = TRUE)
cond_yr <- catch$year <= max(catch$year) &  catch$year >= min(catch$year)
cond_total <- catch$species == "Total"
plot(x = catch$year[cond_yr & cond_total], 
     y =  catch$count[cond_yr & cond_total] * 10^-6, 
     xlim = c(year_min,year_max),
     ylim = c(0, 44.8), #SP: Need to set ylim so that horizontal lines match axes ticks for populations on left
     lwd = 2, col = "#9E163C",type = "l",bty = "u",yaxt = 'n', ylab = '',xlab='', yaxs = "i", xpd = NA)
axis(side = 4, at = seq(0, 40, 8))
# axis(side = 4, at = seq(0, 36, 6))

mtext(text = "Canadian catch (millions of salmon)", side = 4, cex = 1, line = 2.5, col = "#9E163C")
# legend("bottom",c("Monitoring","Fishing"), lwd = 2, bty = 'n',
#        col = c("black", "#9E163C"))
if(figures_print){
  dev.off()
}

#
# FIGURE 2: Number populations monitored per years per region > species --------
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
    cond <- data_rg_sp$region == rg & data_rg_sp$species == sp
    return(max(data_rg_sp$count[cond],na.rm = T))
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
    cond <- data_rg_sp$region == rg & data_rg_sp$species == sp
    lines(x = data_rg_sp$year[cond], y = data_rg_sp$count[cond], lwd = 1.5, # SP: too thick IMO with so many lines. hard to see
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

# FIGURE 3: trends 1986 to 2022 per regions > species ---------
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
    fit <- lm(data_rg_sp$count[cond_rg_sp & cond_yr] ~ data_rg_sp$year[cond_rg_sp & cond_yr])
    
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
# FIGURE 4: Correlation btw Number population monitored vs. catch ---------
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
  data_s <- data_sp[cond_sp,c("year","count")]
  
  # merge the two
  data <- merge(x = data_c, y = data_s, by = "year", all = T)

  # merge with data_yr_col
  data <- merge(x = data, y = data_yr_col, by = "year", all = T)
  
  # Only select data points after 1960
  cond <- data$year > year_cut
  
  x <- data$catch[cond]
  y <- data$count[cond]
  
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
# FIGURE 5: Proportion of CUs monitored (i.e. at least 1 population) --------
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
    return(max(data_rg_sp$count[cond],na.rm = T))
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
    lines(x = data_rg_sp$year[cond], y = data_rg_sp$count[cond], lwd = 1.5, 
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
lines(x = data_total$year, y = data_total$proportion, lwd = 2)
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
  lines(x = data_rg$year[cond], y = data_rg$proportion[cond], 
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
  lines(x = data_sp$year[cond], y = data_sp$proportion[cond],
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
       width = 21.59 * coef, height = 18 * coef, units = 'cm', res = 300)
}
par(mfrow = c(3,2), mar = c(4,4,2,1), oma = c(0,2,1,0))
for(rg in regions){
  cond_rg <- data_pk$region == rg
  if(any(cond_rg) & rg != "Transboundary"){
    plot(x = data_pk$year[cond_rg],y = data_pk$count[cond_rg], "l", 
         col = colours_rg[rg], xlab = "", ylab = "", xlim = c(1950, 2023))
    abline(h = pretty(data_pk$count[cond_rg]), lty = 2, col = "#00000030")
    abline(v = seq(1950, 2023, 10), lty = 2, col = "#00000030")
    points(x = data_pk$year[cond_rg],y = data_pk$count[cond_rg], 
           pch = ifelse(data_pk$year[cond_rg] %in% years_even, 19, 21), 
           col = colours_rg[rg], bg = "white")
    mtext(side = 3, line = 0.5, paste0(letters[r], ") ",rg), adj = 0)
  }
}
mtext(side = 2, outer = TRUE, "Number of populations monitored")

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
    return(max(data_rg_sp_0$count[cond],na.rm = T))
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
    lines(x = data_rg_sp_0$year[cond], y = data_rg_sp_0$count[cond], lwd = 1.5, # SP: too thick IMO with so many lines. hard to see
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
    fit <- lm(data_rg_sp_0$count[cond_rg_sp & cond_yr] ~ data_rg_sp_0$year[cond_rg_sp & cond_yr])
    
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
  data_s <- data_sp_0[cond_sp,c("year","count")]
  
  # merge the two
  data <- merge(x = data_c, y = data_s, by = "year", all = T)
  
  # merge with data_yr_col
  data <- merge(x = data, y = data_yr_col, by = "year", all = T)
  
  # Only select data points after 1960
  cond <- data$year > year_cut
  
  x <- data$catch[cond]
  y <- data$count[cond]
  
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
