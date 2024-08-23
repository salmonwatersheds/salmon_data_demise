

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
#'******************************************************************************

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
data_total <- read.csv(paste0(wd_data_output,"/Number_Prop_populationsAssessed_total.csv"),
                       header = T)

head(data_total)

# Counts and proportions of populations and CUs assessed per regions
data_rg <- read.csv(paste0(wd_data_output,"/Number_Prop_populationsAssessed_regions.csv"),
                       header = T)

head(data_rg)

# Counts and proportions of populations and CUs assessed per species
data_sp <- read.csv(paste0(wd_data_output,"/Number_Prop_populationsAssessed_species.csv"),
                    header = T)

head(data_rg)

# Counts and proportions of populations and CUs assessed per regions and species
data_rg_sp <- read.csv(paste0(wd_data_output,"/Number_Prop_populationsAssessed_regions_species.csv"),
                    header = T)

head(data_rg_sp)

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
# Define the order of the regions and species bases on number of populations assessed -------

regions <- unique(data_rg$region)
species <- unique(data_sp$species)

# Retain the order of the most monitored regions at the global scale:
x_max <- sapply(regions, function(rg){
  cond <- data_rg$region == rg
  return(max(data_rg$count[cond]))
})

regions <- regions[rev(order(x_max))]

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
# Define species and regions colours -----

colours_sp <- species_cols_light[species]

colours_rg <- paletteer_d("peRReo::planb", n = length(regions), type = "discrete", ) # Monet Panb 
names(colours_rg) <- regions

#
# FIGURE 1: Number populations monitored per years (all) (EMMA PRODUCED IT ON HER SIDE) ------

coef <- 1.5
if(figures_print){
  jpeg(paste0(wd_figures,"/Number_populations_monitored_total.jpeg"),
       width = 15 * coef, height = 10 * coef, units = 'cm', res = 300)
}
layout(mat = matrix(1))
par(mar = c(4.5,4.5,1,.5))
plot(x = data_total$year, y = data_total$count, type = "l", lwd = 2,
     ylab = "Number of populations monitored", xlab = "Years")
# points(x = data_total$year, y = data_total$count, pch = 16)
if(figures_print){
  dev.off()
}

#
# FIGURE 2: Number populations monitored per years per region > species --------
#

if(figures_print){
  jpeg(paste0(wd_figures,"/Number_populations_monitored_regions_species.jpeg"),
       width = 21.59 * 1, height = 21.59 * .8, units = 'cm', res = 300)
}
m <- matrix(c(1:length(regions)), ncol = 3, byrow = T)
layout(m, widths =  c(1.12,1,1), heights = c(1.1,1,1.27))
for(rg in regions){
  # rg <- regions[1]
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
       ylab = ylab, xlab = xlab, xaxt = xaxt, yaxt = yaxt)
  # vertical segments
  xs <- min(data_rg$year):max(range(data_rg$year))
  segments(x0 = xs[xs %% 10 == 0], x1 = xs[xs %% 10 == 0], 
           y0 = 0, y1 = y_max, 
           col = "grey70", lwd = .5)
  # plot species counts
  for(sp in species_lines){
    # sp <- species[1]
    cond <- data_rg_sp$region == rg & data_rg_sp$species == sp
    lines(x = data_rg_sp$year[cond], y = data_rg_sp$count[cond], lwd = 2, 
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
}
if(figures_print){
  dev.off()
}

#
# FIGURE 3: Number population monitored vs. catch ---------
#

cond <- colnames(data_dt) %in% 1900:2050
years <- colnames(data_dt)[cond]
year_cut <- 1960

plot_correlation <- T
show_trendline <- F
pval_show <- T

# colour gradient for years
colfunc_yr <- colorRampPalette(c("dodgerblue3","firebrick3"))
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
  cond_sp <- catch$Species == sp
  cond_ra <- catch$`Reporting Area` == "Whole country"
  data <- catch[cond_sp & cond_ra,]
  data[,years] <- as.numeric(data[,years])
  cacth_total <- data.frame(year = years,
                            catch = data[1,years] |> as.numeric())
  
  # surveys
  cond_sp <- data_sp$species == sp
  
  # merge the two
  data <- merge(x = cacth_total, y = data_sp[cond_sp,], by = "year", all = T)
  
  # merge with data_yr_col
  data <- merge(x = data, y = data_yr_col, by = "year", all = T)
  
  # ONly select data points after 1960
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
    xlab <- "Catches (number of fishes in thousands)"
  }
  if(i %in% c(1,4)){
    side2 <- 4.5
    ylab <- "Number of populations monitored"
    # yaxt <- 's'
  }
  
  par(mar = c(side1,side2,.5,.5))
  plot(x = x, y = y, pch = 16, col = data$colours_trans[cond],
       cex = 2, yaxt = yaxt,
       xlab = xlab, ylab = ylab, main = "")
  #abline(a = 0, b = 1)
  if(i == 3){
    mtext("Catches (number of fishes in thousands)",side = 1,line = 3,cex = .65)
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
  legend("topleft",paste0(letters[i],") ",sp),bty='n')
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
# FIGURE 4: Proportion of CUs monitored (i.e. at least 1 population) --------
#

coef <- 0.8
if(figures_print){
  jpeg(paste0(wd_figures,"/Proportion_CUs_monitored_total_regions_species.jpeg"),
       width = 21.59 * coef, height = 27.94 * coef, units = 'cm', res = 300)
}
m <- matrix(1:3, ncol = 1)
layout(m, heights = c(1,1,1.2))
#' Proportion of the total number of CUs with at least one population monitored:
par(mar = c(.5,4.5,1,.5))
plot(NA, las = 1, ylim = c(0,1.1), xlim = c(min(data_total$year) - 5, max(data_total$year)),
     ylab = "Proportion of CUs monitored", xlab = "", xaxt = 'n')
segments(x0 = data_total$year[data_total$year %% 10 == 0], 
         x1 = data_total$year[data_total$year %% 10 == 0], 
         y0 = 0, y1 = 1.2, col = "grey70")
lines(x = data_total$year, y = data_total$proportion_CU, lwd = 2)
legend("topleft","a)", bty = "n")
legend("topleft",c("","Total"), col = c(NA,"black"), lwd = 2, bty = "n")

# Proportion of CUs monitored per year for each region:
plot(NA, las = 1, ylim = c(0,1.1), xlim =  c(min(data_total$year) - 5, max(data_total$year)),
     ylab = "Proportion of CUs monitored", xlab = "", xaxt = 'n')
segments(x0 = data_total$year[data_total$year %% 10 == 0], 
         x1 = data_total$year[data_total$year %% 10 == 0], 
         y0 = 0, y1 = 1.2, col = "grey70")
for(rg in regions){
  cond <- data_rg$region == rg
  lines(x = data_rg$year[cond], y = data_rg$proportion_CU[cond], 
        lwd = 2, col = colours_rg[rg])
}
legend("topleft","b)", bty = "n")
legend("topleft",c("",regions), col = c(NA,colours_rg[regions]), lwd = 2, bty = "n")

# Proportion of CUs monitored per year for each species:
par(mar = c(4.5,4.5,1,.5))
plot(NA, las = 1, ylim = c(0,1.1), xlim =  c(min(data_total$year) - 5, max(data_total$year)),
     ylab = "Proportion of CUs assessed", xlab = "Year")
segments(x0 = data_total$year[data_total$year %% 10 == 0], 
         x1 = data_total$year[data_total$year %% 10 == 0], 
         y0 = 0, y1 = 1.2, col = "grey70")
for(sp in species_lines){
  cond <- data_sp$species == sp
  lines(x = data_sp$year[cond], y = data_sp$proportion_CU[cond],
        lwd = 2, col = colours_sp[sp])
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
           y0 = 0, y1 = y_max, 
           col = "grey70", lwd = .5)
  # plot species counts
  for(rg in regions){
    # rg <- regions[1]
    cond <- data_rg_sp$region == rg & data_rg_sp$species == sp
    lines(x = data_rg_sp$year[cond], y = data_rg_sp$count[cond], lwd = 2, 
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

coef <- 0.8
if(figures_print){
  jpeg(paste0(wd_figures,"/Proportion_populations_monitored_total_regions_species.jpeg"),
       width = 21.59 * coef, height = 27.94 * coef, units = 'cm', res = 300)
}
m <- matrix(1:3, ncol = 1)
layout(m, heights = c(1,1,1.2))
#' Proportion of the total number of CUs with at least one population monitored:
par(mar = c(.5,4.5,1,.5))
plot(NA, las = 1, ylim = c(0,1), xlim = range(data_total$year),
     ylab = "Proportion of populations monitored", xlab = "", xaxt = 'n')
segments(x0 = data_total$year[data_total$year %% 10 == 0], 
         x1 = data_total$year[data_total$year %% 10 == 0], 
         y0 = 0, y1 = 1.2, col = "grey70")
lines(x = data_total$year, y = data_total$proportion, lwd = 2)
legend("topleft","a)", bty = "n")
legend("topleft",c(NA,"Total"), col = c(NA,"black"), lwd = 2, bty = "n")

# Proportion of CUs monitored per year for each region:
plot(NA, las = 1, ylim = c(0,1), xlim = range(data_total$year),
     ylab = "Proportion of populations monitored", xlab = "", xaxt = 'n')
segments(x0 = data_total$year[data_total$year %% 10 == 0], 
         x1 = data_total$year[data_total$year %% 10 == 0], 
         y0 = 0, y1 = 1.2, col = "grey70")
for(rg in regions){
  cond <- data_rg$region == rg
  lines(x = data_rg$year[cond], y = data_rg$proportion[cond], 
        lwd = 2, col = colours_rg[rg])
}
legend("topleft","b)", bty = "n")
legend("topleft",c("",regions), col = c(NA,colours_rg[regions]), lwd = 2, bty = "n")

# Proportion of CUs monitored per year for each species:
par(mar = c(4.5,4.5,1,.5))
plot(NA, las = 1, ylim = c(0,1), xlim = range(data_total$year),
     ylab = "Proportion of populations assessed", xlab = "Year")
segments(x0 = data_total$year[data_total$year %% 10 == 0], 
         x1 = data_total$year[data_total$year %% 10 == 0], 
         y0 = 0, y1 = 1.2, col = "grey70")
for(sp in species_lines){
  cond <- data_sp$species == sp
  lines(x = data_sp$year[cond], y = data_sp$proportion[cond],
        lwd = 2, col = colours_sp[sp])
}
legend("topleft","c)", bty = "n")
legend("topleft",c("",species), col = c(NA,colours_sp[species]), lwd = 2, bty = "n")
if(figures_print){
  dev.off()
}

#




