###############################################################################
#
# Figures for the paper
# Monitoring for fisheries or for fish? Declines in monitoring of salmon 
# spawners continue despite a conservation crisis
# 
###############################################################################

library(paletteer)

regions <- c("Yukon","Transboundary", "Haida Gwaii","Nass","Skeena", "Central Coast","Vancouver Island & Mainland Inlets","Fraser","Columbia") # order north to south

species <- c("Chinook", "Chum", "Coho", "Pink", "Sockeye") # order alphabetical

###############################################################################
# Set colour palettes for regions and species
###############################################################################
colours_rg <- paletteer_d("ltc::crbhits") # SP: not in love with this one; feel free to change

colours_sp <- c(
  Chinook = "#000000",
  Chum = "#1C6838",
  Coho = "#1962A0",
  Pink = "#D89CA9",
  Sockeye = "#9E163C"
) # building on intuitive palette suggested by Eric

# View colour palettes (in if loop so not plotted if script is sourced)
if(1 == 2){
  par(mfrow = c(1,1), mar = c(1,1,1,1), oma = rep(0, 4))
  plot(1:9, rep(1, 9), pch = 19, cex = 3, col = colours_rg, ylim = c(0,2.5), bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  points(1:5, rep(1.5, 5), pch = 19, cex = 3, col = colours_sp)
  points(1:5, rep(1.6, 5), pch = 19, cex = 3, col = paste0(colours_sp, 80))
  text(1:5, rep(1.8, 5), c("Chinook", "Chum", "Coho", "Pink", "Sockeye"), srt = 90, xpd = NA, adj = 0, col = colours_sp, font = 2)
  
  text(1:9, rep(0.8, 5), regions, srt = -90, xpd = NA, adj = 0, col = colours_rg, font = 2)
}

###############################################################################
# Figure Sx: Patterns in zero counts for Fraser sockeye
###############################################################################

nuseds <- read.csv("data_input/2_nuseds_cuid_streamid_2024-11-25.csv")

nuseds_FrSE <- nuseds %>% filter(region == "Fraser", SPECIES == "Sockeye")

n_pop <- length(unique(nuseds_FrSE$streamid)) # 335 populations
yr_range <- range(nuseds_FrSE$Year)
par(mar = c(4, 4, 4, 1))
plot(yr_range, c(0.5, n_pop+0.5), "n", yaxs = "i", ylab = "Population", xlab = "Year", bty = "l")
polygon(x = c(1998.5, 2023, 2023, 1998.5), y = c(0.5, 0.5, 360, 360), col = grey(0.8), border = NA, xpd = NA)
abline(v = seq(1940, 2020, 10), lty = 3, col = grey(0.6))
abline(h = seq(20,330,20), lty = 3, col = grey(0.6))
for(i in 1:335){
  dat.i <- nuseds_FrSE %>% filter(streamid == unique(nuseds_FrSE$streamid)[i])
  points(dat.i$Year, rep(i, nrow(dat.i)), pch = 19, col = ifelse(dat.i$MAX_ESTIMATE == 0, 2, 1), cex = 0.5, xpd = NA)
}
text(2010, 350, "Zeroes appear starting\n in 1999", xpd = NA, cex = 0.8)
legend(1940, 380, pch = 19, col = c(1,2), title = "MAX_ESTIMATE", c("Non-zero", "Zero"), xpd = NA, cex = 0.8)

###############################################################################
# Figure Sx: Even/odd monitoring by region
###############################################################################
colours_rg <- paletteer_d("ltc::crbhits",n = length(regions)) # SP: not in love with this one; feel free to change
names(colours_rg) <- regions

data_pk <- read.csv("data_output/Number_Prop_populationsAssessed_regions_species.csv") %>%
  filter(species == "Pink")

# To deal with odd and even Pink CUs
years <- sort(unique(data_pk$year))
years_odd <- years[years %% 2 == 1]
years_even <- years[years %% 2 == 0]


regions_pk <- c("Haida Gwaii", "Nass", "Skeena", "Central Coast", "Vancouver Island & Mainland Inlets", "Fraser")
quartz(width = 7.615, height = 7, pointsize = 10)
png(filename = "PinkMonitoring.png", width = 7.615, height = 7, units = "in", res = 300, pointsize = 10)
par(mfrow = c(3,2), mar = c(4,4,2,1), oma = c(0,2,1,0))
for(r in 1:6){
  data_pk.r <- data_pk %>% filter(region == regions_pk[r])
  plot(data_pk.r$year, data_pk.r$count, "l", col = colours_rg[regions_pk[r]], xlab = "", ylab = "", xlim = c(1950, 2023))
  abline(h = pretty(data_pk.r$count), lty = 2, col = "#00000030")
  abline(v = seq(1950, 2023, 10), lty = 2, col = "#00000030")
  points(data_pk.r$year, data_pk.r$count, pch = ifelse(data_pk.r$year %in% years_even, 19, 21), col = colours_rg[regions_pk[r]], bg = "white")
  mtext(side = 3, line = 0.5, paste0(letters[r], ") ", regions_pk[r]), adj = 0)
}
mtext(side = 2, outer = TRUE, "Number of populations monitored")
dev.off()
###############################################################################
# Figure 2: Number of populations by region/species
###############################################################################

dat <- read.csv("data_output/Number_Prop_populationsAssessed_regions_species.csv")

par(mfrow = c(3,3), mar = c(0.5,3,0,0), oma = c(4, 1, 2, 1))
# Note: Supress plotting of columbia (r = 9) since there is just one population per species
for(r in 1:8){
  dat.r <- dat[dat$region == regions[r],]
  
  plot(dat.r$year, dat.r$count, "n", xaxt = "n", las = 1, xlab = "", ylab = "", xlim = c(1930, 2023), bty = "l", ylim = c(0, 1.1*max(dat.r$count, na.rm = TRUE)))
  abline(v = seq(1930, 2025, 10), lty = 3, col = grey(0.8))
  abline(h = pretty(dat.r$count), lty = 3, col = grey(0.8))
  
  mtext(side = 3, line = -1.5, paste0(" ", letters[r], ") ", regions[r]), adj = 0, cex = 0.8)
  for(s in 1:5){
    points(dat.r$year[dat.r$species == names(colours_sp)[s]], dat.r$count[dat.r$species == species[s]], "o", cex = 0.6, col = paste0(colours_sp[s], 90), pch = 19)
  }
  if(r > 6) axis(side = 1)
  
}

plot(1,1,"n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
legend("left", pch = 19, pt.cex = 2, col = paste0(colours_sp, 90), legend = species, bty = "n", cex = 1.5, title = "Species")

###############################################################################
# Figure Sx: Trends in montoring since 1986
###############################################################################

# What is the linear trend from 1986 to present in number of streams monitored by each region and species?
trend <- data.frame(
  region = rep(regions, each = 5),
  species = rep(names(colours_sp), 9),
  slope = NA, 
  se = NA
)

for(r in 1:9){
  dat.r <- dat[dat$region == regions[r],]
  
  for(s in 1:5){
    if(species[s] %in% dat.r$species){
      fit <- lm(dat.r$count[dat.r$species == species[s] & dat.r$year >= 1986] ~ dat.r$year[dat.r$species == species[s] & dat.r$year >= 1986] )
      trend$slope[which(trend$region == regions[r] & trend$species == species[s])] <- coefficients(fit)[2]
      trend$se[which(trend$region == regions[r] & trend$species == species[s])] <- summary(fit)$coefficients[2,2]
    }
  } # end s
} # end r

#------------------------------------------------------------------------------
# Plot
#------------------------------------------------------------------------------

png("figures/trend.png", width = 6, height = 6, units = "in",res = 200)
par(mfrow = c(1,1), mar = c(4,2,4,1), oma = rep(0,4))
plot(trend$slope, as.numeric(factor(trend$region, levels = rev(regions))) + as.numeric(factor(trend$species, levels = rev(species)))/6, col = colours_sp[trend$species], pch = 19, cex = 1.5, ylim = c(1,10), yaxs = "i", xlab = "Average change per year in number of populations monitored since 1986", yaxt = "n", ylab = "")
segments(x0 = trend$slope - 1.96*trend$se, x1 = trend$slope + 1.96*trend$se, y0 = as.numeric(factor(trend$region, levels = rev(regions))) + as.numeric(factor(trend$species, levels = rev(species)))/6, y1 = as.numeric(factor(trend$region, levels = rev(regions))) + as.numeric(factor(trend$species, levels = rev(species)))/6, col = colours_sp[trend$species], lwd = 1.2)
abline(v = 0, lty = 3)     
abline(h = 1:9)
text(-6.7, 1:9+0.8, rev(regions), adj = 0)
legend(-6, 11, pch = 19, pt.cex = 1.5, col = colours_sp, legend = species, ncol = 5, xpd = NA, bty = "n")
dev.off()

###############################################################################
# Set colour palettes for regions and species
###############################################################################

