



wd_data_input <- paste0(getwd(),"/data_input")
wd_data_input_PSF <- "C:/Users/bcarturan/Salmon Watersheds Dropbox/Bruno Carturan/X Drive/1_PROJECTS/1_Active/Population Methods and Analysis/population-indicators/spawner-surveys/output"


library(tidyr)
library(dplyr)

source("code/functions.R")


nuseds <- read.csv(paste0(wd_data_input,"/NuSEDS_escapement_data_collated_20240419.csv"),
                   header = T)

colToKeep <- c("POP_ID","SPECIES","CU_NAME","SYSTEM_SITE","GFE_ID","Year",
               "ESTIMATE_CLASSIFICATION","ESTIMATE_METHOD")

nuseds <- read.csv(paste0(wd_data_input_PSF,"/nuseds_cuid_streamid_20240419.csv"),
                   header = T)

head(nuseds)

colToKeep <- c("region","SPECIES","cu_name_pse","cuid","SYSTEM_SITE","GFE_ID","Year",
               "ESTIMATE_CLASSIFICATION","ESTIMATE_METHOD","stream_survey_quality")

#' * Import the definition of the different fields of these two datasets *
fields_def <- nuseds_fields_definitions_fun(wd_references = wd_data_input)


nuseds <- nuseds[,colToKeep]

#' Nb of surveys per year:
nuseds_surveyYear <- nuseds %>%
  group_by(Year) %>%
  summarise(count = n()) %>%
  arrange(Year)

head(nuseds_surveyYear)
hist(nuseds_surveyYear$count)

par(mar = c(4.5,4.5,1,.5))
plot(x = nuseds_surveyYear$Year, y = nuseds_surveyYear$count, type = "l", lwd = 2,
     ylab = "Number of surveys", xlab = "Years")
points(x = nuseds_surveyYear$Year, y = nuseds_surveyYear$count, pch = 16)

#' COMMENT: can we find why it went up in ~1950 and down in ~2015?
#' 

regions <- unique(nuseds$region)

x_max <- sapply(regions, function(reg){
  cond <- nuseds$region == reg
  nuseds_cut <- nuseds[cond,]
  nuseds_cut_surveyYear <- nuseds_cut %>%
    group_by(Year) %>%
    summarise(count = n()) %>%
    arrange(Year)
  out <- max(nuseds_cut_surveyYear$count)
})

#' The declined is observed in each region where assessment has been important:
#' 
par(mar = c(4.5,4.5,1,.5))
plot(1, type = "l", lwd = 2, xlim = range(nuseds$Year), ylim = c(0,max(x_max)),
     ylab = "Number of surveys", xlab = "Years")

colours <- rainbow(n = length(unique(nuseds$region)))
for(i in 1:length(regions)){
  reg <- unique(nuseds$region)[i]
  cond <- nuseds$region == reg
  nuseds_cut <- nuseds[cond,]
  nuseds_cut_surveyYear <- nuseds_cut %>%
    group_by(Year) %>%
    summarise(count = n()) %>%
    arrange(Year)
  lines(x = nuseds_cut_surveyYear$Year, y = nuseds_cut_surveyYear$count, 
        pch = 16, col = colours[i])
  points(x = nuseds_cut_surveyYear$Year, y = nuseds_cut_surveyYear$count,
         pch = 16, col = colours[i])
}
legend("topleft",regions, fill = colours, bty = "n")

















