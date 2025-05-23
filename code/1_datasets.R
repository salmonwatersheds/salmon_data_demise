

#'******************************************************************************
#' The goal of the script is to make summary datasets from the cleaned NuSEDS
#' dataset to be used in 2_analyses.R.
#' 
#' Files imported:
#' - nuseds_cuid_streamid_2024-11-25.csv                 # the cleaned NuSEDS data avaiable at: https://zenodo.org/records/14225367
#' - region_survey.csv                                   # the region - populations (field "streamid") associations; created in 0_assign-regions.R
#' 
#' Files produced: 
#' - populationAssessed_catches_data_remove_0s_NAs.xlsx  # The summary files where both 0s and NAs counts were removed (results presented in the main text)
#' - populationAssessed_catches_data_",option_NAs.xlsx   # The summary files where only NAs counts were removed (results presented in the supporting information)
#' 
#'******************************************************************************

rm(list = ls())
graphics.off()

wd_figures <- paste0(getwd(),"/figures")
wd_data_input <- paste0(getwd(),"/data_input")
wd_data_output <- paste0(getwd(),"/data_output")

library(tidyr)
library(dplyr)
library(xlsx) # Package doesn't work..on mac?
library(readxl)
source("code/functions.R")

#'* OPTION CONCERNING THE 0s and NAs *
#' - option 1: remove both the NAs and Os (--> results in the main text)
#' - option 2: remove only the NAs
#' - option 3: replace NAs with 0s and 0s with NAs using the fields ADULT_PRESENCE and JACK_PRESENCE (--> not assessed)

i_option <- 2
option_0s_NAs <- c("remove_0s_NAs","remove_NAs","fix_0s_NAs")[i_option]

# Import source files ------
#

wd_data_input <- paste0(getwd(),"/data_input")

#'* Import the definition of the different fields of these two datasets *
fields_def <- nuseds_fields_definitions_fun(wd_references = wd_data_input)

#'* Import the file where GFE_ID are associated to region *
#' The file is generated in 0_assign-regions.R
#' It report the region where the survey was conducted, which can differ from the 
#' region of the CU certain time series belong to.
region_survey <- read.csv("data_output/region_survey.csv")
head(region_survey)

#'* Import the catch data from the NPAFC Statistics *
# Metadata: 
# https://www.npafc.org/wp-content/uploads/Statistics/Statistics-Metadata-Report-June2024.pdf
catch_total <- read_xlsx(paste0(wd_data_input,"/NPAFC_Catch_Stat-1925-2023.xlsx")) |> as.data.frame()
catch <- catch_total
# catch_total <- catch
colnames(catch) <- catch[1,]
catch <- catch[-1,]
head(catch)

# Only keep Canada
catch$Country |> unique()
cond <- catch$Country == "Canada"
catch <- catch[cond,]

# Only keep "Whole Country" (vs. BC or Yukon)
catch$`Whole Country/Province/State` |> unique()
cond <- catch$`Whole Country/Province/State` == "Whole country"
catch <- catch[cond,]

# Only keep Commercial
catch$`Catch Type` |> unique()
cond <- catch$`Catch Type` == "Commercial"
catch <- catch[cond,]

# Remove Steelhead (Check values 1st --> there is no values)
cond_SH <- catch$Species == "Steelhead"
catch[cond_SH,]
catch <- catch[!cond_SH,]

# Make sure all the counts are "numeric"
col_yr <- colnames(catch)[colnames(catch) %in% 1900:2050]
# apply(catch[,col_yr],2,as,numeric()) # does not work... 
for(yr in col_yr){
  catch[,yr] <- as.numeric(catch[,yr])
}

# Replace the Total values by the sum of the salmon species (without SH)
# (that should not change anything but it is the right way to proceed).
cond_Total <- catch$Species == "Total"
cond_count <- catch$`Data Type` == "Number (000's)"
cond_mt <- catch$`Data Type` == "Round wt (MT)"

# check 
layout(matrix(1:2,ncol = 2))
plot(x = catch[cond_Total & cond_count,col_yr] |> as.numeric(), y = colSums(catch[!cond_Total & cond_count,col_yr]))
abline(a = 0,b = 1)
plot(x = catch[cond_Total & cond_mt,col_yr] |> as.numeric(), y = colSums(catch[!cond_Total & cond_mt,col_yr]))
abline(a = 0,b = 1)

catch[cond_Total & cond_count,col_yr][1,] <- colSums(catch[!cond_Total & cond_count,col_yr])
catch[cond_Total & cond_mt,col_yr][1,] <- colSums(catch[!cond_Total & cond_mt,col_yr])


#' Make it a long format

unique(catch$Species)

# To long format
dataExport <- NULL
for(sp in unique(catch$Species)){
  # sp <- unique(catch$Species)[1]
  cond_sp <- catch$Species == sp
  count <- catch[cond_sp & cond_count,col_yr,drop = T] |> unlist()
  count <- count * 1000
  wt_t <- catch[cond_sp & cond_mt,col_yr,drop = T] |> unlist()
  wt_kg <- wt_t * 1000
  
  dataExportHere <- data.frame(species = sp,
                               year = as.numeric(col_yr),
                               count = count,
                               wt_kg = wt_kg)
  if(is.null(dataExport)){
    dataExport <- dataExportHere
  }else{
    dataExport <- rbind(dataExport,dataExportHere)
  }
}

Number_catches_species_total <- dataExport

# check that fish weight make sense:
hist(Number_catches_species_total$wt_kg/Number_catches_species_total$count)


#'* Import the cleaned NuSEDS data matched with PSF cuid and streamid *
#' This is the clean version of the New Salmon Escapement Database (NuSEDS). It 
#' must be downloaded at https://zenodo.org/records/14194639 and placed in the
#' /data_input folder.

nuseds <- read.csv(paste0(wd_data_input,"/nuseds_cuid_streamid_2025-04-15.csv"), 
                   header = T)
# nuseds <- read.csv(paste0(wd_data_input,"/nuseds_cuid_streamid_20240419.csv"), header = T)
head(nuseds)
nrow(nuseds) # 312539 306823

cond <- nuseds$region == "Northern Transboundary"
nuseds$region[cond] <- "Transboundary"

# edit the field streamid --> population_id to avoid confusion
# the field is a unique combination between a CU (cuid) and a stream location (GFE_ID)
# = a popualation
colnames(nuseds)[colnames(nuseds) == "streamid"] <- "population_id"

#' IMPORTANT NOTE: streamid = unique cuid & GFE_ID combination
#' The name 'streamid' is miss-leading as it seems to characterise a stream only
#' but it characterises a unique CU & location association.

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
# Remove NAs OR NAs and 0s (based on decision) ------
#

#'* Filter or fix 0s and/or NAs *
#'  

cond_NA <- is.na(nuseds$MAX_ESTIMATE)
sum(cond_NA) # 156507 152992
cond_0 <- nuseds$MAX_ESTIMATE == 0 & !cond_NA
sum(cond_0)  # 3449 2992

sum(cond_0)/sum(!cond_NA) * 100

if(option_0s_NAs == "remove_0s_NAs"){
  
  nuseds <- nuseds[!(cond_NA | cond_0),]
  nrow(nuseds) # 152583 150839
  
}else if(option_0s_NAs == "remove_NAs"){
  
  nuseds <- nuseds[!cond_NA,]
  nrow(nuseds) # 156032 153831
  
}else if(option_0s_NAs == "fix_0s_NAs"){
  
  table(nuseds$ADULT_PRESENCE[cond_0])
  # NONE OBSERVED NOT INSPECTED       PRESENT 
  #          2885             5           102 
  # --> set the 5 not inspected to NAs
  
  table(nuseds$ADULT_PRESENCE[cond_NA])
  # "" NONE OBSERVED NOT INSPECTED       PRESENT       UNKNOWN 
  # 30         35964         96798         16848          3352 
  # --> set the 35964 to 0s
  
  table(nuseds$JACK_PRESENCE[cond_0])
  #       NONE OBSERVED NOT INSPECTED       PRESENT 
  #  1813           665            36           478 
  # --> set the 36 not inspected to NAs
  
  table(nuseds$JACK_PRESENCE[cond_NA])
  #    "" NONE OBSERVED NOT INSPECTED       PRESENT       UNKNOWN 
  # 15971          2178          4460            91        130292 
  # --> set the 2178 to 0s
  
  cond_ADULT_PRESENCE_NONE_OBSERVED <- nuseds$ADULT_PRESENCE == "NONE OBSERVED"
  cond_ADULT_PRESENCE_NOT_INSPECTED <- nuseds$ADULT_PRESENCE == "NOT INSPECTED"
  cond_JACK_PRESENCE_NONE_OBSERVED <- nuseds$JACK_PRESENCE == "NONE OBSERVED"
  cond_JACK_PRESENCE_NOT_INSPECTED <- nuseds$JACK_PRESENCE == "NOT INSPECTED"
  
  cond_0_to_NA <- cond_0 & (cond_ADULT_PRESENCE_NOT_INSPECTED & cond_JACK_PRESENCE_NOT_INSPECTED)
  nuseds[cond_0_to_NA,]
  sum(cond_0_to_NA) # 0
  sum(cond_0 & cond_ADULT_PRESENCE_NOT_INSPECTED) # 5 ; if we only consider ADULT_PRESENCE
  
  cond_NA_to_0 <- cond_NA & (cond_ADULT_PRESENCE_NONE_OBSERVED | cond_JACK_PRESENCE_NONE_OBSERVED)
  sum(cond_NA_to_0) # 36397
  sum(cond_NA & cond_ADULT_PRESENCE_NONE_OBSERVED) # 35964 ; if we only consider ADULT_PRESENCE
  
  (sum(cond_0_to_NA) + sum(cond_NA_to_0)) / nrow(nuseds) * 100
  
  nuseds$MAX_ESTIMATE[cond_0_to_NA] <- NA
  nuseds$MAX_ESTIMATE[cond_NA_to_0] <- 0
  
  # Remove rows with NAs
  sum(is.na(nuseds$MAX_ESTIMATE)) # 116595
  sum(nuseds$MAX_ESTIMATE == 0 & !is.na(nuseds$MAX_ESTIMATE)) # 39389
  nuseds <- nuseds[!is.na(nuseds$MAX_ESTIMATE),]
  nrow(nuseds) # 190228
  
}

# 
colToKeep <- c("region","SPECIES","SPECIES_QUALIFIED","cu_name_pse","cuid","POP_ID","cu_name_dfo","CU_NAME",
               "SYSTEM_SITE","GFE_ID","X_LONGT","Y_LAT","population_id",
               "Year","MAX_ESTIMATE",
               "ESTIMATE_CLASSIFICATION","ESTIMATE_METHOD","stream_survey_quality")

nuseds <- nuseds[,colToKeep] 

# Some checks
sum(is.na(nuseds$population_id)) # 1121 1347 1102 1011
sum(is.na(nuseds$cuid))     # 1121 1347 1102 1011
sum(is.na(nuseds$cuid)) / nrow(nuseds) * 100 # 0.718 0.708 0.716 0.67
sum(is.na(nuseds$POP_ID))   # 0
sum(is.na(nuseds$GFE_ID))   # 0

length(unique(nuseds$cuid))     # 393 391
length(unique(nuseds$population_id)) # 6949 6767 6767
length(unique(nuseds$POP_ID))        # 6167 6009 6009 --> ideally should be = to nb population_id but several POP_ID are associated to multiple GFE_IDs in NuSEDS

# Check CU_NAMEs without a cuid
cond <- is.na(nuseds$cuid)
CU_NAME_noCuid <- nuseds[cond,c("region","SPECIES","CU_NAME")] |> unique()
CU_NAME_noCuid 
length(CU_NAME_noCuid$CU_NAME)                # there are 22 CUS without a cuid
cond_NA <- is.na(nuseds$population_id)
pop_NA <- unique(nuseds[cond_NA,c("region","SPECIES_QUALIFIED","CU_NAME","POP_ID","GFE_ID")]) # same nb rows as unique(nuseds[cond_NA,c("POP_ID","GFE_ID")])
nrow(pop_NA)                   # corresponding to 82 populations
sum(is.na(nuseds$cuid))        # corresponding number of data points

#
# Associate the region_survey ------
#

#' the file contain the "region" = CU-related region and "region_survey" = 
#' the stream-location region associated to a location (GFE_ID) for each populations.
#' There are consequently instances where a same GFE_ID is associated to multiple 
#' "regions" but there should not be any associated to multiple "region_survey".
head(region_survey)

nrow(region_survey) # 2454

# The GFE_ID associated to multiple regions:
GFE_ID_dupli <- region_survey$GFE_ID[duplicated(region_survey$GFE_ID)]
cond <- region_survey$GFE_ID %in% GFE_ID_dupli
check <- region_survey[cond,]
check <- check[order(check$GFE_ID),]
check

# drop the "region" column
region_survey <- unique(region_survey[,c("region_survey","GFE_ID")])
nrow(region_survey) # 2361
length(unique(region_survey$GFE_ID)) # 2361 --> same as above so each GFE_ID is associated one unique region_survey, ALL GOOD

nuseds <- merge(x = nuseds,
                y = region_survey,
                by = "GFE_ID", all.x = T) #  all.x and not all because the code needs to run for both cases when 0s are kept or removed

nrow(nuseds) # 156032

# Check there are no NAs
sum(is.na(nuseds$region_survey))

#
# Give a population_id to populations without a cuid -------
#

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

sum(is.na(nuseds$population_id))

#
# Produce the datasets -----
#

#'* Important notes *
#' The calculations on the number of population surveyed consider the populations
#' without a cuid, the ones on the number of CUs, do not.

cond_cuid_NA <- is.na(nuseds$cuid)
sum(cond_cuid_NA) # 1121
sum(cond_cuid_NA) / nrow(nuseds) * 100 # 0.72%

years <- nuseds$Year |> unique()
years <- years[order(years)]

# To deal with odd and even Pink CUs
years_odd <- years[years %% 2 == 1]
years_even <- years[years %% 2 == 0]

#'* Total count and proportions population and CUs surveyed: *

cond_odd <- nuseds$Year %in% years_odd
cond_even <- nuseds$Year %in% years_even
# SP: I don't understand why these counts have to be separated for even and odd years?
# BSC: Because of the pink odd and even CUs: the proportions of populations (population_id)
# in each year has to account for the fact that the pink population are only 
# assessed every two years. So we should not count the presence or absence of 
# pink populations in their off years.

count_odd <- sapply(years_odd,function(y){
  cond <- nuseds$Year == y
  out <- length(unique(nuseds$population_id[cond]))
  return(out)
})

count_even <- sapply(years_even,function(y){
  cond <- nuseds$Year == y
  out <- length(unique(nuseds$population_id[cond]))
  return(out)
})

# proportion of populations assessed 
nb_pop_total_odd <- length(unique(nuseds$population_id[cond_odd]))
proportion_odd <- count_odd / nb_pop_total_odd
nb_pop_total_even <- length(unique(nuseds$population_id[cond_even]))
proportion_even <- count_even / nb_pop_total_even

# number of CUs assessed
count_CU_odd <-  sapply(years_odd,function(y){
  cond <- nuseds$Year == y
  out <- length(unique(nuseds$cuid[cond & !cond_cuid_NA]))
  return(out)
})

count_CU_even <-  sapply(years_even,function(y){
  cond <- nuseds$Year == y
  out <- length(unique(nuseds$cuid[cond & !cond_cuid_NA]))
  return(out)
})

# proportion of CUs assessed
nb_CU_total_odd <- length(unique(nuseds$cuid[cond_odd & !cond_cuid_NA]))
proportion_CU_odd <- count_CU_odd/nb_CU_total_odd

nb_CU_total_even <- length(unique(nuseds$cuid[cond_even & !cond_cuid_NA]))
proportion_CU_even <- count_CU_even/nb_CU_total_even


dataExport <- data.frame(year = c(years_odd,years_even),
                         count_pop = c(count_odd,count_even),
                         proportion_pop = c(proportion_odd,proportion_even),
                         count_CU = c(count_CU_odd,count_CU_even),
                         proportion_CU = c(proportion_CU_odd,proportion_CU_even))

dataExport <- dataExport[order(dataExport$year),]

Number_Prop_populationsAssessed_total <- dataExport

# Check that counting population_id or data points per years is the same 
dataExport_2 <- nuseds %>%
  group_by(Year) %>%
  summarise(count = n()) %>%
  arrange(Year)

head(dataExport_2)

plot(dataExport$count_pop, dataExport_2$count)
abline(a = 0, b = 1)

identical(dataExport$count_pop,dataExport_2$count) # TRUE

#'* Count of surveys per region *

cond_cuid_NA <- is.na(nuseds$cuid) # same as above

regions <- nuseds$region_survey |> unique()

dataExport <- NULL
for(rg in regions){
  # rg <- regions[5]
  cond_rg <- nuseds$region_survey == rg
  
  cond_rg_CU <- nuseds$region == rg
  
  cond_odd <- nuseds$Year %in% years_odd
  cond_even <- nuseds$Year %in% years_even
  
  # count the number of populations
  count_odd <- sapply(years_odd, function(y){
    # y <- dataExport$year[50]
    cond <- cond_rg & nuseds$Year == y
    out <- length(nuseds$population_id[cond])
    return(out)
  })
  
  count_even <- sapply(years_even, function(y){
    # y <- dataExport$year[50]
    cond <- cond_rg & nuseds$Year == y
    out <- length(nuseds$population_id[cond])
    return(out)
  })
  
  # proportion populations assessed
  nb_pop_total_odd <- length(unique(nuseds[cond_rg & cond_odd,]$population_id))
  proportion_odd <- count_odd / nb_pop_total_odd
  nb_pop_total_even <- length(unique(nuseds[cond_rg & cond_even,]$population_id))
  proportion_even <- count_even / nb_pop_total_even
  
  # number of CUs
  count_CU_odd <-  sapply(years_odd,function(y){
    cond <- cond_rg_CU & nuseds$Year == y
    out <- length(unique(nuseds$cuid[cond & !cond_cuid_NA]))
    return(out)
  })
  
  count_CU_even <-  sapply(years_even,function(y){
    cond <- cond_rg_CU & nuseds$Year == y
    out <- length(unique(nuseds$cuid[cond & !cond_cuid_NA]))
    return(out)
  })
  
  # proportion CUs assessed
  nb_CU_total_odd <- length(unique(nuseds[cond_rg_CU & cond_odd & !cond_cuid_NA,]$cuid))
  proportion_CU_odd <- count_CU_odd/nb_CU_total_odd
  
  nb_CU_total_even <- length(unique(nuseds[cond_rg_CU & cond_even & !cond_cuid_NA,]$cuid))
  proportion_CU_even <- count_CU_even/nb_CU_total_even
  
  dataExportHere <- data.frame(region = rg, 
                               year = c(years_odd,years_even),
                               count_pop = c(count_odd,count_even),
                               proportion_pop = c(proportion_odd,proportion_even),
                               count_CU = c(count_CU_odd,count_CU_even),
                               proportion_CU = c(proportion_CU_odd,proportion_CU_even))
  
  dataExportHere <- dataExportHere[order(dataExportHere$year),]
  
  if(is.null(dataExport)){
    dataExport <- dataExportHere
  }else{
    dataExport <- rbind(dataExport,dataExportHere)
  }
}

Number_Prop_populationsAssessed_regions <- dataExport


#'* Count of surveys per species *

cond_cuid_NA <- is.na(nuseds$cuid) # same as above

species <- nuseds$SPECIES |> unique()

dataExport <- NULL
for(sp in species){
  # sp <- species[1]
  # sp <- "Pink"
  cond_sp <- nuseds$SPECIES == sp
  
  if(sp == "Pink"){
    
    cond_sp_odd <- nuseds$SPECIES_QUALIFIED == "PKO"
    cond_sp_even <- nuseds$SPECIES_QUALIFIED == "PKE"
    
    # count the number of populations
    count_odd <- sapply(years_odd, function(y){
      # y <- dataExport$year[50]
      cond <- cond_sp_odd & nuseds$Year == y
      out <- length(nuseds$population_id[cond])
      return(out)
    })
    
    count_even <- sapply(years_even, function(y){
      # y <- dataExport$year[50]
      cond <- cond_sp_even & nuseds$Year == y
      out <- length(nuseds$population_id[cond])
      return(out)
    })
    
    # proportion populations assessed
    nb_pop_total_odd <- length(unique(nuseds[cond_sp_odd,]$population_id))
    proportion_odd <- count_odd / nb_pop_total_odd
    nb_pop_total_even <- length(unique(nuseds[cond_sp_even,]$population_id))
    proportion_even <- count_even / nb_pop_total_even
    
    # number of CUs assessed
    count_CU_odd <- sapply(years_odd,function(y){
      cond <- cond_sp_odd & nuseds$Year == y
      out <- length(unique(nuseds$cuid[cond & !cond_cuid_NA]))
      return(out)
    })
    
    count_CU_even <- sapply(years_even,function(y){
      cond <- cond_sp_even & nuseds$Year == y
      out <- length(unique(nuseds$cuid[cond & !cond_cuid_NA]))
      return(out)
    })
    
    # proportion CUs assessed
    nb_CU_total_odd <- length(unique(nuseds[cond_sp_odd & !cond_cuid_NA,]$cuid))
    proportion_CU_odd <- count_CU_odd/nb_CU_total_odd
    
    nb_CU_total_even <- length(unique(nuseds[cond_sp_even & !cond_cuid_NA,]$cuid))
    proportion_CU_even <- count_CU_even/nb_CU_total_even
    
    data_pink <- data.frame(year = c(years_odd,years_even), 
                            count_pop = c(count_odd,count_even), 
                            proportion_pop = c(proportion_odd,proportion_even),
                            count_CU = c(count_CU_odd,count_CU_even),
                            proportion_CU = c(proportion_CU_odd,proportion_CU_even))
    
    data_pink <- data_pink[order(data_pink$year),]
    
    count_pop <- data_pink$count_pop
    proportion_pop <- data_pink$proportion_pop
    count_CU <- data_pink$count_CU
    proportion_CU <- data_pink$proportion_CU
    
  }else{ # if not pink
    
    # count populations
    count_pop <- sapply(years, function(y){
      # y <- dataExport$year[50]
      cond <- cond_sp & nuseds$Year == y
      out <- length(nuseds$population_id[cond])
      if(out != length(unique(nuseds$population_id[cond]))){
        print("Duplicated population_id TO INVESTIGATE")
      }
      return(out)
    })
    
    # proportion populations assessed
    nb_pop_total <- length(unique(nuseds[cond_sp,]$population_id))
    proportion_pop <- count_pop / nb_pop_total
    
    # count number CUs
    count_CU <- sapply(years,function(y){
      cond <- cond_sp & nuseds$Year == y
      out <- length(unique(nuseds$cuid[cond & !cond_cuid_NA]))
      return(out)
    })
    
    # proportion CUs assessed
    nb_CU_total <- length(unique(nuseds$cuid[cond_sp & !cond_cuid_NA]))
    proportion_CU <- count_CU/nb_CU_total
    
  }
  
  dataExportHere <- data.frame(species = sp, 
                               year = years,
                               count_pop = count_pop,
                               proportion_pop = proportion_pop,
                               count_CU = count_CU,
                               proportion_CU = proportion_CU)
  
  if(is.null(dataExport)){
    dataExport <- dataExportHere
  }else{
    dataExport <- rbind(dataExport,dataExportHere)
  }
}

Number_Prop_populationsAssessed_species <- dataExport


#'* Count of surveys per region > species *

regions <- nuseds$region_survey |> unique()

cond_cuid_NA <- is.na(nuseds$cuid) # same as above

dataExport <- NULL
for(rg in regions){
  # rg <- regions[2]
  # rg <- "Central Coast"
  cond_rg <- nuseds$region_survey == rg
  cond_rg_CU <- nuseds$region == rg
  species <- nuseds$SPECIES[cond_rg] |> unique()
  
  for(sp in species){
    # sp <- species[1]
    # sp <- "Pink"
    cond_rg_sp <- cond_rg & nuseds$SPECIES == sp
    
    # Note that what dealing with CUs we use region and not region_survey
    cond_rg_sp_CUs <- cond_rg_CU & nuseds$SPECIES == sp
    
    # count
    if(sp == "Pink"){
      
      cond_rg_sp_odd <- cond_rg & nuseds$SPECIES_QUALIFIED == "PKO"
      cond_rg_sp_even <- cond_rg & nuseds$SPECIES_QUALIFIED == "PKE"
      
      cond_rg_sp_CUs_odd <- cond_rg_CU & nuseds$SPECIES_QUALIFIED == "PKO"
      cond_rg_sp_CUs_even <- cond_rg_CU & nuseds$SPECIES_QUALIFIED == "PKE"
      
      count_odd <- sapply(years_odd, function(y){
        # y <- dataExport$year[50]
        cond <- cond_rg_sp_odd & nuseds$Year == y
        out <- length(nuseds$population_id[cond])
        return(out)
      })
      
      count_even <- sapply(years_even, function(y){
        # y <- dataExport$year[50]
        cond <- cond_rg_sp_even & nuseds$Year == y
        out <- length(nuseds$population_id[cond])
        return(out)
      })
      
      # proportion populations assessed
      nb_pop_total_odd <- length(unique(nuseds[cond_rg_sp_odd,]$population_id))
      proportion_odd <- count_odd / nb_pop_total_odd
      nb_pop_total_even <- length(unique(nuseds[cond_rg_sp_even,]$population_id))
      proportion_even <- count_even / nb_pop_total_even
      
      # number of CUs assessed
      count_CU_odd <- sapply(years_odd,function(y){
        cond <- cond_rg_sp_CUs_odd & nuseds$Year == y
        out <- length(unique(nuseds$cuid[cond & !cond_cuid_NA]))
        return(out)
      })
      
      count_CU_even <- sapply(years_even,function(y){
        cond <- cond_rg_sp_CUs_even & nuseds$Year == y
        out <- length(unique(nuseds$cuid[cond & !cond_cuid_NA]))
        return(out)
      })
      
      # proportion of CUs
      nb_CU_total_odd <- length(unique(nuseds$cuid[cond_rg_sp_odd & !cond_cuid_NA]))
      if(nb_CU_total_odd == 0){
        proportion_CU_odd <- count_CU_odd # because the latter should only be 0
      }else{
        proportion_CU_odd <- count_CU_odd/nb_CU_total_odd
      }
      
      nb_CU_total_even <- length(unique(nuseds$cuid[cond_rg_sp_even & !cond_cuid_NA]))
      if(nb_CU_total_even == 0){
        proportion_CU_even <- count_CU_even # because the latter should only be 0
      }else{
        proportion_CU_even <- count_CU_even/nb_CU_total_even
      }
      
      data_pink <- data.frame(year = c(years_odd,years_even), 
                              count_pop = c(count_odd,count_even), 
                              proportion_pop = c(proportion_odd,proportion_even),
                              count_CU = c(count_CU_odd,count_CU_even),
                              proportion_CU = c(proportion_CU_odd,proportion_CU_even))
      
      data_pink <- data_pink[order(data_pink$year),]
      
      count_pop <- data_pink$count_pop
      proportion_pop <- data_pink$proportion_pop
      count_CU <- data_pink$count_CU
      proportion_CU <- data_pink$proportion_CU
      
    }else{ # not Pink salmon
      
      # count the number of populations
      count_pop <- sapply(years, function(y){
        # y <- dataExport$year[50]
        cond <- cond_rg_sp & nuseds$Year == y
        out <- length(nuseds$population_id[cond])
        if(out != length(unique(nuseds$population_id[cond]))){
          print("Duplicated population_id TO INVESTIGATE")
        }
        return(out)
      })
      
      # proportion populations assessed
      nb_pop_total <- length(unique(nuseds[cond_rg_sp,]$population_id))
      proportion_pop <- count_pop/nb_pop_total
      
      # count number of CUs
      count_CU <- sapply(years,function(y){
        cond <- cond_rg_sp_CUs & nuseds$Year == y #
        out <- length(unique(nuseds$cuid[cond & !cond_cuid_NA]))
        return(out)
      })
      
      # proportion CUs assessed
      nb_CU_total <- length(unique(nuseds[cond_rg_sp_CUs & !cond_cuid_NA,]$cuid))
      proportion_CU <- count_CU/nb_CU_total
      
    }
    
    dataExportHere <- data.frame(region = rg, 
                                 species = sp,
                                 year = years,
                                 count_pop = count_pop,
                                 proportion_pop = proportion_pop,
                                 count_CU = count_CU,
                                 proportion_CU = proportion_CU)
    
    if(is.null(dataExport)){
      dataExport <- dataExportHere
    }else{
      dataExport <- rbind(dataExport,dataExportHere)
    }
  }
}

head(dataExport)
# View(dataExport)

Number_Prop_populationsAssessed_regions_species <- dataExport

cond <- Number_Prop_populationsAssessed_regions_species$region == "Fraser" & 
  Number_Prop_populationsAssessed_regions_species$species == "Pink"
Number_Prop_populationsAssessed_regions_species$count_pop[cond]

cond <- Number_Prop_populationsAssessed_regions_species$region == "Fraser" &
  Number_Prop_populationsAssessed_regions_species$species == "Coho"
Number_Prop_populationsAssessed_regions_species$count_pop[cond]


#'* Count of surveys per species > stream_survey_quality *

cond_cuid_NA <- is.na(nuseds$cuid) # same as above

species <- nuseds$SPECIES |> unique()

dataExport <- NULL
for(sp in species){
  # sp <- species[5]
  # sp <- "Pink"
  cond_sp <- nuseds$SPECIES == sp
  
  for(ssq in unique(nuseds$stream_survey_quality[cond_sp])){
    # ssq <- unique(nuseds$stream_survey_quality[cond_sp])[1]
    # ssq <- NA
    
    if(is.na(ssq)){
      cond_ssq <- is.na(nuseds$stream_survey_quality)
    }else{
      cond_ssq <- !is.na(nuseds$stream_survey_quality) & nuseds$stream_survey_quality == ssq
    }
    
    if(sp == "Pink"){
      
      cond_sp_odd <- nuseds$SPECIES_QUALIFIED == "PKO"
      cond_sp_even <- nuseds$SPECIES_QUALIFIED == "PKE"
      
      # count the number of populations
      count_odd <- sapply(years_odd, function(y){
        # y <- dataExport$year[50]
        cond <- cond_sp_odd & cond_ssq & nuseds$Year == y
        out <- length(nuseds$population_id[cond])
        return(out)
      })
      
      count_even <- sapply(years_even, function(y){
        # y <- dataExport$year[50]
        cond <- cond_sp_even & cond_ssq & nuseds$Year == y
        out <- length(nuseds$population_id[cond])
        return(out)
      })
      
      # proportion populations assessed
      nb_pop_total_odd <- length(unique(nuseds[cond_sp_odd & cond_ssq,]$population_id))
      proportion_odd <- count_odd / nb_pop_total_odd
      nb_pop_total_even <- length(unique(nuseds[cond_sp_even & cond_ssq,]$population_id))
      proportion_even <- count_even / nb_pop_total_even
      
      # number of CUs assessed
      count_CU_odd <- sapply(years_odd,function(y){
        cond <- cond_sp_odd & cond_ssq & nuseds$Year == y
        out <- length(unique(nuseds$cuid[cond & !cond_cuid_NA]))
        return(out)
      })
      
      count_CU_even <- sapply(years_even,function(y){
        cond <- cond_sp_even & cond_ssq & nuseds$Year == y
        out <- length(unique(nuseds$cuid[cond & !cond_cuid_NA]))
        return(out)
      })
      
      # proportion CUs assessed
      nb_CU_total_odd <- length(unique(nuseds[cond_sp_odd & cond_ssq & !cond_cuid_NA,]$cuid))
      proportion_CU_odd <- count_CU_odd/nb_CU_total_odd
      
      nb_CU_total_even <- length(unique(nuseds[cond_sp_even & cond_ssq & !cond_cuid_NA,]$cuid))
      proportion_CU_even <- count_CU_even/nb_CU_total_even
      
      data_pink <- data.frame(year = c(years_odd,years_even), 
                              count_pop = c(count_odd,count_even), 
                              proportion_pop = c(proportion_odd,proportion_even),
                              count_CU = c(count_CU_odd,count_CU_even),
                              proportion_CU = c(proportion_CU_odd,proportion_CU_even))
      
      data_pink <- data_pink[order(data_pink$year),]
      
      count_pop <- data_pink$count_pop
      proportion_pop <- data_pink$proportion_pop
      count_CU <- data_pink$count_CU
      proportion_CU <- data_pink$proportion_CU
      
    }else{ # if not pink
      
      # count populations
      count_pop <- sapply(years, function(y){
        # y <- dataExport$year[50]
        cond <- cond_sp & cond_ssq & nuseds$Year == y
        out <- length(nuseds$population_id[cond])
        if(out != length(unique(nuseds$population_id[cond]))){
          print("Duplicated population_id TO INVESTIGATE")
        }
        return(out)
      })
      
      # proportion populations assessed
      nb_pop_total <- length(unique(nuseds[cond_sp & cond_ssq,]$population_id))
      proportion_pop <- count_pop / nb_pop_total
      
      # count number CUs
      count_CU <- sapply(years,function(y){
        cond <- cond_sp & cond_ssq & nuseds$Year == y
        out <- length(unique(nuseds$cuid[cond & !cond_cuid_NA]))
        return(out)
      })
      
      # proportion CUs assessed
      nb_CU_total <- length(unique(nuseds$cuid[cond_sp & cond_ssq & !cond_cuid_NA]))
      proportion_CU <- count_CU/nb_CU_total
      
    }
    
    dataExportHere <- data.frame(species = sp,
                                 stream_survey_quality = ssq,
                                 year = years,
                                 count_pop = count_pop,
                                 proportion_pop = proportion_pop,
                                 count_CU = count_CU,
                                 proportion_CU = proportion_CU)
    
    if(is.null(dataExport)){
      dataExport <- dataExportHere
    }else{
      dataExport <- rbind(dataExport,dataExportHere)
    }
  }
}

Number_Prop_populationsAssessed_species_ssq <- dataExport


#'* Summary table for each regions *

# Define the order of the regions
regions <- c("Yukon","Transboundary","Haida Gwaii","Nass","Skeena","Central Coast",
             "Vancouver Island & Mainland Inlets","Fraser","Columbia")

summary <- data.frame(region = regions) 

summary$CU_nb <- sapply(summary$region,function(rg){
  cond <- nuseds$region == rg
  return(length(unique(nuseds$cuid[cond & !cond_cuid_NA])))
})

summary$pop_nb <- sapply(summary$region,function(rg){
  cond <- nuseds$region_survey == rg # Changed to region_survey
  return(length(unique(nuseds$population_id[cond])))
})

summary$site_nb <- sapply(summary$region,function(rg){
  cond <- nuseds$region_survey == rg
  return(nrow(unique(nuseds[cond,c("GFE_ID","SYSTEM_SITE","X_LONGT","Y_LAT")]))) # GFE_ID would be enough
})

# Proportion of populations in the last decade with estimates (2014-2023)
summary$prop_pop_recent <- sapply(summary$region, function(rg){
  cond_rg <- nuseds$region_survey == rg
  cond_2014 <- nuseds$Year >= 2014
  out <- length(unique(nuseds$population_id[cond_rg & cond_2014]))/length(unique(nuseds$population_id[cond_rg]))
  return(round(out,3))
})

# Proportion of populations in the peak (1980-1989) with estimates
summary$prop_pop_80s <- sapply(summary$region, function(rg){
  cond_rg <- nuseds$region_survey == rg
  cond_80s <- nuseds$Year %in% c(1980:1989)
  out <- length(unique(nuseds$population_id[cond_rg & cond_80s]))/length(unique(nuseds$population_id[cond_rg]))
  return(round(out, 3))
})

sum_sum <- colSums(summary[,c("CU_nb","pop_nb","site_nb")])
summary <- rbind(summary,
                 c("Overall", sum_sum, 
                   round(length(unique(nuseds$population_id[nuseds$Year %in% c(2013:2022)]))/length(unique(nuseds$population_id)), 3), # over all regions
                   round(length(unique(nuseds$population_id[nuseds$Year %in% c(1980:1989)]))/length(unique(nuseds$population_id)), 3) # over all regions
                 ))

#
# Export excel file -------
#
files_l <- list(Number_Prop_populationsAssessed_total,
                Number_Prop_populationsAssessed_regions,
                Number_Prop_populationsAssessed_species,
                Number_Prop_populationsAssessed_regions_species,
                Number_Prop_populationsAssessed_species_ssq,
                Number_catches_species_total,
                summary)

names(files_l) <- c("populations_total",
                    "populations_regions",
                    "populations_species",
                    "populations_regions_species",
                    "populations_species_ssq",
                    "catches_species_total",
                    "summary_regions")

for(sh_i in 1:length(names(files_l))){
  # sh_i <- 1
  if(sh_i == 1){
    append <- F
  }else{
    append <- T
  }
  sheetName <- names(files_l)[sh_i]
  sheet <- as.data.frame(files_l[[sheetName]])
  write.xlsx(sheet, 
             file = paste0(wd_data_output,"/populationAssessed_catches_data_",option_0s_NAs,".xlsx"),
             sheetName = sheetName, 
             row.names = FALSE,
             append = append,
             showNA = T)
  print(sh_i)
}








