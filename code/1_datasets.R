

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

#'* Import the cleaned NuSEDS data matched with PSF cuid and streamid *
#' This is the clean version of the New Salmon Escapement Database (NuSEDS). It 
#' must be downloaded at https://zenodo.org/records/14194639 and placed in the
#' /data_input folder.

nuseds <- read.csv(paste0(wd_data_input,"/nuseds_cuid_streamid_2024-11-25.csv"), 
                   header = T)
# nuseds <- read.csv(paste0(wd_data_input,"/nuseds_cuid_streamid_20240419.csv"), header = T)
head(nuseds)
nrow(nuseds) # 306823

cond <- nuseds$region == "Northern Transboundary"
nuseds$region[cond] <- "Transboundary"

#' IMPORTANT NOTE: streamid = unique cuid & GFE_ID combination
#' The name 'streamid' is miss-leading as it seems to characterise a stream only
#' but it characterises a unique CU & location association.


#'* Filter or fix 0s and/or NAs *
#'  

nrow(nuseds) # 306823

cond_NA <- is.na(nuseds$MAX_ESTIMATE)
sum(cond_NA) # 152992
cond_0 <- nuseds$MAX_ESTIMATE == 0 & !cond_NA
sum(cond_0)  # 2992

if(option_0s_NAs == "remove_0s_NAs"){
  
  nuseds <- nuseds[!(cond_NA | cond_0),]
  nrow(nuseds) # 150839
  
}else if(option_0s_NAs == "remove_NAs"){
  
  nuseds <- nuseds[!cond_NA,]
  nrow(nuseds) # 153831
  
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
colToKeep <- c("region","SPECIES","cu_name_pse","cuid","POP_ID","cu_name_dfo","CU_NAME",
               "SYSTEM_SITE","GFE_ID","X_LONGT","Y_LAT","streamid",
               "Year","MAX_ESTIMATE",
               "ESTIMATE_CLASSIFICATION","ESTIMATE_METHOD","stream_survey_quality")

nuseds <- nuseds[,colToKeep] 

# Some checks
sum(is.na(nuseds$streamid)) # 1347 1102 1011
sum(is.na(nuseds$cuid))     # 1347 1102 1011
sum(is.na(nuseds$cuid)) / nrow(nuseds) * 100 # 0.708 0.716 0.67
sum(is.na(nuseds$POP_ID))   # 0
sum(is.na(nuseds$GFE_ID))   # 0

length(unique(nuseds$cuid))     # 391
length(unique(nuseds$streamid)) # 6767 6767
length(unique(nuseds$POP_ID))   # 6009 6009

# Check CU_NAMEs without a cuid
cond <- is.na(nuseds$cuid)
CU_NAME_noCuid <- nuseds[cond,c("region","SPECIES","CU_NAME")] |> unique()
CU_NAME_noCuid 
length(CU_NAME_noCuid$CU_NAME) # 22 CUS

#'* Remove populations without a cuid * 

# How many "populations" do not have a CUID?
length(unique(nuseds$POP_ID[is.na(nuseds$cuid)])) # 82

# remove them
cond <- !is.na(nuseds$cuid)
nuseds <- nuseds[cond,]
nrow(nuseds) # 188881 152729 149828
sum(is.na(nuseds$streamid)) # 0
sum(is.na(nuseds$GFE_ID))   # 0

length(unique(nuseds$streamid)) # 6766
length(unique(nuseds$GFE_ID))   # 2300
length(unique(nuseds$cuid))     # 390 
unique(nuseds$region)

# check if there are duplicated streamid
data_check <- nuseds[cond,c("cuid","GFE_ID","streamid")] |> unique()
any(duplicated(data_check$streamid)) # FALSE

#'* Import the definition of the different fields of these two datasets *
fields_def <- nuseds_fields_definitions_fun(wd_references = wd_data_input)

nuseds$region |> unique() |> length()
nuseds$cuid |> unique() |> length()    # 390
nuseds$streamid |> unique() |> length()    # 6766

#'* Import the file where streamid are associated to region *
#' The file is generated in 0_assign-regions.R
region_survey <- read.csv("data_output/region_survey.csv")
head(region_survey)

# Merge region_survey assignment from 0_assign-regions.R that assigns PSE region
# to survey sites based on exact location (rather than CU assignment)
nuseds <- nuseds %>% left_join(region_survey, by = "streamid")

# Check there are no NAs
sum(is.na(nuseds$region_survey))

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

# Only keep number of fish (discard weights)
catch$`Data Type` |> unique()  # "Number (000's)" "Round wt (MT)" = round weight of fish, which is the weight of the whole fish before processing
cond <- catch$`Data Type` == "Number (000's)"
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

# check 
plot(x = catch[cond_Total,col_yr] |> as.numeric(), y = colSums(catch[!cond_Total,col_yr]))
abline(a = 0,b = 1)

catch[cond_Total,col_yr][1,] <- colSums(catch[!cond_Total,col_yr])

#
# Produce the datasets -----
#

years <- nuseds$Year |> unique()
years <- years[order(years)]

# To deal with odd and even Pink CUs
years_odd <- years[years %% 2 == 1]
years_even <- years[years %% 2 == 0]

#'* Total count and proportions population and CUs surveyed: *

cond_odd <- nuseds$Year %in% years_odd
cond_even <- nuseds$Year %in% years_even
# SP: I don't understand why these counts have to be separated for even and odd years?
# BSC: Because of the pink odd and even CUs: the proportions of populations (streamid)
# in each year has to account for the fact that the pink population are only 
# assessed every two years. So we should not could the present or absence of 
# pink populations in their off year.

count_odd <- sapply(years_odd,function(y){
  cond <- nuseds$Year == y
  out <- length(unique(nuseds$streamid[cond]))
  return(out)
})

count_even <- sapply(years_even,function(y){
  cond <- nuseds$Year == y
  out <- length(unique(nuseds$streamid[cond]))
  return(out)
})

# proportion of populations assessed 
nb_pop_total_odd <- length(unique(nuseds$streamid[cond_odd]))
proportion_odd <- count_odd / nb_pop_total_odd
nb_pop_total_even <- length(unique(nuseds$streamid[cond_even]))
proportion_even <- count_even / nb_pop_total_even

# proportion of CUs assessed
nb_CU_total_odd <- length(unique(nuseds$cuid[cond_odd]))
proportion_CU_odd <- sapply(years_odd, function(y){
  cond <- nuseds$Year == y
  out <- length(unique(nuseds$cuid[cond]))
  return(out / nb_CU_total_odd)
})
nb_CU_total_even <- length(unique(nuseds$cuid[cond_even]))
proportion_CU_even <- sapply(years_even, function(y){
  cond <- nuseds$Year == y
  out <- length(unique(nuseds$cuid[cond]))
  return(out / nb_CU_total_even)
})

dataExport <- data.frame(year = c(years_odd,years_even),
                         count = c(count_odd,count_even),
                         proportion = c(proportion_odd,proportion_even),
                         proportion_CU = c(proportion_CU_odd,proportion_CU_even))


dataExport <- dataExport[order(dataExport$year),]

Number_Prop_populationsAssessed_total <- dataExport

# Check that counting streamid or data points per years is the same 
dataExport_2 <- nuseds %>%
  group_by(Year) %>%
  summarise(count = n()) %>%
  arrange(Year)

head(dataExport_2)

plot(dataExport$count, dataExport_2$count)
abline(a = 0, b = 1)

identical(dataExport$count,dataExport_2$count) # TRUE

#'* Count of surveys per region *

regions <- nuseds$region_survey |> unique()

dataExport <- NULL
for(rg in regions){
  # rg <- regions[5]
  cond_rg <- nuseds$region_survey == rg
  
  cond_rg_CU <- nuseds$region == rg
  
  cond_odd <- nuseds$Year %in% years_odd
  cond_even <- nuseds$Year %in% years_even
  
  # count
  count_odd <- sapply(years_odd, function(y){
    # y <- dataExport$year[50]
    cond <- nuseds$region_survey == rg & nuseds$Year == y
    out <- length(nuseds$streamid[cond])
    return(out)
  })
  
  count_even <- sapply(years_even, function(y){
    # y <- dataExport$year[50]
    cond <- nuseds$region_survey == rg & nuseds$Year == y
    out <- length(nuseds$streamid[cond])
    return(out)
  })
  
  # proportion populations assessed
  nb_pop_total_odd <- length(unique(nuseds[cond_rg & cond_odd,]$streamid))
  proportion_odd <- count_odd / nb_pop_total_odd
  nb_pop_total_even <- length(unique(nuseds[cond_rg & cond_even,]$streamid))
  proportion_even <- count_even / nb_pop_total_even
  
  # proportion CUs assessed
  nb_CU_total_odd <- length(unique(nuseds[cond_rg_CU & cond_odd,]$cuid))
  proportion_CU_odd <- sapply(years_odd,function(y){
    cond <- nuseds$region == rg & nuseds$Year == y
    out <- length(unique(nuseds$cuid[cond]))
    return(out / nb_CU_total_odd)
  })
  
  nb_CU_total_even <- length(unique(nuseds[cond_rg_CU & cond_even,]$cuid))
  proportion_CU_even <- sapply(years_even,function(y){
    cond <- nuseds$region == rg & nuseds$Year == y
    out <- length(unique(nuseds$cuid[cond]))
    return(out / nb_CU_total_even)
  })
  
  dataExportHere <- data.frame(region = rg, 
                               year = c(years_odd,years_even),
                               count = c(count_odd,count_even),
                               proportion = c(proportion_odd,proportion_even),
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
species <- nuseds$SPECIES |> unique()

dataExport <- NULL
for(sp in species){
  # sp <- species[1]
  # sp <- "Pink"
  cond_sp <- nuseds$SPECIES == sp
  
  if(sp == "Pink"){
    
    cond_sp_odd <- cond_sp & grepl("odd",nuseds$cu_name_pse)
    cond_sp_even <- cond_sp & grepl("even",nuseds$cu_name_pse)
    
    count_odd <- sapply(years_odd, function(y){
      # y <- dataExport$year[50]
      cond <- nuseds$SPECIES == sp & nuseds$Year == y & grepl("odd",nuseds$cu_name_pse)
      out <- length(nuseds$streamid[cond])
      return(out)
    })
    count_even <- sapply(years_even, function(y){
      # y <- dataExport$year[50]
      cond <- nuseds$SPECIES == sp & nuseds$Year == y & grepl("even",nuseds$cu_name_pse)
      out <- length(nuseds$streamid[cond])
      return(out)
    })
    
    # proportion populations assessed
    nb_pop_total_odd <- length(unique(nuseds[cond_sp_odd,]$streamid))
    proportion_odd <- count_odd / nb_pop_total_odd
    nb_pop_total_even <- length(unique(nuseds[cond_sp_even,]$streamid))
    proportion_even <- count_even / nb_pop_total_even
    
    # proportion CUs assessed
    nb_CU_total_odd <- length(unique(nuseds[cond_sp_odd,]$cuid))
    proportion_CU_odd <- sapply(years_odd,function(y){
      cond <- nuseds$SPECIES == sp & nuseds$Year == y & grepl("odd",nuseds$cu_name_pse)
      out <- length(unique(nuseds$cuid[cond]))
      return(out / nb_CU_total_odd)
    })
    nb_CU_total_even <- length(unique(nuseds[cond_sp_even,]$cuid))
    proportion_CU_even <- sapply(years_even,function(y){
      cond <- nuseds$SPECIES == sp & nuseds$Year == y & grepl("even",nuseds$cu_name_pse)
      out <- length(unique(nuseds$cuid[cond]))
      return(out / nb_CU_total_even)
    })
    
    data_pink <- data.frame(year = c(years_odd,years_even), 
                            count = c(count_odd,count_even), 
                            proportion = c(proportion_odd,proportion_even),
                            proportion_CU = c(proportion_CU_odd,proportion_CU_even))
    
    data_pink <- data_pink[order(data_pink$year),]
    
    count <- data_pink$count
    proportion <- data_pink$proportion
    proportion_CU <- data_pink$proportion_CU
    
  }else{ # if not pink
    # count
    count <- sapply(years, function(y){
      # y <- dataExport$year[50]
      cond <- nuseds$SPECIES == sp & nuseds$Year == y
      out <- length(nuseds$streamid[cond])
      if(out != length(unique(nuseds$streamid[cond]))){
        print("Duplicated streamid TO INVESTIGATE")
      }
      return(out)
    })
    
    # proportion populations assessed
    nb_pop_total <- length(unique(nuseds[cond_sp,]$streamid))
    proportion <- count / nb_pop_total
    
    # proportion CUs assessed
    nb_CU_total <- length(unique(nuseds[cond_sp,]$cuid))
    proportion_CU <- sapply(years,function(y){
      cond <- nuseds$SPECIES == sp & nuseds$Year == y
      out <- length(unique(nuseds$cuid[cond]))
      return(out / nb_CU_total)
    })
  }
  
  dataExportHere <- data.frame(species = sp, 
                               year = years,
                               count = count,
                               proportion = proportion,
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

dataExport <- NULL
for(rg in regions){
  # rg <- regions[2]
  # rg <- "Central Coast"
  cond_rg <- nuseds$region_survey == rg
  species <- nuseds$SPECIES[cond_rg] |> unique()
  
  for(sp in species){
    # sp <- species[4]
    # sp <- "Pink"
    cond_rg_sp <- nuseds$region_survey == rg & nuseds$SPECIES == sp
    
    # Note that what dealing with CUs we use region and not region_survey
    cond_rg_sp_CUs <- nuseds$region == rg & nuseds$SPECIES == sp
    
    # count
    if(sp == "Pink"){
      
      cond_rg_sp_odd <- cond_rg_sp & grepl("odd",nuseds$cu_name_pse)
      cond_rg_sp_even <- cond_rg_sp & grepl("even",nuseds$cu_name_pse)
      
      count_odd <- sapply(years_odd, function(y){
        # y <- dataExport$year[50]
        cond <- nuseds$region_survey == rg & nuseds$SPECIES == sp & nuseds$Year == y &
          grepl("odd",nuseds$cu_name_pse)
        out <- length(nuseds$streamid[cond])
        return(out)
      })
      count_even <- sapply(years_even, function(y){
        # y <- dataExport$year[50]
        cond <- nuseds$region_survey == rg & nuseds$SPECIES == sp & nuseds$Year == y &
          grepl("even",nuseds$cu_name_pse)
        out <- length(nuseds$streamid[cond])
        return(out)
      })
      
      # proportion populations assessed
      nb_pop_total_odd <- length(unique(nuseds[cond_rg_sp_odd,]$streamid))
      proportion_odd <- count_odd / nb_pop_total_odd
      nb_pop_total_even <- length(unique(nuseds[cond_rg_sp_even,]$streamid))
      proportion_even <- count_even / nb_pop_total_even
      
      #---
      # proportion CUs assessed
      #---
      # Note that what dealing with CUs we use region and not region_survey
      
      cond_rg_sp_CUs_odd <- cond_rg_sp_CUs & grepl("odd",nuseds$cu_name_pse)
      cond_rg_sp_CUs_even <- cond_rg_sp_CUs & grepl("even",nuseds$cu_name_pse)
      
      nb_CU_total_odd <- length(unique(nuseds$cuid[cond_rg_sp_CUs_odd]))
      proportion_CU_odd <- sapply(years_odd,function(y){
        cond <- nuseds$region == rg & nuseds$SPECIES == sp & nuseds$Year == y &
          grepl("odd",nuseds$cu_name_pse)
        out <- length(unique(nuseds$cuid[cond]))
        return(out / nb_CU_total_odd)
      })
      
      nb_CU_total_even <- length(unique(nuseds$cuid[cond_rg_sp_CUs_even]))
      proportion_CU_even <- sapply(years_even,function(y){
        cond <- nuseds$region == rg & nuseds$SPECIES == sp & nuseds$Year == y &
          grepl("even",nuseds$cu_name_pse)
        out <- length(unique(nuseds$cuid[cond]))
        return(out / nb_CU_total_even)
      })
      
      data_pink <- data.frame(year = c(years_odd,years_even), 
                              count = c(count_odd,count_even), 
                              proportion = c(proportion_odd,proportion_even),
                              proportion_CU = c(proportion_CU_odd,proportion_CU_even))
      
      data_pink <- data_pink[order(data_pink$year),]
      
      count <- data_pink$count
      proportion <- data_pink$proportion
      proportion_CU <- data_pink$proportion_CU
      
    }else{ # not Pink salmon
      
      count <- sapply(years, function(y){
        # y <- dataExport$year[50]
        cond <- nuseds$region_survey == rg & nuseds$SPECIES == sp & nuseds$Year == y
        out <- length(nuseds$streamid[cond])
        if(out != length(unique(nuseds$streamid[cond]))){
          print("Duplicated streamid TO INVESTIGATE")
        }
        return(out)
      })
      
      # proportion populations assessed
      nb_pop_total <- length(unique(nuseds[cond_rg_sp,]$streamid))
      proportion <- count / nb_pop_total
      
      # proportion CUs assessed
      nb_CU_total <- length(unique(nuseds[cond_rg_sp_CUs,]$cuid))
      proportion_CU <- sapply(years,function(y){
        cond <- nuseds$region == rg & nuseds$SPECIES == sp & nuseds$Year == y # Not region-survey!!!
        out <- length(unique(nuseds$cuid[cond]))
        return(out / nb_CU_total)
      })
      
    }
    
    dataExportHere <- data.frame(region = rg, 
                                 species = sp,
                                 year = years,
                                 count = count,
                                 proportion = proportion,
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

#'* Catches per species and total *

# To long format
dataExport <- NULL
for(sp in unique(catch$Species)){
  cond_sp <- catch$Species == sp
  count <- catch[cond_sp,col_yr,drop = T] |> unlist()
  count <- count * 1000
  dataExportHere <- data.frame(species = sp,
                               year = as.numeric(col_yr),
                               count = count)
  if(is.null(dataExport)){
    dataExport <- dataExportHere
  }else{
    dataExport <- rbind(dataExport,dataExportHere)
  }
}

Number_catches_species_total <- dataExport


#'* Summary table for each regions *

# Define the order of the regions
regions <- c("Yukon","Transboundary","Haida Gwaii","Nass","Skeena","Central Coast",
             "Vancouver Island & Mainland Inlets","Fraser","Columbia")

summary <- data.frame(region = regions) 

summary$CU_nb <- sapply(summary$region,function(rg){
  cond <- nuseds$region == rg
  return(length(unique(nuseds$cuid[cond])))
})

summary$pop_nb <- sapply(summary$region,function(rg){
  cond <- nuseds$region_survey == rg # Changed to region_survey
  return(length(unique(nuseds$streamid[cond])))
})

summary$site_nb <- sapply(summary$region,function(rg){
  cond <- nuseds$region_survey == rg
  return(nrow(unique(nuseds[cond,c("SYSTEM_SITE","X_LONGT","Y_LAT")])))
})

# Proportion of populations in the last decade with estimates (2013-2022)
summary$prop_pop_recent <- sapply(summary$region, function(rg){
  cond_rg <- nuseds$region_survey == rg
  cond_2013 <- nuseds$Year >= 2013
  out <- length(unique(nuseds$streamid[cond_rg & cond_2013]))/length(unique(nuseds$streamid[cond_rg]))
  return(round(out,3))
})

# Proportion of populations in the peak (1980-1989) with estimates
summary$prop_pop_80s <- sapply(summary$region, function(rg){
  cond_rg <- nuseds$region_survey == rg
  cond_80s <- nuseds$Year %in% c(1980:1989)
  out <- length(unique(nuseds$streamid[cond_rg & cond_80s]))/length(unique(nuseds$streamid[cond_rg]))
  return(round(out, 3))
})

sum_sum <- colSums(summary[,c("CU_nb","pop_nb","site_nb")])
summary <- rbind(summary,
                 c("Overall", sum_sum, 
                   round(length(unique(nuseds$streamid[nuseds$Year %in% c(2013:2022)]))/length(unique(nuseds$streamid)), 3), # over all regions
                   round(length(unique(nuseds$streamid[nuseds$Year %in% c(1980:1989)]))/length(unique(nuseds$streamid)), 3) # over all regions
                 ))

#
# Export excel file -------
#
files_l <- list(Number_Prop_populationsAssessed_total,
                Number_Prop_populationsAssessed_regions,
                Number_Prop_populationsAssessed_species,
                Number_Prop_populationsAssessed_regions_species,
                Number_catches_species_total,
                summary)

names(files_l) <- c("populations_total",
                    "populations_regions",
                    "populations_species",
                    "populations_regions_species",
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



