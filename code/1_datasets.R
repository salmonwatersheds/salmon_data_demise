

#'******************************************************************************
#' The goal of the script is to 
#' 
#' Files imported:
#' - nuseds_cuid_streamid_20240419.csv
#' 
#' Files produced: 
#' - Number_Prop_populationsAssessed_total.csv
#' - Number_Prop_populationsAssessed_regions.csv
#' - Number_Prop_populationsAssessed_species.csv
#' - Number_Prop_populationsAssessed_regions_species.csv
#' 
#'******************************************************************************

rm(list = ls())
graphics.off()

wd_figures <- paste0(getwd(),"/figures")
wd_data_input <- paste0(getwd(),"/data_input")
wd_data_output <- paste0(getwd(),"/data_output")

library(tidyr)
library(dplyr)
# library(xlsx) # Package doesn't work..on mac?
library(readxl)
source("code/functions.R")

# **** TEMPORARY (i.e. to define a reproduceable universal workflow) ***
# Bruno Dropbox
wd_data_input_PSF <- "C:/Users/bcarturan/Salmon Watersheds Dropbox/Bruno Carturan/X Drive/1_PROJECTS/1_Active/Population Methods and Analysis/population-indicators/spawner-surveys/output"

# Steph Dropbox
wd_data_input_PSF <- "/Users/stephaniepeacock/Salmon\ Watersheds\ Dropbox/Stephanie\ Peacock/X\ Drive/1_PROJECTS/1_Active/Population\ Methods\ and\ Analysis/population-indicators/spawner-surveys/output"

# Import source files ------

#'* Import the cleaned NuSEDS data matched with PSF cuid and streamid *
#' Note: the choice was made to not use the cleaned and combined nuseds file 
#' NuSEDS_escapement_data_collated_DATE.csv because it does not contain the region.
#' Providing the region requires to match cuid which is hard and was done to 
#' produce nuseds_cuid_streamid_DATE.csv, which is the file that is used instead.
#' It contains extra fields not needed.
nuseds <- import_mostRecent_file_fun(wd = paste0(wd_data_input_PSF,"/archive"),
                                     pattern = "nuseds_cuid_streamid")
head(nuseds)

#' IMPORTANT NOTE: streamid = unique cuid & GFE_ID combination
#' The name 'streamid' is miss-leading as it seems to characterise a stream only
#' but it characterise a unique CU & location association.

# Remove rows with NAs
sum(is.na(nuseds$MAX_ESTIMATE)) # 155984
nuseds <- nuseds[!is.na(nuseds$MAX_ESTIMATE),]

colToKeep <- c("region","SPECIES","cu_name_pse","cuid","POP_ID","cu_name_dfo","CU_NAME",
               "SYSTEM_SITE","GFE_ID","X_LONGT","Y_LAT","streamid",
               "Year","MAX_ESTIMATE",
               "ESTIMATE_CLASSIFICATION","ESTIMATE_METHOD","stream_survey_quality")

nuseds <- nuseds[,colToKeep] 

# Some checks
sum(is.na(nuseds$streamid)) # 1011
sum(is.na(nuseds$cuid))     # 1011
sum(is.na(nuseds$cuid)) / nrow(nuseds) # 0.006702511
sum(is.na(nuseds$POP_ID))   # 0
sum(is.na(nuseds$GFE_ID))   # 0

length(unique(nuseds$streamid)) # 6767
length(unique(nuseds$POP_ID))   # 6009

# Check CU_NAMEs without a cuid
cond <- is.na(nuseds$cuid)
CU_NAME_noCuid <- nuseds[cond,c("region","SPECIES","CU_NAME")] |> unique()
CU_NAME_noCuid 
length(CU_NAME_noCuid$CU_NAME) # 22 of them

# remove them
cond <- !is.na(nuseds$cuid)
nuseds <- nuseds[cond,]
nrow(nuseds) # 149828
sum(is.na(nuseds$streamid)) # 0
sum(is.na(nuseds$GFE_ID)) # 0

length(unique(nuseds$streamid)) # 6766
length(unique(nuseds$GFE_ID))   # 2300
unique(nuseds$region)

# check if there are duplicated streamid
data_check <- nuseds[cond,c("cuid","GFE_ID","streamid")] |> unique()
any(duplicated(data_check$streamid)) # FALSE

#'* Import the definition of the different fields of these two datasets *
fields_def <- nuseds_fields_definitions_fun(wd_references = wd_data_input)

nuseds$region |> unique() |> length()
nuseds$cuid |> unique() |> length()    # 389
nuseds$streamid |> unique() |> length()    # 6766

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

# write.csv(Number_Prop_populationsAssessed_total,
#           paste0(wd_data_output,"/Number_Prop_populationsAssessed_total.csv"),
#           row.names = F)

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
regions <- nuseds$region |> unique()

dataExport <- NULL
for(rg in regions){
  # rg <- regions[5]
  cond_rg <- nuseds$region == rg
  
  cond_odd <- nuseds$Year %in% years_odd
  cond_even <- nuseds$Year %in% years_even
  
  # count
  count_odd <- sapply(years_odd, function(y){
    # y <- dataExport$year[50]
    cond <- nuseds$region == rg & nuseds$Year == y
    out <- length(nuseds$streamid[cond])
    return(out)
  })
  
  count_even <- sapply(years_even, function(y){
    # y <- dataExport$year[50]
    cond <- nuseds$region == rg & nuseds$Year == y
    out <- length(nuseds$streamid[cond])
    return(out)
  })
  
  # proportion populations assessed
  nb_pop_total_odd <- length(unique(nuseds[cond_rg & cond_odd,]$streamid))
  proportion_odd <- count_odd / nb_pop_total_odd
  nb_pop_total_even <- length(unique(nuseds[cond_rg & cond_even,]$streamid))
  proportion_even <- count_even / nb_pop_total_even
  
  # proportion CUs assessed
  nb_CU_total_odd <- length(unique(nuseds[cond_rg & cond_odd,]$cuid))
  proportion_CU_odd <- sapply(years_odd,function(y){
    cond <- nuseds$region == rg & nuseds$Year == y
    out <- length(unique(nuseds$cuid[cond]))
    return(out / nb_CU_total_odd)
  })
  nb_CU_total_even <- length(unique(nuseds[cond_rg & cond_even,]$cuid))
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

# write.csv(Number_Prop_populationsAssessed_regions,
#           paste0(wd_data_output,"/Number_Prop_populationsAssessed_regions.csv"),
#           row.names = F)


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

# write.csv(Number_Prop_populationsAssessed_species,
#           paste0(wd_data_output,"/Number_Prop_populationsAssessed_species.csv"),
#           row.names = F)


#'* Count of surveys per region > species *
regions <- nuseds$region |> unique()

dataExport <- NULL
for(rg in regions){
  # rg <- regions[1]
  # rg <- "Central Coast"
  cond_rg <- nuseds$region == rg
  species <- nuseds$SPECIES[cond_rg] |> unique()
  
  for(sp in species){
    # sp <- species[1]
    # sp <- "Pink"
    cond_rg_sp <- nuseds$region == rg & nuseds$SPECIES == sp
    
    # count
    if(sp == "Pink"){
      
      cond_rg_sp_odd <- cond_rg_sp & grepl("odd",nuseds$cu_name_pse)
      cond_rg_sp_even <- cond_rg_sp & grepl("even",nuseds$cu_name_pse)
      
      count_odd <- sapply(years_odd, function(y){
        # y <- dataExport$year[50]
        cond <- nuseds$region == rg & nuseds$SPECIES == sp & nuseds$Year == y &
          grepl("odd",nuseds$cu_name_pse)
        out <- length(nuseds$streamid[cond])
        return(out)
      })
      count_even <- sapply(years_even, function(y){
        # y <- dataExport$year[50]
        cond <- nuseds$region == rg & nuseds$SPECIES == sp & nuseds$Year == y &
          grepl("even",nuseds$cu_name_pse)
        out <- length(nuseds$streamid[cond])
        return(out)
      })
      
      # proportion populations assessed
      nb_pop_total_odd <- length(unique(nuseds[cond_rg_sp_odd,]$streamid))
      proportion_odd <- count_odd / nb_pop_total_odd
      nb_pop_total_even <- length(unique(nuseds[cond_rg_sp_even,]$streamid))
      proportion_even <- count_even / nb_pop_total_even
      
      # proportion CUs assessed
      nb_CU_total_odd <- length(unique(nuseds[cond_rg_sp_odd,]$cuid))
      proportion_CU_odd <- sapply(years_odd,function(y){
        cond <- nuseds$region == rg & nuseds$SPECIES == sp & nuseds$Year == y &
          grepl("odd",nuseds$cu_name_pse)
        out <- length(unique(nuseds$cuid[cond]))
        return(out / nb_CU_total_odd)
      })
      nb_CU_total_even <- length(unique(nuseds[cond_rg_sp_even,]$cuid))
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
        cond <- nuseds$region == rg & nuseds$SPECIES == sp & nuseds$Year == y
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
      nb_CU_total <- length(unique(nuseds[cond_rg_sp,]$cuid))
      proportion_CU <- sapply(years,function(y){
        cond <- nuseds$region == rg & nuseds$SPECIES == sp & nuseds$Year == y
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

# write.csv(Number_Prop_populationsAssessed_regions_species,
#           paste0(wd_data_output,"/Number_Prop_populationsAssessed_regions_species.csv"),
#           row.names = F)


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

# write.csv(Number_Prop_populationsAssessed_regions_species,
#           paste0(wd_data_output,"/Number_catches_species_total.csv"),
#           row.names = F)

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
  cond <- nuseds$region == rg
  return(length(unique(nuseds$streamid[cond])))
})

summary$site_nb <- sapply(summary$region,function(rg){
  cond <- nuseds$region == rg
  return(nrow(unique(nuseds[cond,c("SYSTEM_SITE","X_LONGT","Y_LAT")])))
})

# Proportion of populations in the last decade with estimates (2013-2022)
summary$prop_pop_recent <- sapply(summary$region, function(rg){
  cond <- nuseds$region == rg
  return(round(length(unique(nuseds$streamid[nuseds$region == rg & nuseds$Year >= 2013]))/length(unique(nuseds$streamid[cond])), 3))
})

# Proportion of populations in the peak (1980-1989) with estimates
summary$prop_pop_80s <- sapply(summary$region, function(rg){
  cond <- nuseds$region == rg
  return(round(length(unique(nuseds$streamid[nuseds$region == rg & nuseds$Year %in% c(1980:1989)]))/length(unique(nuseds$streamid[cond])), 3))
})

sum_sum <- colSums(summary[,c("CU_nb","pop_nb","site_nb")])
summary <- rbind(summary,
                 c("Overall", sum_sum, 
                   round(length(unique(nuseds$streamid[nuseds$Year %in% c(2013:2022)]))/length(unique(nuseds$streamid)), 3), # over all regions
                   round(length(unique(nuseds$streamid[nuseds$Year %in% c(1980:1989)]))/length(unique(nuseds$streamid)), 3) # over all regions
                 ))

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
             file = paste0(wd_data_output,"/populationAssessed_catches_data.xlsx"),
             sheetName = sheetName, 
             row.names = FALSE,
             append = append,
             showNA = T)
  print(sh_i)
}


###############################################################################
# Reported Statistics
###############################################################################

years <- 1980:1990
populations <- sapply(X = years, function(yr){
  cond_y <- nuseds$Year == yr
  out <- length(unique(nuseds$streamid[cond_y]))
  names(out) <- yr
  return(out)
})
populations
# 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 
# 2307 2322 2310 2315 2340 2652 2745 2459 2436 2440 2441 

streams <- sapply(X = years, function(yr){
  cond_y <- nuseds$Year == yr
  out <- length(unique(nuseds$GFE_ID[cond_y]))
  names(out) <- yr
  return(out)
})
streams
# 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 
# 1192 1224 1208 1187 1215 1356 1369 1294 1298 1341 1306 


#------------------------------------------------------------------------------
# Average loss of populations per yeat since 1986
#------------------------------------------------------------------------------

catch_total <- read_xlsx(paste0(wd_data_output,"/populationAssessed_catches_data.xlsx"),
                         sheet = "populationsAssessed_total") |> as.data.frame()

cond_min <- 1986 <= data_total$year
lm <- lm(data_total$count[cond_min] ~ data_total$year[cond_min])
lm


#------------------------------------------------------------------------------
# Monitoring of indicator stocks
#------------------------------------------------------------------------------

# Indicator systems and associated streamid for the Pacific Salmon Comission's Chinook Technical Committee:
# NWVI indicators:
# Colonial-Cayeagle, 1438
# Tashish, 1542
# Artlish, and 1541
# Kaouk 1540
# SWVI indicators:
# Bedwell-Ursus, 2017
# Megin,  1497
# Moyeha 1494
# Marble (Area 27); 1440
# Leiner, 1518
# Burman 1504
# Tahsis (Area 25); 1519
# Sarita, 1463
# Nahmint (Area 23); 1474
# San Juan (Area 20).1452
# Phillips River 1948
# Cowichan 1443
# Nanaimo (fall) 1978
ctc_indicators <- c(1438, 1542, 1541, 1540, 2017, 1497, 1494, 1440, 1518, 1504, 1519, 1463, 1474, 1452, 1948, 1443, 1978)

# Filter NuSEDS to VIMI Chinook
nuseds_vimiCK <- nuseds %>% filter(region == "Vancouver Island & Mainland Inlets", SPECIES == "Chinook")
length(unique(nuseds_vimiCK$streamid)) # 263
ctc_indicators %in% nuseds_vimiCK$streamid

# Summarize monitoring 
summary_vimiCK <- nuseds_vimiCK %>% 
  group_by(streamid) %>%
  summarise(nYearsData = length(streamid), 
            meanSpawners = round(exp(mean(log(MAX_ESTIMATE), na.rm = TRUE)))) %>%
  arrange(-nYearsData)

# Visualize monitoring of each stream through time (points = monitored, red = indicator)
plot(c(1926,2022), c(1,100), "n", ylab = "Population", xlab = "Year")
for(i in 1:263){
  z <- nuseds_vimiCK %>% filter(streamid == summary_vimiCK$streamid[i])
  points(z$Year, rep(i, length(z$Year)), col = ifelse(summary_vimiCK$streamid[i] %in% ctc_indicators, 2, 1), pch = ifelse(summary_vimiCK$streamid[i] %in% ctc_indicators, 19, 1), cex = 0.5)
}
abline(h = seq(0, 100, 10))
summary_vimiCK$ctc_indicator <- summary_vimiCK$streamid %in% ctc_indicators

summary_vimiCK[1:30,]

# How many indicator streams are in top 30 monitored
sum(summary_vimiCK$ctc_indicator[1:30])

# How many years of data for indicator stocks?
mean(summary_vimiCK$nYearsData[summary_vimiCK$ctc_indicator == TRUE])

# How many streams total?
dim(summary_vimiCK)[1]

# Mean number of years for non indicators
mean(summary_vimiCK$nYearsData[summary_vimiCK$ctc_indicator == FALSE])

# How many non-indicators have been monitored in the past decade?
maxYr <- nuseds_vimiCK %>% filter(streamid %in% ctc_indicators == FALSE) %>%
  group_by(streamid) %>%
  summarise(max(Year))

# What percent have not been monitored in the past decade?
length(which(maxYr$`max(Year)` < 2013))/length(maxYr$`max(Year)`)
