

#' Function to return a list of list of the description of the fields in 
#' all_areas_nuseds and conservation_unit_system_sites.
# wd_references <- wd_references_dropbox
nuseds_fields_definitions_fun <- function(wd_references){
  
  #' Sources of information:
  #' - Data Dictionary NuSEDS: 
  #'    - /references/nuseds_report_definitions.csv
  #'    - https://open.canada.ca/data/en/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6/resource/60c2827f-c439-3b37-ab84-42515eb1b521
  #'    - 
  #' Conservation_Unit_Report_Definitions:
  #' - /references/conservation_unit_report_definitions.csv
  #' - https://open.canada.ca/data/en/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6/resource/894ba9df-8931-3cf1-bf4c-ba8876bcf515
  
  nuseds_report_def <- read.csv(paste(wd_references,"nuseds_report_definitions.csv",
                                      sep = "/"))
  conservation_unit_report_def <- read.csv(paste(wd_references,"conservation_unit_report_definitions.csv",
                                                 sep = "/"))
  
  nuseds_report_def_l <- list()
  for(r in 1:nrow(nuseds_report_def)){
    nuseds_report_def_l[[r]] <- nuseds_report_def$Field.Definition[r]
  }
  names(nuseds_report_def_l) <- nuseds_report_def$Field.Name
  
  conservation_unit_report_def_l <- list()
  for(r in 1:nrow(conservation_unit_report_def)){
    conservation_unit_report_def_l[[r]] <- conservation_unit_report_def$Field.Description[r]
  }
  names(conservation_unit_report_def_l) <- conservation_unit_report_def$Field.Name
  
  fields_def <- list(nuseds_report_def_l,conservation_unit_report_def_l)
  names(fields_def) <- c("all_areas_nuseds","cu_system_sites")
  return(fields_def)
}

#' Function to plot the count of a given variable/field in a dataset for each 
#' values in column "Year".
# dataset <- nuseds
# varCount <- "SPECIES" # "region"
# varX <- "Year"
plot_countsYear_oneVar_fun <- function(dataset, varCount, varX, figure_print = F, 
                                   width = 15, height = 10, coef = 1, 
                                   units = 'cm', res = 300, lwd = 2, las = 0,
                                   ylab = "Count", xlab = "xlab", main = "",
                                   side1 = 4.5, side2 = 4.5, side3 = 3, side4 = .5,
                                   xaxt = "s", yaxt = "s", 
                                   add_linesVertical = T, lwd_vertical = 1,
                                   colours = NA, y_max = NA, x_max = NA, x_min = NA,
                                   wd_figures, figure_name = NA, 
                                   legend_imposed = T, # the legend is imposed by the vector colours
                                   legend_show = T){

  varCount_values <- unique(dataset[,varCount])
  varCount_values[is.na(varCount_values)] <- "NA"
  varX_val <- unique(dataset[,varX])
  varX_val <- varX_val[order(varX_val)]
  
  varXClass <- class(dataset[,varX])
  
  # find the maximum varCount 
  count_max <- c()
  dataset_varCount_l <- list()
  for(i in 1:length(varCount_values)){
    v <- varCount_values[i]
    if(v == 'NA'){
      cond <- is.na(dataset[,varCount])
      dataset_cut <- dataset[cond,]
      dataset_cut[,varCount] <- "NA"
    }else{
      cond <- dataset[,varCount] == v
      dataset_cut <- dataset[cond,]
    }
    
    dataset_cut[,varX] <- factor(dataset_cut[,varX], levels = varX_val)
    dataset_cut_varCount <- tapply(X = dataset_cut, INDEX = dataset_cut[,varX] ,
                                     FUN = count) |>
      unlist()
    
    dataset_cut_varCount <- data.frame(var1 = names(dataset_cut_varCount),
                                         count = dataset_cut_varCount, 
                                         row.names = NULL)
    
    colnames(dataset_cut_varCount)[1] <- varX
    
    class(dataset_cut_varCount[,1]) <- varXClass
    
    count_max <- c(count_max,max(dataset_cut_varCount$count))
    #names(count_max)[i] <- v
    
    dataset_varCount_l[[i]] <- dataset_cut_varCount
  }
  names(count_max) <- names(dataset_varCount_l) <- varCount_values
  
  # order varCount_values by abundance
  varCount_values <- varCount_values[rev(order(count_max))]
  dataset_varCount_l <- dataset_varCount_l[varCount_values]
  
  if(is.na(y_max)[1]){
    y_max <- max(count_max)
  }
  if(is.na(x_max)[1]){
    x_max <- max(dataset[,varX])
  }
  if(is.na(x_min)[1]){
    x_min <- min(dataset[,varX])
  }
  
  if(is.na(figure_name)){
    figure_name <- paste0("figure_",varCount,".jpeg")
  }
  
  if(figure_print){
    jpeg(paste(wd_figure,figure_name,sep = "/"),
         width = width * coef, height = height * coef, units = units, res = res)
  }
  par(mar = c(side1,side2,side3,side4))
  plot(NA, xlim = c(x_min,x_max), ylim = c(0,y_max), las = las,
       ylab = ylab, xlab = xlab, main = main, xaxt = xaxt, yaxt = yaxt)
  
  if(add_linesVertical){
    xs <- x_min:x_max
    segments(x0 = xs[xs %% 10 == 0], x1 = xs[xs %% 10 == 0], 
             y0 = 0, y1 = y_max, 
             col = "grey70", lwd = lwd_vertical)
  }
  
  if(is.na(colours)[1]){
    colours <- rainbow(n = length(varCount_values))
    names(colours) <- varCount_values
  }
  
  for(i in 1:length(varCount_values)){
    v <- varCount_values[i]
    lines(x = dataset_varCount_l[[v]][,varX], y = dataset_varCount_l[[v]]$count, 
          pch = 16, col = colours[v], lwd = lwd)
    # points(x = nuseds_cut_surveyYear$Year, y = nuseds_cut_surveyYear$count,
    #        pch = 16, col = colours[i])
  }
  if(legend_show){
    if(legend_imposed & !is.na(colours)[1]){
      legend("topleft",names(colours), col = colours, lwd = lwd, bty = "n")
    }else{
      legend("topleft",varCount_values, col = colours, lwd = lwd, bty = "n")
    }
  }
  if(figure_print){
    dev.off()
  }
}

#' Function to compute the euclidean distance between a reference point
#' and points whose coordinates are provided 
distance_Euclidean_fun <- function(x_ref,y_ref,x,y){
  x_ref <- as.numeric(x_ref)
  y_ref <- as.numeric(y_ref)
  x <- as.numeric(x)
  y <- as.numeric(y)
  out <- sqrt((x_ref - x)^2 + (y_ref - y)^2)
  return(out)
}

#' Function to return the last version of a file whose name contains the given
#' pattern. Works with .csv and .xlsx formats.
import_mostRecent_file_fun <- function(wd,pattern,pattern_exclude = NA,second_last = F){
  
  files_c <- list.files(wd)
  files_c <- files_c[grepl(x = files_c, 
                           pattern = pattern)]
  
  if(!is.na(pattern_exclude[1])){
    for(p in pattern_exclude){
      files_c <- files_c[!grepl(x = files_c, 
                                pattern = p)]
    }
  }
  
  if(length(files_c) == 0){
    print("File not found.")
    out <- NA
  }else{
    file.mtime <- file.mtime(paste(wd,files_c,sep="/"))
    
    if(second_last){ # to select the second last file
      time_secondLast <- rev(sort(file.mtime))[2]
      file <- files_c[file.mtime == time_secondLast]
      print(paste0("Second last file imported: ",file," ; Date modified: ",time_secondLast))
      file_last <- files_c[file.mtime == max(file.mtime)]
      print(paste0("Last file is: ",file_last," ; Date modified: ", max(file.mtime)))
      
    }else{
      file <- files_c[file.mtime == max(file.mtime)]
      print(paste0("File imported: ",file," ; Date modified: ", max(file.mtime)))
    }
    
    if(grepl(".xlsx",file)){
      require(readxl)
      sheets_n <- excel_sheets(paste(wd,file,sep = "/"))
      out <- list()
      for(s in sheets_n){
        out[[which(s == sheets_n)]] <- read_excel(path = paste(wd,file,sep = "/"),
                                                  sheet = s)
      }
      names(out) <- sheets_n
      
      if(length(out) == 1){ # return a data frame instead of a list if there is only one sheet
        out <- out[[1]]
      }
      
    }else if(grepl(".csv",file)){
      out <- read.csv(paste(wd,file,sep = "/"),header = T)
      
    }else{
      Print("File format to implement.")
    }
  }
  return(out)
}

#' Function used to lower case characters and remove special characters from 
#' strings. It is used to strings of characters.
simplify_string_fun <- function(string){
  
  string <- tolower(string)
  string <- gsub(" ","",string)
  string <- gsub("-","",string)
  string <- gsub("_","",string)
  string <- gsub("'","",string)
  string <- gsub("\\\\","",string)
  string <- gsub("\\(","",string)
  string <- gsub(")","",string)
  string <- gsub("\\[","",string)
  string <- gsub("]","",string)
  string <- gsub("\\.","",string)
  string <- gsub("/","",string)
  
  return(string)
}

# Function taking a vector of colours names and return a vector of the same colours
# but with more transparency added
# colours <- colours_rg
colour_transparency_fun <- function(colours,alpha = 0.35){
  col.rgb <- col2rgb(colours)
  colnames(col.rgb) <- colours
  output <- c()
  for(i in 1:length(col.rgb[1,])){
    output[i] <- rgb(red = col.rgb[1,i],
                     green = col.rgb[2,i],
                     blue = col.rgb[3,i], 
                     maxColorValue = 255,
                     alpha = alpha*255)
  }
  return(output)
}


#' Function to create horizontal segments in a figure that matched the ticks.
segments_horizontal_fun <- function(y_range, x_range,
                                    nb_lines = 5,       # if length(grid_vals) < nb_lines--> add lines in between each values in grid_vals
                                    colour = "grey50", alpha = 1, lwd = 2, lty = 1){
  
  divide_10 <- F
  if(nchar(ceiling(y_range[2])) == 1){
    y_range[2] <- y_range[2] * 10 
    divide_10 <- T
  }
  
  digits_nb <- nchar(ceiling(y_range[2]))
  grid_max <- substr(x = ceiling(y_range[2]), start = 1, stop = 1)
  factor <- paste(c(1,rep(0,(digits_nb - 1))), collapse = "") |> as.numeric()
  grid_vals <- 0:as.numeric(grid_max) * factor
  if(length(grid_vals) < nb_lines){ # add a segment to each 1/2 intervals as well
    grid_max <- substr(x = ceiling(y_range[2]), start = 1, stop = 2)
    grid_vals <- 0:as.numeric(grid_max) 
    grid_vals <- grid_vals[grid_vals %% 5 == 0]
    grid_vals <- grid_vals * factor / 10
  }
  if(length(grid_vals) < nb_lines){ # add a segment to each 1/5 intervals as well
    grid_vals <- 0:as.numeric(grid_max) 
    grid_vals <- grid_vals[grid_vals %% 2 == 0]
    grid_vals <- grid_vals * factor / 10
  }
  
  if(divide_10){
    grid_vals <- grid_vals / 10 
  }
  
  segments(x0 = rep(x_range[1],length(grid_vals)),
           x1 = rep(x_range[2],length(grid_vals)),
           y0 = grid_vals, y1 = grid_vals,
           lwd = lwd, lty = lty,
           col = colour_transparency_fun(colour,alpha = alpha))
}


# calculate the McFadden pseudo R squared
# if the model is an average model, the weighted mean (using the weights used for the model averaging procedure, 
# such as wAICc) of the pseudo R2 for each model is returned
R2.McFadden.fun <- function(model,model.null){
  require(diagis)
  # http://www.glmj.org/archives/articles/Smith_v39n2.pdf
  # https://web.archive.org/web/20130701052120/http://www.ats.ucla.edu:80/stat/mult_pkg/faq/general/Psuedo_RSquareds.htm
  # https://stats.stackexchange.com/questions/82105/mcfaddens-pseudo-r2-interpretation
  LL <- as.numeric(logLik(object = model))
  LL.n <- as.numeric(logLik(object = model.null))
  R.MF <- 1 - LL / LL.n
  output <- R.MF
  if(length(R.MF) > 1){ # for averaged models
    output <- data.frame(w.mean=NA, w.SE= NA, nb.models= NA)
    output[,1] <- sum(R.MF * model$msTable$weight)
    output[,2] <- weighted_se(x = R.MF, w =  model$msTable$weight)
    output[,3]<- nrow(model$msTable)
  }
  
  return(output)
}


AIC_R2_glm_fun <- function(list_glm,glm_null){
  
  if(is.null(names(list_glm))){
    print("The list of models should be named; names are given but are not descriptive")
    m_names <- paste0("model_",1:length(list_glm))
  }else{
    m_names <- names(list_glm)
  }
  
  out <- data.frame(model = m_names)
  
  out$AIC <- lapply(list_glm,function(m){
    return(AIC(m))
  }) |> unlist()
  
  out$AICc <- lapply(list_glm,function(m){
    return(AICc(m))
  }) |> unlist()
  
  out$R2_McFadden <- lapply(list_glm,function(m){
    out <- R2.McFadden.fun(model = m, model.null = glm_null)
    return(round(out,3))
  }) |> unlist()
  
  out$R2_pseudo <- lapply(list_glm,function(m){
    sm <- summary(m)
    out <- (sm$null.deviance - sm$deviance)/sm$null.deviance
    return(round(out,3))
  }) |> unlist()
  
  out <- out[order(out$AIC),]
  rownames(out) <- NULL
  return(out)
}

#' Function to return a list for the fields in all_areas_nuseds and 
#' conservation_unit_system_sites that are associated to unique IndexId and GFE_ID,
#' to both and to none. The assumption is based on if single or multiple values
#' of a given field is returned for each IndexId or GDE_ID.
fields_IndexId_GFE_ID_fun <- function(all_areas_nuseds = NA,
                                      conservation_unit_system_sites,
                                      runProcess = F,                # takes a while
                                      newFieldsIncluded = T){
  
  #' the commented out code was used to obtained the vectors of fields. The procedure
  #' take a bit of time so instead those vectors were copy pasted.
  
  #' *** conservation_unit_system_sites ***
  CUSS_l <- list()
  
  # IndexId
  if(runProcess){
    fields_CUSS_asso <- association_twoFields_fun(fields_1 = c("IndexId","GFE_ID"),
                                                  fields_2 = colnames(conservation_unit_system_sites),
                                                  dataset = conservation_unit_system_sites,
                                                  silence = T)
    
    cond_iid <- fields_CUSS_asso$fields_1 == "IndexId" &
      fields_CUSS_asso$association == "single"
    out <- as.character(fields_CUSS_asso$fields_2[cond_iid])
    
  }else{
    out <- c('SPECIES_QUALIFIED','FAZ_ACRO','MAZ_ACRO','JAZ_ACRO',
             'CU_NAME','CU_ACRO','CU_LAT','CU_LONGT','CU_TYPE','CU_INDEX','FULL_CU_IN',
             'SBJ_ID','POP_ID','SPECIES',"IS_INDICATOR","CMNTS","EFFECTIVE_DT")
    
    if(newFieldsIncluded){
      out <- c(out,'species_acronym_ncc','IndexId')
      out <- unique(out)
    }
  }
  CUSS_l[[1]] <- out
  
  
  # GFE_ID
  if(runProcess){
    cond_gfeid <- fields_CUSS_asso$fields_1 == "GFE_ID" &
      fields_CUSS_asso$association == "single"
    fields_CUSS_asso[cond_gfeid,]
    out <- as.character(fields_CUSS_asso$fields_2[cond_gfeid])
    
    # remove fields that are known to be CU related only
    out <- out[!out %in% c('FAZ_ACRO','MAZ_ACRO','JAZ_ACRO')]
    
  }else{
    out <- c('GFE_ID','SYSTEM_SITE','GFE_TYPE','Y_LAT','X_LONGT','WATERSHED_CDE',
             'FWA_WATERSHED_CDE')
    
    if("coordinates_changed" %in% colnames(conservation_unit_system_sites)){
      out <- c(out,"coordinates_changed")
    }
  }
  CUSS_l[[2]] <- out
  
  # both
  if(runProcess){
    out <- c()
    for(f in  unique(fields_CUSS_asso$fields_2)){
      cond1 <- fields_CUSS_asso[fields_CUSS_asso$fields_1 == "IndexId" &
                                  fields_CUSS_asso$fields_2 == f,]$association == "single"
      cond2 <- fields_CUSS_asso[fields_CUSS_asso$fields_1 == "GFE_ID" &
                                  fields_CUSS_asso$fields_2 == f,]$association == "single"
      if(all(c(cond1,cond2))){
        out <- c(out,f)
      }
    }
    out <- out
    
    # remove fields that are known to be CU related only
    out <- out[!out %in% c('FAZ_ACRO','MAZ_ACRO','JAZ_ACRO')]
    
  }else{
    out <- c()
  }
  CUSS_l[[3]] <- out
  
  # none
  if(runProcess){
    out <- c()
    for(f in  unique(fields_CUSS_asso$fields_2)){
      cond1 <- fields_CUSS_asso[fields_CUSS_asso$fields_1 == "IndexId" &
                                  fields_CUSS_asso$fields_2 == f,]$association == "single"
      cond2 <- fields_CUSS_asso[fields_CUSS_asso$fields_1 == "GFE_ID" &
                                  fields_CUSS_asso$fields_2 == f,]$association == "single"
      if(all(c(!cond1,!cond2))){
        out <- c(out,f)
      }
    }
    
  }else{
    out <- c('MAP_LABEL')
  }
  CUSS_l[[4]] <- out
  
  # CU_NAME & species_acronym_ncc
  # if(runProcess & newFieldsIncluded){ # species_acronym_ncc is a new field
  #   
  #   newVar <- apply(conservation_unit_system_sites[,c("species_acronym_ncc","CU_NAME")],
  #                   1,paste, collapse = " ")
  #   
  #   CUSS_new <- conservation_unit_system_sites
  #   CUSS_new$species_CU_NAME <- newVar
  #   
  #   fields_CUSS_asso <- association_twoFields_fun(fields_1 = c("species_CU_NAME"),
  #                                                 fields_2 = colnames(CUSS_new),
  #                                                 dataset = CUSS_new,
  #                                                 silence = T)
  #   out <- c()
  #   for(f in  unique(fields_CUSS_asso$fields_2)){
  #     cond1 <- fields_CUSS_asso[fields_CUSS_asso$fields_1 == "species_CU_NAME" &
  #                                 fields_CUSS_asso$fields_2 == f,]$association == "single"
  #     cond2 <- fields_CUSS_asso[fields_CUSS_asso$fields_1 == "species_CU_NAME" &
  #                                 fields_CUSS_asso$fields_2 == f,]$association == "single"
  #     if(all(c(cond1,cond2))){
  #       out <- c(out,f)
  #     }
  #   }
  #   
  # }else{
  #   out <- c('SPECIES_QUALIFIED','CU_NAME','CU_ACRO','CU_LAT','CU_LONGT','CU_TYPE',
  #            'CU_INDEX','FULL_CU_IN','SBJ_ID','SPECIES')
  #   
  #   if(newFieldsIncluded){
  #     out <- c(out,"species_acronym_ncc")
  #   }
  # }
  # 
  # CUSS_l[[5]] <- out
  
  #
  names(CUSS_l) <- c('IndexId','GFE_ID','both','none')
  
  
  #' *** all_areas_nuseds ***
  NUSEDS_l <- list()
  if(runProcess){
    fields_NUSEDS_asso <- association_twoFields_fun(fields_1 = c("IndexId","GFE_ID"),
                                                    fields_2 = colnames(all_areas_nuseds),
                                                    dataset = all_areas_nuseds,
                                                    silence = T)
    
    # IndexId
    cond_iid <- fields_NUSEDS_asso$fields_1 == "IndexId" &
      fields_NUSEDS_asso$association == "single"
    out <- as.character(fields_NUSEDS_asso$fields_2[cond_iid])
    
  }else{
    out <- c("SPECIES","POPULATION","RUN_TYPE","POP_ID")
    
    if(newFieldsIncluded){
      out <- c(out,'species_acronym_ncc','IndexId')
      out <- unique(out)
    }
  }
  NUSEDS_l[[1]] <- out
  
  # GFE_ID
  if(runProcess){
    cond_gfeid <- fields_NUSEDS_asso$fields_1 == "GFE_ID" &
      fields_NUSEDS_asso$association == "single"
    fields_NUSEDS_asso[cond_gfeid,]
    out <- as.character(fields_NUSEDS_asso$fields_2[cond_gfeid])
    
  }else{
    out <- c('AREA','WATERBODY','GAZETTED_NAME','LOCAL_NAME_1','LOCAL_NAME_2',
             'WATERSHED_CDE','WATERBODY_ID','GFE_ID')
    
    if(newFieldsIncluded){
      # out <- c(out,'StatArea') # field not create in script 1 anymore
      out <- unique(out)
    }
  }
  NUSEDS_l[[2]] <- out
  
  # both
  if(runProcess){
    out <- c()
    for(f in  unique(fields_NUSEDS_asso$fields_2)){
      cond1 <- fields_NUSEDS_asso[fields_NUSEDS_asso$fields_1 == "IndexId" &
                                    fields_NUSEDS_asso$fields_2 == f,]$association == "single"
      cond2 <- fields_NUSEDS_asso[fields_NUSEDS_asso$fields_1 == "GFE_ID" &
                                    fields_NUSEDS_asso$fields_2 == f,]$association == "single"
      if(all(c(cond1,cond2))){
        out <- c(out,f)
      }
    }
    
  }else{
    out <- c()
    
  }
  NUSEDS_l[[3]] <- out
  
  # none
  if(runProcess){
    out <- c()
    for(f in  unique(fields_NUSEDS_asso$fields_2)){
      cond1 <- fields_NUSEDS_asso[fields_NUSEDS_asso$fields_1 == "IndexId" &
                                    fields_NUSEDS_asso$fields_2 == f,]$association == "single"
      cond2 <- fields_NUSEDS_asso[fields_NUSEDS_asso$fields_1 == "GFE_ID" &
                                    fields_NUSEDS_asso$fields_2 == f,]$association == "single"
      if(all(c(!cond1,!cond2))){
        out <- c(out,f)
      }
    }
    
  }else{
    out <- c('ANALYSIS_YR','NATURAL_ADULT_SPAWNERS',
             'NATURAL_JACK_SPAWNERS','NATURAL_SPAWNERS_TOTAL',
             'ADULT_BROODSTOCK_REMOVALS','JACK_BROODSTOCK_REMOVALS',
             'TOTAL_BROODSTOCK_REMOVALS','OTHER_REMOVALS','TOTAL_RETURN_TO_RIVER',
             'ENUMERATION_METHODS','ADULT_PRESENCE','JACK_PRESENCE','START_DTT',
             'END_DTT','NATURAL_ADULT_FEMALES','NATURAL_ADULT_MALES',
             'EFFECTIVE_FEMALES','WEIGHTED_PCT_SPAWN',
             'STREAM_ARRIVAL_DT_FROM','STREAM_ARRIVAL_DT_TO','START_SPAWN_DT_FROM',
             'START_SPAWN_DT_TO','PEAK_SPAWN_DT_FROM','PEAK_SPAWN_DT_TO',
             'END_SPAWN_DT_FROM','END_SPAWN_DT_TO','ACCURACY','PRECISION',
             'INDEX_YN','RELIABILITY','ESTIMATE_STAGE','ESTIMATE_CLASSIFICATION',
             'NO_INSPECTIONS_USED','ESTIMATE_METHOD','CREATED_DTT','UPDATED_DTT',
             'ACT_ID','Source','Spawners','SpawnersSource',
             'Broodstock','BroodstockSource','Removals','RemovalsSource')
    
    if(newFieldsIncluded){
      out <- c(out,'Returns','MAX_ESTIMATE')
      out[out == "ANALYSIS_YR"] <- 'Year'
      out <- unique(out)
    }
  }
  NUSEDS_l[[4]] <- out
  
  #
  names(NUSEDS_l) <- c('IndexId','GFE_ID','both','none')
  
  out <- list(NUSEDS_l,CUSS_l)
  names(out) <- c("NUSEDS","CUSS")
  
  return(out)
}

#' Function to plot "MAX_ESTIMATE" (or "Returns") time series from the 
#' modified all_areas_nuseds data.
#' Options:
#' - show all GFE_IDs corresponding to each IndexId in IndexIds (vector).
#' - show all IndexIds corresponding to each GFE_ID in GFE_IDs (vector)
#'    - possibility in that later case to filter for certain species given as 
#'      acronyms (i.e. CM, CO, CN, SX, PK, PKE, PKO)
# IndexIds <- c("PKO_52704","SX_3302")
# GFE_IDs <- NA
# species_acro <- NA
# IndexIds <- "CM_42743"
# GFE_IDs <- c( 1829,1911)
# species_acro <- c("PK","CM")
plot_IndexId_GFE_ID_fun <- function(IndexIds = NA, GFE_IDs = NA, species_acro = NA,
                                    all_areas_nuseds, 
                                    xaxt = 's', yaxt = 's', xlab = NA, ylab = NA,
                                    Xlim = NA, Ylim = NA, main = "",pchs = NA,ltys= NA,
                                    y_var_name = c("MAX_ESTIMATE","Returns"),
                                    colPalette = c("firebrick","chartreuse3","black","deepskyblue3")){
  
  y_var_name <- y_var_name[1]
  
  # in case both IndedIds and GFE_IDs are provided and have the same length
  if(!all(is.na(IndexIds)) & !all(is.na(GFE_IDs)) & length(IndexIds) == length(GFE_IDs)){
    
    nusedsHere <- lapply(X = 1:length(IndexIds), FUN = function(i){
      cond <- all_areas_nuseds$IndexId == IndexIds[i] &
        all_areas_nuseds$GFE_ID == as.numeric(GFE_IDs[i])
      return(all_areas_nuseds[cond,])
    })
    nusedsHere <- do.call(rbind,nusedsHere)
    
    # if only certain species are displayed:
    if(all(!is.na(species_acro))){
      nusedsHereSub_l <- lapply(X = species_acro, FUN = function(s){
        out <- nusedsHere[grepl(s,nusedsHere$IndexId),]
        return(out)
      })
      nusedsHere <- do.call(rbind,nusedsHereSub_l)
    }
    
    if(is.na(xlab)){
      xlab = "Years"
    }
    if(is.na(ylab)){
      ylab = y_var_name
    }
    
    if(nrow(nusedsHere) == 0){ # in case there is no time series anymore 
      plot(NA, xlim = c(0,1), ylim = c(0,1), ylab = ylab, xlab = xlab, main = main, 
           xaxt = xaxt, yaxt = yaxt)
      
    }else{
      
      yr_min <- min(nusedsHere$Year)
      yr_max <- max(nusedsHere$Year)
      yrs <- yr_min:yr_max
      pop_max <- max(nusedsHere[,y_var_name], na.rm = T)
      if(is.infinite(pop_max)){
        pop_max <- 1
      }
      
      colfunc <- colorRampPalette(colPalette)
      cols <- colfunc(length(IndexIds))
      if(is.na(ltys)[1]){
        ltys <- 1:length(IndexIds)
      }
      if(is.na(pchs)[1]){
        pchs <- 1:length(IndexIds)
      }
      
      if(all(is.na(Xlim))){
        xlim <- c(yr_min-(yr_max - yr_min)/5,yr_max)
      }else{
        xlim <- Xlim
      }
      if(all(is.na(Ylim))){
        ylim <- c(0,pop_max + pop_max / 5)
      }else{
        ylim <- Ylim
      }
      
      plot(NA, xlim = xlim, ylim = ylim, ylab = ylab, xlab = xlab, main = main, 
           xaxt = xaxt, yaxt = yaxt)
      
      for(s in 1:length(IndexIds)){
        # s <- 1
        cond <- nusedsHere$IndexId == IndexIds[s] &
          nusedsHere$GFE_ID == as.numeric(GFE_IDs[s])
        dataHere <- nusedsHere[cond,]
        dataHere <- dataHere[order(dataHere$Year),c(y_var_name,"Year")]
        # print(dataHere)
        lines(y = dataHere[,y_var_name], x = dataHere$Year, lwd = 2, col = cols[s], 
              lty = ltys[s])
        points(y = dataHere[,y_var_name], x = dataHere$Year, 
               pch = pchs[s], col = cols[s], lwd = 2)
      }
      
      series_name <- sapply(X = 1:length(IndexIds),FUN = function(i){
        out <- paste(IndexIds[i],as.numeric(GFE_IDs[i]), sep = " - ")
        return(out)
      })
      
      legend("topleft",series_name, col = cols, lwd = 3, bty = "n", 
             lty = ltys, pch = pchs)
    }
    
  }else{
    
    # plot all the GFE_IDs found for each IndexId in IndexIds
    if(!all(is.na(IndexIds)) & all(is.na(GFE_IDs))){
      var_out <- "IndexId"
      var_in <- "GFE_ID"
      var_out_vals <- IndexIds
      
      # plot all the IndexIds for each GFE_ID in GFE_IDs 
    }else if(all(is.na(IndexIds)) & !all(is.na(GFE_IDs))){
      var_out <- "GFE_ID"
      var_in <- "IndexId"
      var_out_vals <- GFE_IDs
    }
    
    # plot each IndexIds & GFE_IDs series (no hierarchy in the variables) 
    for(var_out_val in var_out_vals){
      # var_out_val <- var_out_vals[1]
      nusedsHere <- all_areas_nuseds[all_areas_nuseds[,var_out] == var_out_val,]
      
      # if only certain species are displayed:
      if(all(!is.na(species_acro))){
        nusedsHereSub_l <- lapply(X = species_acro, FUN = function(s){
          out <- nusedsHere[grepl(s,nusedsHere$IndexId),]
          return(out)
        })
        nusedsHere <- do.call(rbind,nusedsHereSub_l)
      }
      
      if(is.na(xlab)){
        xlab = "Years"
      }
      if(is.na(ylab)){
        ylab = y_var_name
      }
      
      if(nrow(nusedsHere) == 0){ # in case there is no time series anymore 
        plot(NA, xlim = c(0,1), ylim = c(0,1), ylab = ylab, xlab = xlab, main = main, 
             xaxt = xaxt, yaxt = yaxt)
        
      }else{
        
        var_in_vals <- unique(nusedsHere[,var_in])
        yr_min <- min(nusedsHere$Year)
        yr_max <- max(nusedsHere$Year)
        yrs <- yr_min:yr_max
        pop_max <- max(nusedsHere[,y_var_name], na.rm = T)
        if(is.infinite(pop_max)){
          pop_max <- 1
        }
        
        colfunc <- colorRampPalette(colPalette)
        cols <- colfunc(length(var_in_vals))
        if(is.na(ltys)[1]){
          ltys <- 1:length(var_in_vals)
        }
        if(is.na(pchs)[1]){
          pchs <- 1:length(var_in_vals)
        }
        
        if(all(is.na(Xlim))){
          xlim <- c(yr_min-(yr_max - yr_min)/5,yr_max)
        }else{
          xlim <- Xlim
        }
        if(all(is.na(Ylim))){
          ylim <- c(0,pop_max + pop_max / 5)
        }else{
          ylim <- Ylim
        }
        
        plot(NA, xlim = xlim, ylim = ylim, ylab = ylab, xlab = xlab, main = main, 
             xaxt = xaxt, yaxt = yaxt)
        
        for(var_in_val in var_in_vals){
          # var_in_val <- var_in_vals[1]
          i <- which(var_in_val == var_in_vals)
          dataHere <- nusedsHere[nusedsHere[,var_in] == var_in_val,]
          dataHere <- dataHere[order(dataHere$Year),c(y_var_name,"Year")]
          # print(dataHere)
          lines(y = dataHere[,y_var_name], x = dataHere$Year, lwd = 2, col = cols[i], 
                lty = ltys[i])
          points(y = dataHere[,y_var_name], x = dataHere$Year, 
                 pch = pchs[i], col = cols[i], lwd = 2)
        }
      }
      
      legend("topleft",c(paste(var_out,"=",var_out_val),paste(var_in,"=",var_in_vals)), 
             col = c(NA,cols), lwd = 3, bty = "n", lty = c(NA,ltys), pch = c(NA,pchs))
      
    }
  }
}

#' Function to update the fields associated to IndexId or GFE_ID in CUSS or NUSEDS.
# edit_CUSS = F
# edit_NUSEDS = T
# IndexId_focal = "CO_46835"
# IndexId_alter = "CO_46835"
# GFE_ID_focal = 2463
# GFE_ID_alter = 285
fields_edit_NUSEDS_CUSS_fun <- function(IndexId_focal = NA, IndexId_alter = NA,
                                        GFE_ID_focal = NA, GFE_ID_alter = NA,
                                        edit_NUSEDS = F,  edit_CUSS = F, 
                                        all_areas_nuseds = all_areas_nuseds,
                                        conservation_unit_system_sites = conservation_unit_system_sites){
  
  require(dplyr)
  
  #' Import list for the fields in NUSEDS and CUSS that are associated to unique
  #' IndexId and GFE_ID
  fields_l <- fields_IndexId_GFE_ID_fun(all_areas_nuseds = all_areas_nuseds,
                                        conservation_unit_system_sites = conservation_unit_system_sites)
  
  if(!edit_CUSS & !edit_NUSEDS){
    print("Please choose a dataset to edit.")
    
  }else{
    
    varAlter_presentInSameDataset <- T # for the end in case WATERBODY is used for SYSTEM_SITE and vice versa
    
    # check what has to be updated: Index_ID, GFE_ID or both
    iid_diff <- IndexId_focal != IndexId_alter
    gfeid_diff <- GFE_ID_focal != GFE_ID_alter
    
    if(iid_diff & gfeid_diff){
      print("IndexIds and GFE_IDs are the same.")
      
    }else{
      
      if(iid_diff & !gfeid_diff){       # if IndexId is to change
        var <- "IndexId"
        
      }else if(!iid_diff & gfeid_diff){ # if GFE_ID is to change
        var <- "GFE_ID"
        
      }else if(!iid_diff & !gfeid_diff){
        var <- c("IndexId","GFE_ID")
        
      }
      
      # 
      if(!edit_CUSS & edit_NUSEDS){ # edit NUSEDS
        
        datset_name <- "all_areas_nuseds"
        
        dataset_focal <- all_areas_nuseds
        dataset_alter <- all_areas_nuseds
        
        # check if the alternative series is in NUSEDS
        if(length(var) == 2){
          cond_alter <- dataset_alter$IndexId == IndexId_alter & 
            dataset_alter&GFE_ID == GFE_ID_alter
          # to finish eventually but might not be needed
          print("Write code for when the fields for both IndexId and GFE_ID have to be edited.")
          
        }else if(var == "IndexId"){
          fields <- fields_l$NUSEDS$IndexId
          cond_alter <- dataset_alter$IndexId == IndexId_alter
          # if IndexId_alter is not in NUSEDS --> look for Index_Id - related fields in CUSS
          if(sum(cond_alter) == 0){
            varAlter_presentInSameDataset <- F
            dataset_alter <- conservation_unit_system_sites
            cond_alter <- dataset_alter$IndexId == IndexId_alter
            # find the fields in common for IndexId in NUSEDS and CUSS
            fields <- fields_l$NUSEDS$IndexId[fields_l$NUSEDS$IndexId %in% fields_l$CUSS$IndexId]
          }
          
        }else if(var == "GFE_ID"){
          fields <- fields_l$NUSEDS$GFE_ID
          cond_alter <- dataset_alter$GFE_ID == GFE_ID_alter
          # if GFE_ID_alter is not in NUSEDS --> look for GFE_ID - related fields in CUSS
          if(sum(cond_alter) == 0){
            varAlter_presentInSameDataset <- F
            dataset_alter <- conservation_unit_system_sites
            cond_alter <- dataset_alter$GFE_ID == GFE_ID_alter
            # find the fields in common for GFE_ID in NUSEDS and CUSS
            fields <- fields_l$NUSEDS$GFE_ID[fields_l$NUSEDS$GFE_ID %in% fields_l$CUSS$GFE_ID]
            # add SYSTEM_SITE, which is WATERSHED in NUSEDS
            fields <- c(fields,"SYSTEM_SITE")
          }
        }
        
      }else if(edit_CUSS & !edit_NUSEDS){ # edit CUSS
        # print("Write code for when The fields in CUSS have to be edited.")
        
        datset_name <- "conservation_unit_system_sites"
        
        dataset_focal <- conservation_unit_system_sites
        dataset_alter <- conservation_unit_system_sites
        
        # check if the alternative series is in CUSS
        if(length(var) == 2){
          cond_alter <- conservation_unit_system_sites$IndexId == IndexId_alter & 
            conservation_unit_system_sites&GFE_ID == GFE_ID_alter
          # to finish eventually but might not be needed
          print("Write code for when the fields for both IndexId and GFE_ID have to be edited.")
          
        }else if(var == "IndexId"){
          fields <- fields_l$CUSS$IndexId
          cond_alter <- conservation_unit_system_sites$IndexId == IndexId_alter
          
          # if IndexId_alter is not in CUSS --> look for Index_Id - related fields in NUSEDS
          if(sum(cond_alter) == 0){
            varAlter_presentInSameDataset <- F
            dataset_alter <- all_areas_nuseds
            cond_alter <- dataset_alter$IndexId == IndexId_alter
            # find the fields in common for IndexId in NUSEDS and CUSS
            fields <- fields_l$CUSS$IndexId[fields_l$CUSS$IndexId %in% fields_l$NUSEDS$IndexId]
          }
          
        }else if(var == "GFE_ID"){
          fields <- fields_l$CUSS$GFE_ID
          cond_alter <- dataset_alter$GFE_ID == GFE_ID_alter
          # if GFE_ID_alter is not in CUSS --> look for GFE_ID - related fields in NUSEDS
          if(sum(cond_alter) == 0){
            varAlter_presentInSameDataset <- F
            dataset_alter <- all_areas_nuseds
            cond_alter <- dataset_alter$GFE_ID == GFE_ID_alter
            # find the fields in common for GFE_ID in NUSEDS and CUSS
            fields <- fields_l$CUSS$GFE_ID[fields_l$CUSS$GFE_ID %in% fields_l$NUSEDS$GFE_ID]
            # add WATERSHED , which is SYSTEM_SITE in CUSS
            fields <- c(fields,"WATERSHED")
          }
        }
      }
      
      # update dataset_focal
      cond_focal <- dataset_focal$IndexId == IndexId_focal & 
        dataset_focal$GFE_ID == GFE_ID_focal
      
      print(paste(sum(cond_focal),"rows were edited in",datset_name,"at the following fields:",
                  paste(fields, collapse = ", ")))
      
      for(f in fields){
        f_focal <- f
        if(f == "SYSTEM_SITE" & !varAlter_presentInSameDataset){  # add SYSTEM_SITE WATERSHED in NUSEDS
          f_focal <- "WATERBODY"
        }else if(f == "WATERBODY" & !varAlter_presentInSameDataset){
          f_focal <- "SYSTEM_SITE"
        }
        dataset_focal[cond_focal,f_focal] <- unique(dataset_alter[cond_alter,f])
      }
      
      # check there are duplicated year for a same indexId & GFE_ID series:
      if(!edit_CUSS & edit_NUSEDS){ # edit NUSEDS
        
        dupli <- dataset_focal %>%
          dplyr::group_by(IndexId, GFE_ID, Year) %>%
          dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
          dplyr::filter(n > 1L)
        
        if(nrow(dupli) > 0){
          print("There are duplicated rows in NUSEDS:")
          print(dupli)
        }
      }
      
      return(dataset_focal)
    } 
  }
}

#' Function calculating the number of data points OR % of them of a time series
#' that are (1) complementary (i.e. not duplicated, nor conflictual), (2) duplicated
#' (i.e. same value for same year) and (3) conflictual (i.e. different value for 
#' same year). The series supplied are vectors of values (e.g.population size) 
#' with the time unit (e.g. year) are names.
compare_series_fun <- function(series_focal,series_compare,percentage = F){
  
  # make sure years are in the right order
  series_focal <- series_focal[order(as.numeric(names(series_focal)))]
  series_compare <- series_compare[order(as.numeric(names(series_compare)))]
  
  s_focal <- data.frame(year = as.numeric(names(series_focal)),
                        s_focal = series_focal)
  s_comp <- data.frame(year = as.numeric(names(series_compare)),
                       s_comp = series_compare)
  
  s <- merge(x = s_focal,y = s_comp, by = "year", all = T)
  
  dataPoint_nb <- sum(!is.na(series_focal))
  complementary <- dataPoint_nb - nrow(s[!is.na(s$s_focal) & !is.na(s$s_comp),])
  duplicate <- nrow(s[!is.na(s$s_focal) & !is.na(s$s_comp) & s$s_focal == s$s_comp,])
  conflict <- nrow(s[!is.na(s$s_focal) & !is.na(s$s_comp) & s$s_focal != s$s_comp,])
  
  if(percentage){
    complementary <- round(complementary/dataPoint_nb,1) * 100
    duplicate <- round(duplicate/dataPoint_nb,1) * 100
    conflict <- round(conflict/dataPoint_nb,1) * 100
  }
  
  out <- data.frame(nb_dataPt = dataPoint_nb,
                    complementary = complementary,
                    duplicate = duplicate,
                    conflict = conflict)
  return(out)
}

#' Function to remove rows in 'dataframe' base on the combination of values in 
#' the fields of the other dataframe 'toRemove' (fields must macth between the two
#' dataframes). The function returns the the dataframe without the corresponding
#' rows.
# dataframe <- all_areas_nuseds
remove_rows_fields_fun <- function(dataframe,toRemove,fields = NA,silience = F){
  
  nrow_all <- nrow(dataframe)
  if(all(is.na(fields))){
    fields_here <- colnames(toRemove)
  }else{
    fields_here <- fields
  }
  
  condition <- sapply(X = 1:nrow(toRemove), FUN = function(r){
    
    cond <- sapply(X = fields_here, function(f){
      # f <- fields_here[1]
      cond <- dataframe[,f] == toRemove[r,f]
      return(cond)
    })
    cond <- apply(X = cond, MARGIN = 1, FUN = all)
    return(cond)
  })
  condition <- apply(X = condition, MARGIN = 1, FUN = any)
  
  out <- dataframe[!condition,]
  
  nrow_cut <- nrow(out)
  if(!silience){
    print(paste0("Number of rows removed from dataframe = ",nrow_all - nrow_cut))
  }
  
  return(out)
}

#' Function to return a new row for conservation_unit_system_sites (CUSS) with a
#' series not present (but present in all_areas_nuseds (NUSEDS)), i.e. with a 
#' IndexId/POP_ID and GFE_ID association not present in CUSS.
#' The function fills the different fields using the information associated to 
#' IndexId and GFE_ID in conservation_unit_system_sites and all_areas_nuseds.
#' - In case the GFE_ID of the new series is not present in CUSS, the fields are 
#' filled using the GFE_ID - related information that is present in NUSEDS. 
#' - In case the IndexId is not present in CUSS: not implemented yet (not sure)
#' if that's possible).
# IndexId <- "CN_39983"
# GFE_ID <- 1204
CUSS_newRow_fun <- function(IndexId,GFE_ID,
                            conservation_unit_system_sites,
                            all_areas_nuseds){
  
  # make sure the series does not already exist in CUSS
  cond <- conservation_unit_system_sites$IndexId == IndexId & 
    conservation_unit_system_sites$GFE_ID == GFE_ID
  
  if(sum(cond) != 0){
    print("The series is already in CUSS:")
    print(conservation_unit_system_sites[cond,])
    
  }else{
    
    # return the fields in NUSEDS and CUSS associated to indexId and GFE_ID:
    fields_l <- fields_IndexId_GFE_ID_fun(all_areas_nuseds = all_areas_nuseds,
                                          conservation_unit_system_sites = conservation_unit_system_sites)
    
    # create an empty row and start filling it
    cuss_new <- conservation_unit_system_sites[NA,][1,]
    cuss_new$IndexId <- IndexId
    cuss_new$GFE_ID <- GFE_ID
    
    # Fill with IndexId related fields
    cond_cuss_iid <- conservation_unit_system_sites$IndexId == IndexId
    if(sum(cond_cuss_iid) > 0){ # if IndexId is already present in CUSS:
      
      for(f in fields_l$CUSS$IndexId){
        cuss_new[,f] <- unique(conservation_unit_system_sites[,f][cond_cuss_iid])
      }
      
    }else{  # if IndexId is not already present in CUSS:
      # use fields_l$NUSEDS$IndexId
      
      cond_nuseds_iid <- all_areas_nuseds$IndexId == IndexId
      
      field_comm <- fields_l$CUSS$IndexId[fields_l$CUSS$IndexId %in% fields_l$NUSEDS$IndexId]
      field_comm <- field_comm[field_comm != "IndexId"]
      
      for(f in field_comm){
        cuss_new[,f] <- unique(all_areas_nuseds[,f][cond_nuseds_iid])
      }
      
      # add SPECIES_QUALIFIED
      cuss_new$SPECIES_QUALIFIED <- cuss_new$species_acronym_ncc
      if(cuss_new$species_acronym_ncc == "CN"){
        cuss_new$SPECIES_QUALIFIED <- "CK"
        
      }else if(cuss_new$species_acronym_ncc %in% c("SEL","SER")){
        cuss_new$SPECIES_QUALIFIED <- "SX"
        
      }
    }
    
    # Fill with GFE_ID related fields
    cond_cuss_gfeid <- conservation_unit_system_sites$GFE_ID == GFE_ID
    if(sum(cond_cuss_gfeid) > 0){
      
      for(f in fields_l$CUSS$GFE_ID){
        cuss_new[,f] <- unique(conservation_unit_system_sites[,f][cond_cuss_gfeid])
      }
      
    }else{
      # use fields_l$NUSEDS$GFE_ID
      field_comm <- fields_l$CUSS$GFE_ID[fields_l$CUSS$GFE_ID %in% fields_l$NUSEDS$GFE_ID]
      field_comm <- field_comm[field_comm != "GFE_ID"]
      cond_nuseds_gfeid <- all_areas_nuseds$GFE_ID == GFE_ID
      
      for(f in field_comm){
        cuss_new[,f] <- unique(all_areas_nuseds[,f][cond_nuseds_gfeid])
      }
      
      cuss_new$SYSTEM_SITE <- unique(all_areas_nuseds$WATERBODY[cond_nuseds_gfeid])
    }
  }
  return(cuss_new)
}

#' Function that takes the dataframe locations_duplicated with the column GFE_ID,
#' SYSTEM_SITE, Y_LAT and X_LONGT from conservation_system_sites and returns the 
#' same dataframe but with the rows grouped by identical coordinates. It is used
#' to manually define new coordinates to locations having different SYSTEM_SITE
#' but same coordinates in conservation_system_sites.
#' Additional fields are created: X_LONGT_new and Y_LAT_new to fill after when 
#' attributing new coordinates for certain rows. 
locations_duplicated_group_fun <- function(locations_duplicated,return_dist = F){
  
  #' Sort the dataset with the distance from a unique (random) location, and create
  #' a group column
  locations_duplicated$dist <- distance_Euclidean_fun(x_ref = 0,y_ref = 0,
                                                      x = locations_duplicated$X_LONGT, 
                                                      y = locations_duplicated$Y_LAT)
  
  locations_duplicated <- locations_duplicated[order(locations_duplicated$dist),]
  
  locations_duplicated$group <- NA
  r <- 1
  i_l <- 1
  while(any(is.na(locations_duplicated$group))){
    x <- locations_duplicated$X_LONGT[r]
    y <- locations_duplicated$Y_LAT[r]
    cond <- locations_duplicated$X_LONGT == x & 
      locations_duplicated$Y_LAT == y
    locations_duplicated$group[cond] <- LETTERS[i_l]
    i_l <- i_l + 1
    if(any(is.na(locations_duplicated$group))){
      r <- min(which(is.na(locations_duplicated$group)))
    }
  }
  
  locations_duplicated$X_LONGT_new <- locations_duplicated$Y_LAT_new <- NA
  
  cols <- colnames(locations_duplicated)
  if(!return_dist){
    cols <- cols[cols != "dist"]
  }
  
  return(locations_duplicated[,cols])
}


