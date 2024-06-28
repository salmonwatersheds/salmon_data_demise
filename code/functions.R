

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
#' pattern.
import_mostRecent_file_fun <- function(wd,pattern){
  
  files_c <- list.files(wd)
  files_c <- files_c[grepl(x = files_c, 
                           pattern = pattern)]
  
  if(length(files_c) == 0){
    print("File not found.")
    out <- NA
  }else{
    file.mtime <- file.mtime(paste(wd,files_c,sep="/"))
    file <- files_c[file.mtime == max(file.mtime)]
    print(paste0("File imported: ",file," ; Date modified: ", max(file.mtime)))
    out <- read.csv(paste(wd,file,sep = "/"),header = T)
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
colour_transparency_fun <- function(colours,alpha=0.35){
  col.rgb <- col2rgb(colours)
  colnames(col.rgb) <- colours
  output <- c()
  for(i in 1:length(col.rgb[1,])){
    output[i] <- rgb(red = col.rgb[1,i],
                     green = col.rgb[2,i],
                     blue = col.rgb[3,i], 
                     maxColorValue = 255,alpha = alpha*255)
  }
  return(output)
}

