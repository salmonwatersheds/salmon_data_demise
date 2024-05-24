

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
                                   colours = NA, y_max = NA,
                                   wd_figures, figure_name = NA, 
                                   legend_imposed = T, # the legend is imposed by the vector colours
                                   legend_show = T){

  varCount_values <- unique(dataset[,varCount])
  varCount_values[is.na(varCount_values)] <- "NA"
  varX_val <- unique(dataset[,varX])
  varX_val <- varX_val[order(varX_val)]
  
  varXClass <- class(dataset[,varX])
  
  # find the maximum varCount 
  x_max <- c()
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
    
    x_max <- c(x_max,max(dataset_cut_varCount$count))
    #names(x_max)[i] <- v
    
    dataset_varCount_l[[i]] <- dataset_cut_varCount
  }
  names(x_max) <- names(dataset_varCount_l) <- varCount_values
  
  # order varCount_values by abundance
  varCount_values <- varCount_values[rev(order(x_max))]
  dataset_varCount_l <- dataset_varCount_l[varCount_values]
  
  if(is.na(y_max)[1]){
    y_max <- max(x_max)
  }
  
  if(is.na(figure_name)){
    figure_name <- paste0("figure_",varCount,".jpeg")
  }
  
  if(figure_print){
    jpeg(paste(wd_figure,figure_name,sep = "/"),
         width = width * coef, height = height * coef, units = units, res = res)
  }
  par(mar = c(side1,side2,side3,side4))
  plot(1, xlim = range(nuseds$Year), ylim = c(0,y_max), las = las,
       ylab = ylab, xlab = xlab, main = main, xaxt = xaxt, yaxt = yaxt)
  
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

