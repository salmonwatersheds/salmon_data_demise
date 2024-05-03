

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


