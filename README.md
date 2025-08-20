
Code, datasets and figures associated to the manuscript:

[Monitoring for fisheries or for fish? Declines in monitoring of salmon spawners continue despite a conservation crisis](https://doi.org/10.1139/cjfas-2024-0387)

Published in the **Canada Journal of Fisheries and Aquatic Science (CJFAS**)

**Authorship:**

Emma M Atkinson, Bruno S Carturan, Clare P Atkinson, Andrew W Bateman, Katrina Connors, Eric Hertz, Stephanie J Peacock

# Important notes

This repository does not contain all the input datasets needed to fully reproduce the analyses. The entire repository containing all the files is available on [Zenodo](https://doi.org/10.5281/zenodo.14248904). By downloading the latter, one does not have to bring the input files in the correct subfolders.

We provide below a detail of the input files needed to complete the different steps of the analyses.

## NuSEDS cleaning procedure (Supplement A - Part 1)

- script: *a_nuseds_collation.Rmd*; [Supplement A - Part 1](https://bookdown.org/salmonwatersheds/1_nuseds_collation_atkinson/1_nuseds_collation.html)

- Files to place in the `/data_input` folder:

  - *all_areas_nuseds_2025-02-21.csv*                    : the downloaded *all_areas_nuseds.csv* from DFO
  
  - *conservation_unit_system_sites_2025-02-20.csv*      : the downloaded *conservation_unit_system_sites.csv* from DFO

  - *DFO_All_Streams_Segments_20240408.xlsx*             : stream - GFE_ID data file from DFO (emailed by Wu Zhipeng, DFO, 09/04/2024)
  
  - *PSF_modified_SEP_releases_2023.xlsx*                : DFO's Hatchery dataset to access additionaly GFE_IDs (emailed by Brock Ramshaw, DFO, 03/07/2023)
  
  - */pse_conservation_units/pse_conservation_units.gdb* : shape files of the conservation unites as defined in the [PSE](https://www.salmonexplorer.ca/)
  
  - */se_boundary_regions/se_boundary_regions.shp*       : shape files for the Region boundaries as defined in the [PSE](https://www.salmonexplorer.ca/)


- Files to place in the `/data_output` folder:

  - *all_areas_nuseds_noNAs_TEMP.csv*   # NUSEDS after removing the time series only made of NAs (used in paper)

  - *all_areas_nuseds_noNA0s_TEMP.csv*  # NUSEDS after removing the time series only made of NAs and/or 0s (not used in paper)
  
  - *trackRecord_noNAs_TEMP.csv*        # all the time series in CUSS not in NUSEDS using all_areas_nuseds_noNAs_TEMP.csv (used in paper)
  
  - *trackRecord_noNA0s_TEMP.csv*       # all the time series in CUSS not in NUSEDS using all_areas_nuseds_noNA0s_TEMP.csv (not used in paper)
  
  

!!! WORK IN PROGRESS !!!


#' Files both imported & exported (intermediate "_TEMP" files created to save computation time):


#'
#'



