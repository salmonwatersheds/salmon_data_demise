
Code, datasets and figures associated to the manuscript:

[Monitoring for fisheries or for fish? Declines in monitoring of salmon spawners continue despite a conservation crisis](https://doi.org/10.1139/cjfas-2024-0387)

Published in the **Canada Journal of Fisheries and Aquatic Science (CJFAS**)

**Authorship:**

Emma M Atkinson, Bruno S Carturan, Clare P Atkinson, Andrew W Bateman, Katrina Connors, Eric Hertz, Stephanie J Peacock

# Important notes

This repository does not contain all the input datasets needed to fully reproduce the analyses. The entire repository containing all the files is available on [Zenodo](https://doi.org/10.5281/zenodo.14248904). By downloading the latter, one does not have to bring the input files in the correct subfolders.

We provide below the detail of the input files needed to complete the different steps of the analyses that are not present in the subfolder on this Github repository.

## NuSEDS cleaning procedure (Supplement A - Part 1)

- script: *a_nuseds_collation.Rmd*; [Supplement A - Part 1](https://bookdown.org/salmonwatersheds/1_nuseds_collation_atkinson/1_nuseds_collation.html)

- Files to place in the `/data_input` folder:

  - *all_areas_nuseds_2025-02-21.csv*                    : the downloaded *all_areas_nuseds.csv* from [DFO](https://open.canada.ca/data/en/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6)

  - *conservation_unit_system_sites_2025-02-20.csv*      : the downloaded *conservation_unit_system_sites.csv* from [DFO](https://open.canada.ca/data/en/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6)

  - *DFO_All_Streams_Segments_20240408.xlsx*             : stream - GFE_ID data file from DFO (emailed by Wu Zhipeng, DFO, 09/04/2024)
  
  - *PSF_modified_SEP_releases_2023.xlsx*                : DFO's Hatchery dataset to access additional GFE_IDs (emailed by Brock Ramshaw, DFO, 03/07/2023)
  
  - */pse_conservation_units/pse_conservation_units.gdb* : shape files of the conservation unites as defined in the [PSE](https://www.salmonexplorer.ca/)
  
  - */se_boundary_regions/se_boundary_regions.shp*       : shape files for the Region boundaries as defined in the [PSE](https://www.salmonexplorer.ca/)


- Files to place in the `/data_output` folder:

  - *all_areas_nuseds_noNAs_TEMP.csv*   # NUSEDS after removing the time series only made of NAs (used in paper)

  - *all_areas_nuseds_noNA0s_TEMP.csv*  # NUSEDS after removing the time series only made of NAs and/or 0s (not used in paper)
  
  - *trackRecord_noNAs_TEMP.csv*        # all the time series in CUSS not in NUSEDS using *all_areas_nuseds_noNAs_TEMP.csv* (used in paper)
  
  - *trackRecord_noNA0s_TEMP.csv*       # all the time series in CUSS not in NUSEDS using *all_areas_nuseds_noNA0s_TEMP.csv* (not used in paper)

Note: these four files are intermediate files to save computation time. The option is provided at the start of the script to use those intermediate files or not. 

## NuSEDS cleaning procedure (Supplement A - Part 2)

- script: *b_nuseds_cuid_pse.Rmd*; [Supplement A - Part 2](https://bookdown.org/salmonwatersheds/2_nuseds_cuid_pse_atkinson/2_nuseds_cuid_pse.html)

- Files to place in the `/data_input` folder:

  - *streamlocationids.csv*                             : names, coordinates and identification number of streams defined in the [PSE](https://www.salmonexplorer.ca/)


- Files to place in the `/data_output` folder:

  - *1_NuSEDS_escapement_data_collated_2025-04-15.csv*  : the cleaned combined NUSEDS and CUSS dataset with time series containing only NAs removed


## Assigning regions to NuSEDS populations based on survey locations

- script: *0_assign-regions.R*

- Files to place in the `/data_input` folder:

  - */gshhg-shp-2.3.7/GSHHS_shp/f/GSHHS_f_L1.shp* : the shape files for the shorelines, to download from OAA, Global Self-consistent, Hierarchical, High-resolution Geography Database (GSHHG) [access](https://www.ngdc.noaa.gov/mgg/shorelines/data/gshhg/latest/)

- Files to place in the `/data_output` folder:

  - *nuseds_cuid_streamid_2025-04-15.csv*    : the cleaned NuSEDS data available [here](https://zenodo.org/records/14225367) and [here: /data_output](https://zenodo.org/records/14248904)


## Preparing the region and species summary datasets

- script: *1_datasets.R*

- Files to place in the `/data_input` folder: 

  - All the needed files are present in the folder 


- Files to place in the `/data_output` folder:

  - *nuseds_cuid_streamid_2025-04-15.csv*    : the cleaned NuSEDS data available [here](https://zenodo.org/records/14225367) and [here: /data_output](https://zenodo.org/records/14248904)
  
  
## Analyses, figures and summary statistics

- script: *2_analyses.R*

- Files to place in the `/data_input` folder: 

  - All the needed files are present in the folder 

- Files to place in the `/data_output` folder:

  - *nuseds_cuid_streamid_2025-04-15.csv*    : the cleaned NuSEDS data available [here](https://zenodo.org/records/14225367) and [here: /data_output](https://zenodo.org/records/14248904)


## Supplementary figures, details of the GLM analyses and summary of the NuSEDS cleaning procedure (Supplement B)

- script: *3_Supplement_B.Rmd*; [Supplement_B](https://cdnsciencepub.com/doi/abs/10.1139/cjfas-2024-0387)

- Files to place in the `/data_input` folder: 

  - All the needed files are present in the folder 
  
- Files to place in the `/data_output` folder:

  - All the needed files are present in the folder

****************
