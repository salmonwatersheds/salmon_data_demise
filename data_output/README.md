
# List of the files present in this repository: 

- *populationAssessed_catches_data_remove_0s_NAs.xlsx*:  The summary files where both 0s and NAs counts were removed (results presented in the main text); created in script *1_datasets.R*

- *populationAssessed_catches_data_remove_NAs.xlsx*   :  The summary files where only NAs counts were removed (results presented in the supporting information); created in script *1_datasets.R*

- *region_survery.csv*                                : the "region" (i.e. CU-related region) and region_survey (i.e. the region where the survey was conducted); created in script *0_assign-regions.R*


# List of the files potentially not present in this repository: 

In case the following files are not present in the repository, they must be imported manually to reproduce the analyses. These can be accessed on [Zenodo](https://doi.org/10.5281/zenodo.14248904).

  - *all_areas_nuseds_noNAs_TEMP.csv*   : NUSEDS after removing the time series only made of NAs (used in paper)

  - *all_areas_nuseds_noNA0s_TEMP.csv*  : NUSEDS after removing the time series only made of NAs and/or 0s (not used in paper)
  
  - *trackRecord_noNAs_TEMP.csv*        : all the time series in CUSS not in NUSEDS using *all_areas_nuseds_noNAs_TEMP.csv* (used in paper)
  
  - *trackRecord_noNA0s_TEMP.csv*       : all the time series in CUSS not in NUSEDS using *all_areas_nuseds_noNA0s_TEMP.csv* (not used in paper)

**Note:** these four files above are intermediate files to save computation time. The option is provided at the start of the script *a_nuseds_collation.Rmd* to use those intermediate files or not, which is the script generating them.

- *1_NuSEDS_escapement_data_collated_2025-04-15.csv*  : the cleaned combined NUSEDS and CUSS dataset with time series containing only NAs removed; created in script *a_nuseds_collation.Rmd*

- *nuseds_cuid_streamid_2024-11-25.csv*    : the cleaned NuSEDS data available [here](https://zenodo.org/records/14225367) and [here: /data_output](https://zenodo.org/records/14248904); created in script *b_nuseds_cuid_pse.Rmd*

Note: **NUSEDS** = *all_area_nuseds.csv* and **CUSS** = *conservation_units_system_sites.csv*.

****************
