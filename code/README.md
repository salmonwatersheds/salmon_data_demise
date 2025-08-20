

Details about the scripts:

## NuSEDS cleaning procedure (Supplement A)

- *a_nuseds_collation.Rmd*; [Supplement A - Part 1](https://bookdown.org/salmonwatersheds/1_nuseds_collation_atkinson/1_nuseds_collation.html)

The goal of the script is to merge the two constituent datasets **all_areas_nuseds** (referred to as **NUSEDS**) and **conservation_unit_system_sites** (referred to as **CUSS**) so each time series in **NUSEDS** (i.e., fish counts associated to a unique populations, characterized by a unique `IndexId`/`POP_ID` & `GFE_ID` association) can be attributed to a unique conservation unit (CU) in **CUSS** (characterized by `CU_NAME` and `FULL_CU_IN`). But there are multiple inconsistencies between the two datasets that lead to discard a significant amount of data points in **NUSEDS**, unless fixes are implemented. This script is a attempt to implement such fixes.


- *b_nuseds_cuid_pse.Rmd*; [Supplement A - Part 2](https://bookdown.org/salmonwatersheds/2_nuseds_cuid_pse_atkinson/2_nuseds_cuid_pse.html)

This is the second part of the NuSEDS cleaning procedure. The goal of the script is to import the cleaned merge of **NUSEDS** and **CUSS** (*1_NuSEDS_escapement_data_collated_2025-04-15.csv*) generated in the first part (Supplement A - Part 1) and to associate each population (`POP_ID` = `IndexId`, the latter also specifying the species acronym) to the conservation unit identification number `cuid`, as defined in the Pacific Salmon explorer ([PSE](https://www.salmonexplorer.ca/)).


## Analyses of salmon monitoring

- *0_assign-regions.R*

The goal is to assign regions to populations based on survey locations (`region_survey`) (in addition to CU-related `region`).


- *1_datasets.R*

The goal is to make species and region summary datasets from the cleaned NuSEDS dataset and the catch data to be used in *2_analyses.R*.


- *2_analyses.R*

The goal is to generate the figures and extra summary statistics.


- *3_Supplement_B.Rmd*; [Supplement_B](https://cdnsciencepub.com/doi/abs/10.1139/cjfas-2024-0387)

The goal is to present the supplementary figures, and conduct the GLM analyses as well as to provide a summary of the NuSEDS cleaning procedure (presented in details in Supplement A).


- *function.R*

The script contains all the custom-made functions used in the other scripts.


- *colours.R*

The script contains the colours used for the figures.


****************
