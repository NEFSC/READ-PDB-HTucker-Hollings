Project Title : Determining the effects of altered survey design on relative abundance indices for groundfish

heat_map_function.R : file that contains the sampling effort map, but in function form.
make_new_shapefiles.R : Script to generate new shapefiles for super strata.
calc_index_nosample: Script to calculate index with specified strata groups without resampling
one_strata_resample.R : function for resampling weight and catch number within one strata. code that creates the function "strata_resample" to do this
final_resample_process.R : The script that runs the resampling loop for both alternatives and both species (needs function in one_strata_resample.R to work)
Plot_results.R : Script to plot index and strata-specific mean results.
final_plotting_output.Rmd : markdown file that has all the up-to-date plots and all the different versions of them
planB_assessment.R : the beginning of a script that would allow to implementation into stock assessment

calc_original_index.R: HMT practice script to calculate index based on original design.
calc_survey_area.R : HMT practice file where I originally made the sampling effort map. 
significance_testing.r : HMT practice file to start significance tests

Information needed from google drive to run all the plots:
In the "DATA" folder
- all the EEX shape files
- 596061, 646566, 6459, 6560, 6661 shape files
- winter_flounder_survey.csv
- winter_flounder_indices_646566_596061.csv
- winter_flounder_indices_6459_6560_6661.csv
- winter_flounder_indices_design.csv
- winter_flounder_strata_means_646566_596061.csv
- winter_flounder_strata_means_6459_6560_6661.csv
- winter_flounder_strata_means_design.csv
- yellowtail_flounder_survey.csv
- yellowtail_flounder_indices_646566_596061.csv
- yellowtail_flounder_indices_6459_6560_6661.csv
- yellowtail_flounder_indices_design.csv
- yellowtail_flounder_strata_means_646566_596061.csv
- yellowtail_flounder_strata_means_6459_6560_6661.csv
- yellowtail_flounder_strata_means_design.csv

In the "RESULTS" folder
- final_resample_resultsAB.csv
- final_resample_resultsCDE.csv
- final_strata_meanAB.csv
- final_strata_meanCDE.csv
- YTfinal_resample_resultsAB.csv
- YTfinal_resample_resultsCDE.csv
- YTfinal_strata_meanAB.csv
- YTfinal_strata_meanCDE.csv

:fish:


This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

