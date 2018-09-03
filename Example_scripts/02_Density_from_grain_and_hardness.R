#
# Description:  In this section we estimate the snow density matrix based on Valt method starting from known layers
#               Snow density are estimated from grain shape and hardnes.
#               In this example we calculate snow density on the dataset of snow surveys (Snow Observer, mod.3-4 AINEVA) of Trentino (MeteoTrentino) for peridod 2012-2018
#               (about 12000 layers on 48 station)

rm(list = ls())

library(devtools)
install_github("bridachristian/SnowProfileProcessing")
library(SnowProfileProcessing)
library(XML)

# ~~~~~~ Set working directory to Source File Location ~~~~~~~~ 

# Remember to set working directory as folder were this script is hosted.
# Click on Session --> Set Working Directory --> To Source File Location

setwd("../File_config/")

# ~~~~~~ Read external input file ~~~~~~~~ 

file_config_name = "file_config_v0.xml"
inpt_file = xmlParse(file_config_name,useInternalNodes = F) 

list_inpt = file_config_xml_parsing(inpt_file = inpt_file)

# ~ ~ ~ ~ General Input ~ ~ ~ ~ 

output_dir = list_inpt$general$output_dir

# ~ ~ ~ ~ Input ~ ~ ~ ~

rm(list =setdiff(ls(),c("list_inpt","output_dir")) )

merged_table_dir = "../Data/Output_data/"
merged_table_file = "Dati_strati_densita_Husky_2012_18.csv"

grain_shape1_colnames = "FomaGrani1"
grain_shape2_colnames = "FomaGrani2"
density_colnames = "density"
hardness_colnames = "TestMano"

output_SWE_grain_density_AVG_file  =  "SWE_grain_hardness.csv"
output_SWE_grain_density_N_file  = "SWE_grain_hardness_N.csv" 
output_SWE_grain_density_SD_file = "SWE_grain_hardness_SD.csv" 

#===========================================================================================================================================================================
# Script starts here:

# ~ ~ ~ ~ Read mergerd table, and calculate matrix of density from grain shape and hardness (Valt et al. ) ~ ~ ~ ~

merged_table = read.csv(paste(merged_table_dir, merged_table_file,sep = ""),stringsAsFactors = F )

SWE_grain_table = calculate_density_matrix_grain_hardness(merged_table,grain_shape1_colnames,grain_shape2_colnames ,density_colnames, hardness_colnames)[[1]]
SWE_grain_table_N_obs = calculate_density_matrix_grain_hardness(merged_table,grain_shape1_colnames,grain_shape2_colnames ,density_colnames, hardness_colnames)[[2]]
SWE_grain_table_st.dv = calculate_density_matrix_grain_hardness(merged_table,grain_shape1_colnames,grain_shape2_colnames ,density_colnames, hardness_colnames)[[3]]

# ~ ~ ~ ~ Write output ~ ~ ~ ~

write.csv(SWE_grain_table, paste(output_dir, output_SWE_grain_density_AVG_file,sep = ""),na = "NaN")
write.csv(SWE_grain_table_N_obs, paste(output_dir, output_SWE_grain_density_N_file,sep = ""),na = "NaN")
write.csv(SWE_grain_table_st.dv, paste(output_dir, output_SWE_grain_density_SD_file,sep = ""),na = "NaN")
