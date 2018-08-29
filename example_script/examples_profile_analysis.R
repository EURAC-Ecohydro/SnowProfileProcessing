rm(list = ls())

library(devtools)
install_github("bridachristian/SnowProfileProcessing")
library(SnowProfileProcessing)
library(XML)

# ~~~~~~ Set working directory to Source File Location ~~~~~~~~ 

# Remember to set working directory as folder were this script is hosted.
# Click on Session --> Set Working Directory --> To Source File Location


# ~~~~~~ Read external input file ~~~~~~~~ 
file_config_name = "file_config_v1.xml"
inpt_file = xmlParse(file_config_name,useInternalNodes = F) 

list_inpt = file_config_xml_parsing(inpt_file = inpt_file)

# ~ ~ ~ ~ General Input ~ ~ ~ ~ 

output_dir = list_inpt$general$output_dir

#===========================================================================================================================================================================
# ....... Part 1 .......
# 
# Description:  In this section we merge stratigraphy with density.
#               The two file must have the same datetime and the same station code to perform the merging process. 
#               Horizontal density sample allow to cross layers and position of meauserment. 

# ~ ~ ~ ~ Input ~ ~ ~ ~
rm(list =setdiff(ls(),c("list_inpt","output_dir")) )

input_dir = list_inpt$part1$input_dir

stratigraphy_file = list_inpt$part1$stratigraphy_file
density_file = list_inpt$part1$density_file
dates_colnames =list_inpt$part1$dates_colnames
hours_colnames = list_inpt$part1$hours_colnames
station_code = list_inpt$part1$station_code
output_combined_file = list_inpt$part1$output_combined_file

# ~ ~ ~ ~ Combine tables ~ ~ ~ ~

merged_table = combine_stratigraphy_density(input_dir = input_dir,
                                            stratigraphy_file = stratigraphy_file,
                                            density_file = density_file,
                                            dates_colnames = dates_colnames,
                                            hours_colnames = hours_colnames,
                                            station_code = station_code)

# ~ ~ ~ ~ Write output ~ ~ ~ ~

write.csv(merged_table,paste(output_dir, output_combined_file,sep = ""),na = "NaN",row.names = F)

#===========================================================================================================================================================================
# ....... Part 2 .......
#
# Description:  In this section we estimate the snow density matrix based on Valt method starting from known layers
#               Snow density are estimated from grain shape and hardnes.
#               In this example we calculate snow density on the dataset of snow surveys (Snow Observer, mod.3-4 AINEVA) of Trentino (MeteoTrentino) for peridod 2012-2018
#               (about 12000 layers on 48 station)

# ~ ~ ~ ~ Input ~ ~ ~ ~
rm(list =setdiff(ls(),c("list_inpt","output_dir")) )

merged_table_dir = list_inpt$part2$merged_table_dir
merged_table_file = list_inpt$part2$merged_table_file

grain_shape1_colnames = list_inpt$part2$grain_shape1_colnames
grain_shape2_colnames = list_inpt$part2$grain_shape2_colnames
density_colnames = list_inpt$part2$density_colnames
hardness_colnames = list_inpt$part2$hardness_colnames

output_SWE_grain_density_AVG_file  = list_inpt$part2$output_SWE_grain_density_AVG_file
output_SWE_grain_density_N_file  = list_inpt$part2$output_SWE_grain_density_N_file
output_SWE_grain_density_SD_file = list_inpt$part2$output_SWE_grain_density_SD_file

# ~ ~ ~ ~ Read mergerd table, and calculate matrix of density from grain shape and hardness (Valt et al. ) ~ ~ ~ ~

merged_table = read.csv(paste(merged_table_dir, merged_table_file,sep = ""),stringsAsFactors = F )

SWE_grain_table = calculate_density_matrix_grain_hardness(merged_table,grain_shape1_colnames,grain_shape2_colnames ,density_colnames, hardness_colnames)[[1]]
SWE_grain_table_N_obs = calculate_density_matrix_grain_hardness(merged_table,grain_shape1_colnames,grain_shape2_colnames ,density_colnames, hardness_colnames)[[2]]
SWE_grain_table_st.dv = calculate_density_matrix_grain_hardness(merged_table,grain_shape1_colnames,grain_shape2_colnames ,density_colnames, hardness_colnames)[[3]]

# ~ ~ ~ ~ Write output ~ ~ ~ ~

write.csv(SWE_grain_table, paste(output_dir, output_SWE_grain_density_AVG_file,sep = ""),na = "NaN")
write.csv(SWE_grain_table_N_obs, paste(output_dir, output_SWE_grain_density_N_file,sep = ""),na = "NaN")
write.csv(SWE_grain_table_st.dv, paste(output_dir, output_SWE_grain_density_SD_file,sep = ""),na = "NaN")


#===========================================================================================================================================================================
# ....... Part 3 .......
#
# Description:  In this section we estimate the snow density based on Valt method for missing layers and calculate snow water equivalent.
#               For each layer,if the density is missing it is estimated from SWE_grain_hardness_file (the matrix created before or a literature matrix)
#               Snow water equivalent are calculated from snow density and layer thickness.
#               SWE and Density are calculated for EACH layer.

# ~ ~ ~ ~ Input ~ ~ ~ ~
rm(list =setdiff(ls(),c("list_inpt","output_dir")))

SWE_grain_hardness_dir = list_inpt$part3$SWE_grain_hardness_dir
SWE_grain_hardness_file = list_inpt$part3$SWE_grain_hardness_file
merged_table_dir =  list_inpt$part3$merged_table_dir
merged_table_file =  list_inpt$part3$merged_table_file
grain_shape1_colnames =  list_inpt$part3$grain_shape1_colnames
grain_shape2_colnames =  list_inpt$part3$grain_shape2_colnames
density_colnames =  list_inpt$part3$density_colnames
hardness_colnames =  list_inpt$part3$hardness_colnames
layer_from_colnames = list_inpt$part3$layer_from_colnames
layer_to_colnames =  list_inpt$part3$layer_to_colnames
filter_profile_colnames = list_inpt$part3$filter_profile_colnames
output_density_SWE_estimated_file = list_inpt$part3$output_density_SWE_estimated_file

# ~ ~ ~ ~ Reconstruct rho and SWE for each layer: if not measured we use Valt et al. ~ ~ ~ ~

merged_table = read.csv(paste(merged_table_dir, merged_table_file,sep = ""),stringsAsFactors = F )
SWE_grain_table = read.csv(paste(SWE_grain_hardness_dir,SWE_grain_hardness_file,sep = ""), stringsAsFactors = F, header = F,na.strings = c("NA", "NaN"))

new_profiles_table = calculator_density_and_SWE(merged_table, SWE_grain_table, grain_shape1_colnames,
                                                grain_shape2_colnames, density_colnames, hardness_colnames,
                                                layer_from_colnames, layer_to_colnames,filter_profile_colnames)

# ~ ~ ~ ~ Write output ~ ~ ~ ~

write.csv(new_profiles_table, paste(output_dir, output_density_SWE_estimated_file, sep = ""),na = "NaN", row.names = F)


#===========================================================================================================================================================================
# ....... Part 4 .......
#
# Description:  In this section we aggregate each profile to observe the behaviour of snowpack in term of snow height, snow density and snow water equivalent
#               In the example there is the possibility to expand variables to show inside output aggregation. To insert new information, for example slope, you can add
#               a new variable in file_config.xml, part4, read it in input section, and add to the variable supplementary_info_variables.

# ~ ~ ~ ~ Input ~ ~ ~ ~

new_profiles_table_dir = list_inpt$part4$new_profiles_table_dir
new_profiles_table_file = list_inpt$part4$new_profiles_table_file

filter_profile_colnames = list_inpt$part4$filter_profile_colnames

datetime_colnames = list_inpt$part4$datetime_colnames
stat.code_colnames = list_inpt$part4$stat.code_colnames
altitude_colnames = list_inpt$part4$altitude_colnames
latitute_colnames = list_inpt$part4$latitute_colnames
longitude_colnames = list_inpt$part4$longitude_colnames
exposition_colnames = list_inpt$part4$exposition_colnames

date_colnames = list_inpt$part4$date_colnames
hours_colnames = list_inpt$part4$hours_colnames
layer_from_colnames = list_inpt$part4$layer_from_colnames
density_final_colnames = list_inpt$part4$density_final_colnames
SWE_colnames = list_inpt$part4$SWE_colnames
recontrustction_flag_colnames = list_inpt$part4$recontrustction_flag_colnames
thickness_colnames = list_inpt$part4$thickness_colnames

output_profile_aggregation = list_inpt$part4$output_profile_aggregation

# ~ ~ ~ ~ Aggregate for each profile Density, HS and SWE ~ ~ ~ ~

new_profiles_table = read.csv(paste(new_profiles_table_dir, new_profiles_table_file,sep = ""),stringsAsFactors = F )

supplementary_info_variables = c(datetime_colnames,stat.code_colnames,
                                 altitude_colnames, latitute_colnames, longitude_colnames,exposition_colnames)

profiles_aggreg = aggregate_profiles(new_profiles_table,filter_profile_colnames,
                                     supplementary_info_variables,
                                     date_colnames, hours_colnames,
                                     layer_from_colnames, density_final_colnames, SWE_colnames,
                                     recontrustction_flag_colnames,thickness_colnames)

write.csv(profiles_aggreg,paste(output_dir,output_profile_aggregation,sep = ""),na = "NaN",row.names = F)


