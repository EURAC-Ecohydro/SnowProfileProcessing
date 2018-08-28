rm(list = ls())
library(SnowProfileProcessing)
library(XML)

# ~~~~~~ Read external input file ~~~~~~~~ 
inpt_file = xmlParse("file_cofig.xml",useInternalNodes = F) 

list_inpt = xmlToList(inpt_file)
list_inpt = list_inpt[-which(names(list_inpt) == "comment")]

for(i in 1:length(list_inpt)){
  list_inpt[[i]] = list_inpt[[i]][-which(names(list_inpt[[i]]) == "comment")]
}

for(i in 1:length(list_inpt)){
  for(j in 1:length(list_inpt[[i]])){
    list_inpt[[i]][[j]] = gsub(pattern =  "\"",replacement = "",x = list_inpt[[i]][[j]])
  }
}

#~~~~~~ Input ~~~~~~~~

input_dir = list_inpt$general$input_dir
support_dir = list_inpt$general$support_dir
output = list_inpt$general$output_dir

# input_dir =  "H:/Projekte/Cryomon/06_Workspace/BrC/neve_tn/2012_2018_test/Input_data/"
# support_dir = "H:/Projekte/Cryomon/06_Workspace/BrC/neve_tn/2012_2018_test/Support_files/"
# output_dir = "H:/Projekte/Cryomon/06_Workspace/BrC/neve_tn/2012_2018_test/Output_data/"



# ....... Part 1 .......

stratigraphy_file = list_inpt$part1$stratigraphy_file
density_file = list_inpt$part1$density_file
dates_colnames =list_inpt$part1$dates_colnames
hours_colnames = list_inpt$part1$hours_colnames
station_code = list_inpt$part1$station_code
output_combined_file = list_inpt$part1$output_combined_file

# stratigraphy_file = "Dati_strati_Husky_2012_18.csv"   # File csv
# density_file = "Dati_densita_Husky_2012_18.csv"       # File csv
# 
# dates_colnames = "Data"    # format YYYY-mm-df_grains_hardness_density, example 2018-01-01
# hours_colnames = "Ora"     # HMM, example 1030, 830 (numeric)
# station_code = "Codice.Stazione"
# 
# output_combined_file = "Dati_strati_densita_Husky_2012_18.csv"

# ....... Part 2 .......

merged_table_dir = output_dir
merged_table_file = "Dati_strati_densita_Husky_2012_18.csv"

grain_shape1_colnames = "FomaGrani1"
grain_shape2_colnames = "FomaGrani2"
density_colnames = "density"
hardness_colnames = "TestMano"

output_SWE_grain_density_AVG_file  = "SWE_grain_hardness.csv"
output_SWE_grain_density_N_file  = "SWE_grain_hardness_N.csv"
output_SWE_grain_density_SD_file = "SWE_grain_hardness_SD.csv"


# ....... Part 3 .......

SWE_grain_hardness_dir = support_dir
SWE_grain_hardness_file = "EXAMPLE_SWE_grain_hardness.csv"

merged_table_dir = output_dir
merged_table_file = "Dati_strati_densita_Husky_2012_18.csv"

grain_shape1_colnames = "FomaGrani1"
grain_shape2_colnames = "FomaGrani2"
density_colnames = "density"
hardness_colnames = "TestMano"

layer_from_colnames = "Da"
layer_to_colnames = "A"

filter_profile_colnames = "Filter_code"

output_density_SWE_estimated_file = "Profiles_with_Density_and_SWE_estimated.csv"

# ....... Part 4 .......

new_profiles_table_dir = output_dir
new_profiles_table_file = "Profiles_with_Density_and_SWE_estimated.csv"

filter_profile_colnames = "Filter_code"
datetime_colnames = "Datetime"
stat.code_colnames = "Codice.Stazione"
altitude_colnames = "Altitudine"
latitute_colnames = "LatUTM"
longitude_colnames = "LongUTM"
exposition_colnames = "Esposizione"
date_colnames = "Data"
hours_colnames = "Ora"
layer_from_colnames = "Da"
density_final_colnames = "density_final"
SWE_colnames = "SWE_calc"

output_profile_aggregation = "Profiles_aggregated.csv"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ....... Part 1 .......

#~~~~~~ Combine tables and write output ~~~~~~~~

merged_table = combine_stratigraphy_density(input_dir = input_dir,
                                            stratigraphy_file = stratigraphy_file,
                                            density_file = density_file,
                                            dates_colnames = dates_colnames,
                                            hours_colnames = hours_colnames,
                                            station_code = station_code)

# write.csv(merged_table,paste(output_dir, output_combined_file,sep = ""),na = "NaN",row.names = F)


# ....... Part 2 .......

#~~~~~~ Read mergerd table, and calculate matrix of density from grain shape and hardness (Valt et al. ) ~~~~~~~~

merged_table = read.csv(paste(merged_table_dir, merged_table_file,sep = ""),stringsAsFactors = F )

SWE_grain_table = calculate_density_matrix_grain_hardness(merged_table,grain_shape1_colnames,grain_shape2_colnames ,density_colnames, hardness_colnames)[[1]]
SWE_grain_table_N_obs = calculate_density_matrix_grain_hardness(merged_table,grain_shape1_colnames,grain_shape2_colnames ,density_colnames, hardness_colnames)[[2]]
SWE_grain_table_st.dv = calculate_density_matrix_grain_hardness(merged_table,grain_shape1_colnames,grain_shape2_colnames ,density_colnames, hardness_colnames)[[3]]

# write.csv(SWE_grain_table, paste(output_dir, output_SWE_grain_density_AVG_file,sep = ""),na = "NaN")
# write.csv(SWE_grain_table_N_obs, paste(output_dir, output_SWE_grain_density_N_file,sep = ""),na = "NaN")
# write.csv(SWE_grain_table_st.dv, paste(output_dir, output_SWE_grain_density_SD_file,sep = ""),na = "NaN")


# ....... Part 3 .......

#~~~~~~ Reconstruct rho and SWE for each layer: if not measured we use Valt et al.  ~~~~~~~~

merged_table = read.csv(paste(merged_table_dir, merged_table_file,sep = ""),stringsAsFactors = F )

SWE_grain_table = read.csv(paste(support_dir,SWE_grain_hardness_file,sep = ""), stringsAsFactors = F, header = F,na.strings = c("NA", "NaN"))


new_profiles_table = calculator_density_and_SWE(merged_table, SWE_grain_table, grain_shape1_colnames,
                                                grain_shape2_colnames, density_colnames, hardness_colnames,
                                                layer_from_colnames, layer_to_colnames,filter_profile_colnames)


# write.csv(new_profiles_table, paste(output_dir, output_density_SWE_estimated_file, sep = ""),na = "NaN", row.names = F)


# ....... Part 4 .......

#~~~~~~ Aggregate for each profile Density, HS and SWE  ~~~~~~~~


new_profiles_table = read.csv(paste(new_profiles_table_dir, new_profiles_table_file,sep = ""),stringsAsFactors = F )

supplementary_info_variables = c(datetime_colnames,stat.code_colnames,
                                 altitude_colnames, latitute_colnames, longitude_colnames,exposition_colnames)

# profiles_aggreg = aggregate_profiles(new_profiles_table,
#                     filter_profile_colnames, datetime_colnames,stat.code_colnames,
#                     altitude_colnames, latitute_colnames, longitude_colnames,exposition_colnames,
#                     date_colnames, hours_colnames,
#                     layer_from_colnames, density_final_colnames, SWE_colnames)

profiles_aggreg = aggregate_profiles(new_profiles_table,filter_profile_colnames,
                                     supplementary_info_variables,
                                     date_colnames, hours_colnames,
                                     layer_from_colnames, density_final_colnames, SWE_colnames)

# write.csv(profiles_aggreg,paste(output_dir,output_profile_aggregation,sep = ""),na = "NaN",row.names = F)





