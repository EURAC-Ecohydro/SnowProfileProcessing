#' aggregate_profiles
#' 
#' @description aggregate_profiles() is a function that aggregate by date and station snow profiles obtaining a information of Snow Height total, mean snow density and Snow Water equivalent for each profile.
#' We suggest to aggregate profiles after a preprocessing using other functions in SnowProfilesProcessing package
#' 
#' @usage aggregate_profiles(new_profiles_table, filter_profile_colnames, supplementary_info_variables, date_colnames, hours_colnames, layer_from_colnames, density_final_colnames, SWE_colnames)
#' 
#' @param new_profiles_table Table of snow profiles having Stratigraphy and Density
#' @param filter_profile_colnames Name of column of new_profiles_table to use to aggregate profiles (Create before new_profiles_table with other functions in this package)
#' @param supplementary_info_variables A vector containing the names of columns to return in output table (Exclude Snow Height, Snow Density and Snow Water Equivalent)
#' @param date_colnames Name of column associated to Dates
#' @param hours_colnames Name of column associated to Hours
#' @param layer_from_colnames Name of column associated to Max Snow Height Layer
#' @param density_final_colnames Name of column associated to layer snow density
#' @param SWE_colnames Name of column associated to layer Snow Water Equivalent
#' 
#' @return A dataframe having that summarise the max snow height, the meand snow density and the total snow water equivalent for each surveys executed
#'  
#' @examples See an example in inst/examples_profile_analysis.R
   
aggregate_profiles = function(new_profiles_table, filter_profile_colnames,
                              supplementary_info_variables,
                              date_colnames, hours_colnames,
                              layer_from_colnames, density_final_colnames, SWE_colnames,
                              recontrustction_flag_colnames,thickness_colnames){
  
  new_profiles_table[, which(colnames(new_profiles_table) == filter_profile_colnames)] = as.factor(new_profiles_table[, which(colnames(new_profiles_table) == filter_profile_colnames)])
  
  vect_colnames = c(filter_profile_colnames,supplementary_info_variables,
                    date_colnames, hours_colnames,
                    layer_from_colnames, density_final_colnames, SWE_colnames,
                    recontrustction_flag_colnames,thickness_colnames)
  
  table_short = new_profiles_table[which(colnames(new_profiles_table) %in% vect_colnames)]
  
  
  aggregate_short_unique = aggregate(table_short[,-c(which(colnames(table_short) %in% c(layer_from_colnames,density_final_colnames,SWE_colnames, recontrustction_flag_colnames,thickness_colnames)))],
                                     by = list(table_short[,which(colnames(table_short) == filter_profile_colnames)]), FUN = unique)[,-1]
  
  
  
  HS = aggregate(table_short[,which(colnames(table_short) == layer_from_colnames)], by = list(table_short[,which(colnames(table_short) == filter_profile_colnames)]), FUN = max)[2]
  colnames(HS) = "HS"
  
  SWE = aggregate(table_short[,which(colnames(table_short) == SWE_colnames)], by = list(table_short[,which(colnames(table_short) == filter_profile_colnames)]), FUN = function(x) sum(x, na.rm = T))[2]
  colnames(SWE) = "SWE_tot"
  
  dens = 100*SWE/HS
  colnames(dens) = "Density_Avg"
  
  sum_reconstr_flag = aggregate(table_short[,which(colnames(table_short) == recontrustction_flag_colnames)], by = list(table_short[,which(colnames(table_short) == filter_profile_colnames)]), FUN = function(x) sum(x, na.rm = T))[2]
  reconstr_flag = sum_reconstr_flag
  reconstr_flag[reconstr_flag!= 0] = 1
  colnames(reconstr_flag) = "Flag_reconstruct"

  thick_reconstr  = table_short[,which(colnames(table_short) == thickness_colnames)] * table_short[,which(colnames(table_short) == recontrustction_flag_colnames)]
  tot_thickness = aggregate(thick_reconstr, by = list(table_short[,which(colnames(table_short) == filter_profile_colnames)]), FUN = function(x) sum(x, na.rm = T))[2]
  percent = tot_thickness/ HS
  
  colnames(percent) = "Fraction_missing_layer"
  
  total_file = data.frame(aggregate_short_unique,HS, dens, SWE,reconstr_flag,percent)
  
  total_file$Ora[nchar(total_file$Ora)<4] = paste0(sapply(X = 4-nchar(total_file$Ora[nchar(total_file$Ora)<4]),FUN = function(x) paste0(rep("0",times = x),collapse = "")),total_file$Ora[nchar(total_file$Ora)<4])
  total_file$Ora = paste(substring(total_file$Ora,1,2),":",substring(total_file$Ora,3,4),sep = "")
    
  return(total_file)
}

## When passing a single vector, paste0 and paste work like as.character.
  


