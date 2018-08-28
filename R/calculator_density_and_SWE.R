#' calculator_density_and_SWE
#' 
#' @description calculator_density_and_SWE() is a function that reconstruct snow density for each layer if it is missing using an external matrix of snow density for each pairing of grain shape and hardness (Valt et al.,2005)   
#'
#' @usage calculator_density_and_SWE (merged_table, SWE_grain_table, grain_shape1_colnames, grain_shape2_colnames, density_colnames, hardness_colnames, layer_from_colnames, layer_to_colnames,filter_profile_colnames){
#' 
#' @param merged_table A table containing stratigraphy and density information for each layer
#' @param SWE_grain_table A table containing snow density for each pairing of grain shape and hardness
#' @param grain_shape1_colnames The name of column of merged_table associated to grain shape 1
#' @param grain_shape1_colnames The name of column of merged_table associated to grain shape 2
#' @param density_colnames The name of column of merged_table associated to Density
#' @param hardness_colnames The name of column of merged_table associated to Hardness
#' @param layer_from_colnames The name of column of merged_table associated to the top of layer
#' @param layer_to_colnames The name of column of merged_table associated to the bottom of layer
#' @param filter_profile_colnames The name of column of merged_tableassociated to filtration code (Usually datetime + station code. We suggest to create merged_table with other funciton in this package)
#' 
#' @return A list of 3 elements. The first element is a dataframe of estimated density for each pairing of grain shape and density,
#' The second element is a dataframe of number of observation for each pairing of grain shape and density
#' The third element is a dataframe of standard deviation of density for each pairing of grain shape and density
#' 
#' @example See an example in inst/examples_profile_analysis.R

calculator_density_and_SWE = function(merged_table, SWE_grain_table, grain_shape1_colnames,
                                      grain_shape2_colnames, density_colnames, hardness_colnames, 
                                      layer_from_colnames, layer_to_colnames,filter_profile_colnames){
  
  
  
  colnames(SWE_grain_table) = SWE_grain_table[1,]
  SWE_grain_table = SWE_grain_table[-1,]
  rownames(SWE_grain_table) = SWE_grain_table[,1]
  SWE_grain_table = SWE_grain_table[,-1]
  rrr = rownames(SWE_grain_table) 
  SWE_grain_table <- data.frame(apply(SWE_grain_table, 2, function(x) as.numeric(as.character(x))))
  rownames(SWE_grain_table)= rrr 
  
  
  # ########################
  
  hardness = as.numeric(merged_table[,which(colnames(merged_table) == hardness_colnames)])
  
  grain_shape1 = as.numeric(substring(text = merged_table[, which(colnames(merged_table) == grain_shape1_colnames)],first = 1,last = 1))
  grain_shape2 = as.numeric(substring(text = merged_table[, which(colnames(merged_table) == grain_shape2_colnames)],first = 1,last = 1))
  
  
  df_grains_hardness_density= data.frame(grain_shape1,grain_shape2,hardness)
  
  df_tmp_grains = df_grains_hardness_density[,c(1,2)]
  
  df_grains_ordered = matrix(ncol = 2, nrow = nrow(df_grains_hardness_density))
  
  df_grains_ordered = matrix(ncol = 2, nrow = nrow(df_grains_hardness_density))
  colnames(df_grains_ordered) = c("F_min", "F_max")
  
  for(i in 1:nrow(df_grains_hardness_density)){
    if(is.na(df_tmp_grains[i,1]) | is.na(df_tmp_grains[i,2])){
      df_grains_ordered[i,1] = min(df_tmp_grains[i,],na.rm = T)
      df_grains_ordered[i,2] = min(df_tmp_grains[i,],na.rm = T)
    }else{
      df_grains_ordered[i,1] = min(df_tmp_grains[i,],na.rm = T)
      df_grains_ordered[i,2] = max(df_tmp_grains[i,],na.rm = T)
    }
  }
  
  df_grains_ordered = as.data.frame(df_grains_ordered)
  df_grains_ordered[df_grains_ordered == Inf]  = NA
  
  grain_shape3 = rep(NA, times = nrow(df_grains_hardness_density))
  
  grain_shape3[which(df_grains_ordered$F_min == df_grains_ordered$F_max)] = df_grains_ordered$F_min[which(df_grains_ordered$F_min == df_grains_ordered$F_max)]
  grain_shape3[which(df_grains_ordered$F_min != df_grains_ordered$F_max & df_grains_ordered$F_min == df_grains_ordered$F_max-1 )] = paste(df_grains_ordered$F_min[which(df_grains_ordered$F_min != df_grains_ordered$F_max & df_grains_ordered$F_min == df_grains_ordered$F_max-1 )], "-",df_grains_ordered$F_max[which(df_grains_ordered$F_min != df_grains_ordered$F_max & df_grains_ordered$F_min == df_grains_ordered$F_max-1)],sep = "")
  grain_shape3[which(df_grains_ordered$F_min != df_grains_ordered$F_max & df_grains_ordered$F_min != df_grains_ordered$F_max-1 )] = NA
  grain_shape3[grain_shape3 ==Inf] = NA
  
  
  
  #######################
  # snow_grain1 = substring(merged_table[,which(colnames(merged_table) == grain_shape1_colnames)],1,1)
  # snow_grain2 = substring(df_new$FomaGrani2,1,1)
  # hand_test = df_new$TestMano
  
  # rho_estim = data.frame(rep(NA,times = nrow(merged_table)),rep(NA,times = nrow(merged_table)))
  # colnames(rho_estim) = c("rho_grain1", "rho_grain2") 
  
  rho_estim = rep(NA,times = nrow(merged_table))
  
  # for(i in 1: nrow(merged_table)){
  #   rho_estim[i,1] = SWE_grain_table[which(SWE_grain_table$`Grani_Mano` == snow_grain1[i]), which(colnames(SWE_grain_table)==hand_test[i])]
  #   rho_estim[i,2] = SWE_grain_table[which(SWE_grain_table$`Grani_Mano` == snow_grain2[i]), which(colnames(SWE_grain_table)==hand_test[i])]
  # }
  
  for(i in 1: nrow(merged_table)){
    if(is.na(grain_shape3[i]) | is.na(hardness[i])){
      rho_estim[i] = NA
    }else{
      rho_estim[i] = SWE_grain_table[which(substring(rownames(SWE_grain_table),13,nchar(SWE_grain_table)) == grain_shape3[i]),
                                     which(substring(colnames(SWE_grain_table),10,nchar(SWE_grain_table)) == hardness[i])]
    }
  }
  
  rho_avg = as.numeric(rho_estim)
  
  w3 = which(!is.na(merged_table[,which(colnames(merged_table) == density_colnames)]))
  rho_avg_new = rho_avg
  rho_avg_new [w3] = NA
  
  merged_table = cbind(merged_table,rho_avg,rho_avg_new)
  colnames(merged_table)[c(ncol(merged_table)-1,ncol(merged_table))]  = c("density_from_grain_hardness","density_estimated")
  
  density_final= apply(X = data.frame(merged_table[,which(colnames(merged_table) == density_colnames)],merged_table$density_estimated),MARGIN = 1,FUN = function(x) mean(x, na.rm = T)) 
  merged_table = cbind(merged_table,density_final)
  
  layer_from_colnames = "Da"
  layer_to_colnames = "A"
  
  SWE_calc = ((merged_table[,which(colnames(merged_table) == layer_from_colnames)]-merged_table[,which(colnames(merged_table) == layer_to_colnames)])/100)*merged_table$density_final
  SWE_calc[which(is.na(SWE_calc))] = NA
  
  merged_table = cbind(merged_table,SWE_calc)
  
  # filter_profile_colnames
  
  merged_table[,which(colnames(merged_table) == filter_profile_colnames)] = factor(merged_table[,which(colnames(merged_table) == filter_profile_colnames)])
  
  merged_table_new = merged_table
  
  return(merged_table_new)
  
}