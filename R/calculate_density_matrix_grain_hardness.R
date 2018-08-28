#' calculate_density_matrix_grain_hardness
#' 
#' @description calculate_density_matrix_grain_hardness() is a function that combine grain shapes and hardness to estimate density. The method following paper Valt e Cagnati, "Stima della densita della neve conoscendo la forma dei grani e la durezza", 2005 Neve e Valanghe,55 pag. 40-45 
#' 
#' @usage calculate_density_matrix_grain_hardness (merged_table, grain_shape1_colnames, grain_shape2_colnames, density_colnames, hardness_colnames)
#' 
#' @param merged_table A table containing stratigraphy and density information for each layer
#' @param grain_shape1_colnames The name of column associated to grain shape 1
#' @param grain_shape1_colnames The name of column associated to grain shape 2
#' @param density_colnames The name of column associated to Density
#' @param hardness_colnames The name of column associated to Hardness
#' 
#' @return A list of 3 elements. The first element is a dataframe of estimated density for each pairing of grain shape and density,
#' The second element is a dataframe of number of observation for each pairing of grain shape and density
#' The third element is a dataframe of standard deviation of density for each pairing of grain shape and density
#' 
#' @example See an example in inst/examples_profile_analysis.R




calculate_density_matrix_grain_hardness = function(merged_table, grain_shape1_colnames, grain_shape2_colnames, density_colnames, hardness_colnames){
  
  grain_shape1 = as.numeric(substring(text = merged_table[, which(colnames(merged_table) == grain_shape1_colnames)],first = 1,last = 1))
  grain_shape2 = as.numeric(substring(text = merged_table[, which(colnames(merged_table) == grain_shape2_colnames)],first = 1,last = 1))
  
  density_measured = as.numeric(merged_table[,which(colnames(merged_table) == density_colnames)])
  
  hardness = as.numeric(merged_table[,which(colnames(merged_table) == hardness_colnames)])
  
  df_grains_hardness_density= data.frame(grain_shape1,grain_shape2,hardness,density_measured)
  
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
  df_grains_hardness_density =  cbind(df_grains_hardness_density, grain_shape3)
  
  
  density_matrix_grain_hardness = matrix(ncol = 6, nrow = 3*length(levels(df_grains_hardness_density$grain_shape3)))
  
  rownames(density_matrix_grain_hardness) =paste(rep(levels(df_grains_hardness_density$grain_shape3),each = 3),rep(c("N","Mean", "Sd"),times = length(levels(df_grains_hardness_density$grain_shape3))),sep = "_")
  colnames(density_matrix_grain_hardness) = paste("R_",seq(1,6,1),sep = "")
  
  
  i=1
  k=1
  vect_F = levels(df_grains_hardness_density$grain_shape3)
  
  for(i in 1: ncol(density_matrix_grain_hardness)){
    for(k in 1:(nrow(density_matrix_grain_hardness)/3)){
      st = 3*k-3+1
      density_matrix_grain_hardness[st,i] = length(df_grains_hardness_density$density_measured[ which( df_grains_hardness_density$grain_shape3 == vect_F[k] & df_grains_hardness_density$hardness == i & !is.na(df_grains_hardness_density$density_measured))])
      density_matrix_grain_hardness[st+1,i] = mean(df_grains_hardness_density$density_measured[which(df_grains_hardness_density$grain_shape3 == vect_F[k]  & df_grains_hardness_density$hardness == i & !is.na(df_grains_hardness_density$density_measured))],na.rm = T)
      density_matrix_grain_hardness[st+2,i] = sd(df_grains_hardness_density$density_measured[which(df_grains_hardness_density$grain_shape3 == vect_F[k]  & df_grains_hardness_density$hardness == i & !is.na(df_grains_hardness_density$density_measured))],na.rm = T)
    }
  }    
  
  density_matrix_grain_hardness_N = as.data.frame(density_matrix_grain_hardness[seq(from = 1, to=nrow(density_matrix_grain_hardness), by = 3),])
  colnames(density_matrix_grain_hardness_N)  = paste("N.", colnames(density_matrix_grain_hardness_N), sep = "")
  rownames(density_matrix_grain_hardness_N) = paste("Grain shape",vect_F)
  
  density_matrix_grain_hardness_avg = as.data.frame(density_matrix_grain_hardness[seq(from = 2, to=nrow(density_matrix_grain_hardness), by = 3),])
  colnames(density_matrix_grain_hardness_avg)  = paste("Avg.", colnames(density_matrix_grain_hardness_avg), sep = "")
  rownames(density_matrix_grain_hardness_avg) = paste("Grain shape",vect_F)
  
  density_matrix_grain_hardness_sd = as.data.frame(density_matrix_grain_hardness[seq(from = 3, to=nrow(density_matrix_grain_hardness), by = 3),])
  colnames(density_matrix_grain_hardness_sd)  = paste("Sd.", colnames(density_matrix_grain_hardness_sd), sep = "")
  rownames(density_matrix_grain_hardness_sd) = paste("Grain shape",vect_F)
  
  density_matrix_grain_hardness_final = cbind(density_matrix_grain_hardness_avg,density_matrix_grain_hardness_N,density_matrix_grain_hardness_sd)
  
  return(list(density_matrix_grain_hardness_avg,density_matrix_grain_hardness_N,density_matrix_grain_hardness_sd))
}
