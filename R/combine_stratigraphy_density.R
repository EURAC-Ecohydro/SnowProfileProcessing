#' combine_stratigraphy_density
#' 
#' @description combine_stratigraphy_density() is a function that combine stratigraphy table and density table. The script joins the two table by datetime and station, centering density inside the proper layer
#' 
#' @usage combine_stratigraphy_density(input_dir, stratigraphy_file, density_file, dates_colnames, hours_colnames, station_code)
#' 
#' @param input_dir Folder where stratigraphy_file and density file are stored
#' @param stratigraphy_file A file .csv containing stratigraphy for each layer and each profile
#' @param density_file A file .csv containing density for each layer and each profile
#' @param dates_colnames The name of column associated to Dates
#' @param hours_colnames The name of column associated to Hours
#' @param station_code The name of column associated to Station identifier
#' 
#' @return A dataframe that is the combiantion of stratigraphy file and the density sample extract in density_file
#' 
#' @example See an example in inst/examples_profile_analysis.R


combine_stratigraphy_density = function(input_dir, stratigraphy_file, density_file, dates_colnames, hours_colnames, station_code){
  #~~~~~~ Import Data ~~~~~~~~ 
  
  strati = read.csv(paste(input_dir, stratigraphy_file, sep = ""), stringsAsFactors = F)
  densita = read.csv(paste(input_dir, density_file, sep = ""), stringsAsFactors = F)
  
  #~~~~~~ Strati ~~~~~~~~ 
  # s_year <- substring(strati$Data,1,4)
  # s_month <- substring(strati$Data,6,7)
  # s_day <- substring(strati$Data,9,10)
  
  s_year <- substring(strati[,which(colnames(strati) == dates_colnames)],1,4)
  s_month <- substring(strati[,which(colnames(strati) == dates_colnames)],6,7)
  s_day <- substring(strati[,which(colnames(strati) == dates_colnames)],9,10)
  
  s_ora_length = nchar(strati[,which(colnames(strati) == hours_colnames)])
  short_hourmin = which(s_ora_length == 3)
  long_hourmin = which(s_ora_length == 4)
  zero_hourmin = which(s_ora_length == 1)
  
  s_hour = rep(NA, length(s_ora_length))
  s_min = rep(NA, length(s_ora_length))
  
  s_hour[short_hourmin] <- paste("0",substring(strati[short_hourmin,which(colnames(strati) == hours_colnames)],1,1),sep = "")
  s_hour[long_hourmin] <- substring(strati[long_hourmin,which(colnames(strati) == hours_colnames)],1,2)
  s_hour[zero_hourmin] <- paste("00")
  
  s_min[short_hourmin] <- substring(strati[short_hourmin,which(colnames(strati) == hours_colnames)],2,3)
  s_min[long_hourmin] <- substring(strati[long_hourmin,which(colnames(strati) == hours_colnames)],3,4)
  s_min[zero_hourmin] <- paste("00")
  
  s_datetime_chr = paste(s_year,"-",s_month,"-",s_day," ",s_hour,":",s_min,":00",sep = "")
  
  s_datetime <- as.POSIXct(strptime(x = s_datetime_chr, format = "%Y-%m-%d %H:%M:%S"), tz = 'Etc/GMT-1')
  
  station_code
  
  # s_datetime_staz = paste(s_datetime,strati$Codice.Stazione,sep = "_")
  s_datetime_staz = paste(s_datetime,strati[,which(colnames(strati) == station_code)],sep = "_")
  s_surveys_filter=unique(s_datetime_staz)
  
  strati = cbind(s_datetime,s_datetime_staz, strati)
  colnames(strati)[1] = "Datetime"
  colnames(strati)[2] = "Filter_code"
  
  s_survey = list()
  
  for(i in 1: length(s_surveys_filter)){
    s_survey[[i]] = strati[which(s_datetime_staz == s_surveys_filter[i]),]
  }
  names(s_survey) = s_surveys_filter
  
  #~~~~~~ Density ~~~~~~~~ 
  d_year <- substring(densita[,which(colnames(densita) == dates_colnames)],1,4)
  d_month <- substring(densita[,which(colnames(densita) == dates_colnames)],6,7)
  d_day <- substring(densita[,which(colnames(densita) == dates_colnames)],9,10)
  
  d_ora_length = nchar(densita[,which(colnames(densita) == hours_colnames)])
  short_hourmin = which(d_ora_length == 3)
  long_hourmin = which(d_ora_length == 4)
  zero_hourmin = which(d_ora_length == 1)
  
  d_hour = rep(NA, length(d_ora_length))
  d_min = rep(NA, length(d_ora_length))
  
  # d_hour[short_hourmin] <- paste("0",substring(densita$Ora[short_hourmin],1,1),sep = "")
  d_hour[short_hourmin] <- paste("0",substring(densita[short_hourmin,which(colnames(densita) == hours_colnames)],1,1),sep = "")
  d_hour[long_hourmin] <- substring(densita[long_hourmin,which(colnames(densita) == hours_colnames)],1,2)
  d_hour[zero_hourmin] <- paste("00")
  
  d_min[short_hourmin] <- substring(densita[short_hourmin,which(colnames(densita) == hours_colnames)],2,3)
  d_min[long_hourmin] <- substring(densita[long_hourmin,which(colnames(densita) == hours_colnames)],3,4)
  d_min[zero_hourmin] <- paste("00")
  
  d_datetime_chr = paste(d_year,"-",d_month,"-",d_day," ",d_hour,":",d_min,":00",sep = "")
  
  d_datetime <- as.POSIXct(strptime(x = d_datetime_chr, format = "%Y-%m-%d %H:%M:%S"), tz = 'Etc/GMT-1')
  
  d_datetime_staz = paste(d_datetime,densita[,which(colnames(densita) == station_code)],sep = "_")
  d_surveys_filter=unique(d_datetime_staz)
  
  densita = cbind(d_datetime,d_datetime_staz, densita)
  colnames(densita)[1] = "Datetime"
  colnames(densita)[2] = "Filter_code"
  
  d_survey = list()
  
  for(i in 1: length(d_surveys_filter)){
    d_survey[[i]] = densita[which(d_datetime_staz == d_surveys_filter[i]),]
  }
  names(d_survey) = d_surveys_filter
  
  
  #~~~~~~ compare ~~~~~~~~ 
  
  
  w = which(s_surveys_filter %in% d_surveys_filter)
  
  s_survey_short = s_survey[w]
  
  s_survey_short = s_survey_short[order(names(s_survey_short))]
  d_survey = d_survey[order(names(d_survey))]
  
  # paste(substring(names(s_survey_short),1,10)[grep(names(s_survey_short),pattern = "PN")],"-->",grep(names(s_survey_short),pattern = "PN"))
  
  # test survey 1
  k = 1 
  for(k in 1:length(s_survey_short)){
    
    # names(s_survey_short)[[k]]
    # names(d_survey)[[k]]
    
    s1 = s_survey_short[[k]]
    d1 = d_survey[[k]]
    
    
    w_d1 = c()
    for(i in 1: nrow(d1)){
      w2 = which(d1$H[i] < s1$Da & d1$H[i] > s1$A)
      w_d1 = c(w_d1,w2)
    }
    
    
    number_strati = nrow(s1)
    
    density = rep(NA, times  = number_strati)
    
    density[w_d1] = d1$Densita
    
    s1 = cbind(s1,density)
    
    s_survey_short[[k]] = s1
    
  }
  
  merged_table = do.call(rbind.data.frame, s_survey_short)
  rownames(merged_table)=seq(from = 1, to = nrow(merged_table), by  = 1)
  
  return(merged_table)
}



