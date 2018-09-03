rm(list = ls())

library(devtools)
install_github("bridachristian/SnowProfileProcessing")
library(SnowProfileProcessing)
library(XML)
library(hydroGOF)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(readxl)
library(ggplot2)
library(plyr)
library(reshape2)
library(lubridate)
library(RColorBrewer)

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

rm(list =setdiff(ls(),c("list_inpt","output_dir")))

# ~ ~ ~ ~ Input Section ~ ~ ~ ~ 

plot_dir = "../Plots/"

plot_dir_timeseries_SWE_rho_HS = "Timeseries_HS_SWE_rho/"

station_name_name = "Localita"
station_code_name = "Codice.Stazione"
date_name = "Data"
snow_height_name = "HS"
density_name =  "Density_Avg"
SWE_name =  "SWE_tot"
reconstr_flag_name =  "Flag_reconstruct"
fraction_miss_name =  "Fraction_missing_layer"

agg_profiles_table_dir = output_dir
agg_profiles_table_file = list_inpt$part3$output_profile_aggregation

# ~~~~~~ Software start here: ~~~~~~~~ 
agg_profiles_table = read.csv(paste(agg_profiles_table_dir, agg_profiles_table_file,sep = ""),stringsAsFactors = F )

station_name_df = unique(agg_profiles_table[,which(colnames(agg_profiles_table) %in% c(station_name_name,station_code_name))])
station_name = station_name_df[,which(colnames(station_name_df) == station_name_name)]
station_code = station_name_df[,which(colnames(station_name_df) == station_code_name)]

point_palette =(RColorBrewer::brewer.pal(10,"RdYlGn"))

s = 1
for(s in 1:length(station_code)){
  
  st_name = station_name_df[s, which(colnames(station_name_df) == station_name_name)]
  st_code = station_name_df[s, which(colnames(station_name_df) == station_code_name)]
  
  w_st = which(agg_profiles_table[,which(colnames(agg_profiles_table) == station_code_name)] == station_name_df[s, which(colnames(station_name_df) == station_code_name)])
  
  imported_table_w_st = agg_profiles_table[w_st,]
  
  station_name_ggplot = paste(st_name,st_code, sep = " - ")
  
  timeseries = data.frame(imported_table_w_st[, which(colnames(agg_profiles_table) == date_name)],
                          imported_table_w_st[, which(colnames(agg_profiles_table) == snow_height_name)],
                          imported_table_w_st[, which(colnames(agg_profiles_table) == SWE_name)],
                          imported_table_w_st[, which(colnames(agg_profiles_table) == density_name)])
  
  colnames(timeseries) = c("Date", "HS", "SWE", "rho")
  
  additional_info = data.frame(imported_table_w_st[, which(colnames(agg_profiles_table) == reconstr_flag_name)],
                               imported_table_w_st[, which(colnames(agg_profiles_table) == fraction_miss_name)])
  
  colnames(additional_info) = c("flag", "Missing_fraction")
  
  
  additional_info$flag[additional_info$flag == 1] = "Estimated"
  additional_info$flag[additional_info$flag == 0] = "Measured"
  
  Percentage = 1-additional_info$Missing_fraction
  additional_info = cbind(additional_info, Percentage)
  additional_info = additional_info[,-2]
  
  ts_melt = melt(timeseries)
  
  df1 = data.frame(rep("Measured", times = nrow(timeseries)),rep(1, times = nrow(timeseries)))
  colnames(df1) = colnames(additional_info)
  
  add_melt = rbind(df1,additional_info,additional_info)
  
  ts_melt = cbind(ts_melt,add_melt)
  
  ts_melt$Date = as.Date(ts_melt$Date, format = "%Y-%m-%d") 
  ts_melt$Date = as.POSIXct(ts_melt$Date, format = "%Y-%m-%d",tz ="Etc/GMT-1") 
  
  
  month = as.numeric(format(ts_melt$Date, format = "%m"))
  year = as.numeric(format(ts_melt$Date, format = "%Y"))
  
  season = rep(NA, times = nrow(ts_melt))
  
  season[which(month %in% c(10,11,12))] = paste(year[which(month %in% c(10,11,12))],year[which(month %in% c(10,11,12))]+1, sep = "-")
  season[which(month %in% c(1,2,3,4,5,6))] = paste(year[which(month %in% c(1,2,3,4,5,6))]-1,year[which(month %in% c(1,2,3,4,5,6))], sep = "-")
  
  newdatetime = format(x = ts_melt$Date, format = "%m-%d")
  
  newdatetime[which(month %in% c(10,11,12))] = paste("2015-",newdatetime[which(month %in% c(10,11,12))],sep = "")
  newdatetime[which(month %in% c(1,2,3,4,5,6))] = paste("2016-",newdatetime[which(month %in% c(1,2,3,4,5,6))],sep = "")
  
  newdatetime = as.POSIXct(x = newdatetime,format = "%Y-%m-%d", tz = "Etc/GMT-1")
  
  ts_melt = cbind(ts_melt,season,newdatetime)
  
  ts_melt = ts_melt[!is.na(ts_melt$Date),]
  
  labs = c(`HS` = "Snow Height [cm]", `SWE` = "SWE [mm]", `rho` = "Density [kg/m3]")
  
  
  df_ts_melt_out =  ts_melt[1:6,]
  df_ts_melt_out$Date = rep(as.POSIXct("1990-01-01 01:00",tz = "Etc/GMT-1")) 
  df_ts_melt_out$variable = rep(c("HS", "rho", "SWE"), times = 2)
  df_ts_melt_out$value = rep(-100, times =  6)
  df_ts_melt_out$flag = rep(c("Measured", "Estimated"), times = 3)
  df_ts_melt_out$Percentage = rep(0, times = 6)
  df_ts_melt_out$season = as.factor(c("2012-2013","2013-2014","2014-2015","2015-2016","2016-2017","2017-2018"))
  df_ts_melt_out$newdatetime = rep(as.POSIXct("1990-01-01", tz = "Etc/GMT-1"))
  
  ts_melt = rbind(ts_melt,df_ts_melt_out)
  
  ts_melt$flag = as.factor(ts_melt$flag)
  
  start_plot = as.POSIXct("2015-10-01", tz = "Etc/GMT-1")
  end_plot = as.POSIXct("2016-06-15", tz = "Etc/GMT-1")
  
  p2 = ggplot(ts_melt,aes(x = newdatetime, y = value))+theme_bw()+
    geom_line(aes(colour = variable),size = 1)+
    geom_point(aes(shape = flag, fill = Percentage),size = 2)+
    facet_wrap(~season) +
    labs(title = station_name_ggplot,
         subtitle = "Surveys 2012-2018")+
    xlab("Date")+
    ylab("SWE [mm] - HS[cm] - rho [Kg/m3]")+
    scale_x_datetime(limits = c(start_plot,end_plot), date_breaks = "2 months", date_minor_breaks = "1 month",date_labels = "%b %d")+
    scale_y_continuous(limits = c(0,1300),breaks = seq(from = 0, to = 1300, by = 200))+
    scale_colour_manual(name = "Variables",values = c("#377EB8","#999999","#F781BF"), breaks = c("HS", "rho", "SWE"), limits =c("HS", "rho", "SWE"))+
    scale_shape_manual(name = "Orig./Reconst.",values = c(4,21), limits = c("Measured", "Estimated"))+
    scale_fill_gradientn(name = "Quality",colours = point_palette,limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
    guides(color = guide_legend(order=1),
           size = guide_legend(order=3,reverse = T),
           shape = guide_legend(order=2))
  
  p2
  
  if(dir.exists(paste(plot_dir,plot_dir_timeseries_SWE_rho_HS,sep = ""))){
    ggsave(paste(plot_dir,plot_dir_timeseries_SWE_rho_HS, station_name_ggplot,".png",sep = ""), width = 40, height = 20, units = "cm")
  }else{
    dir.create(paste(plot_dir,plot_dir_timeseries_SWE_rho_HS,sep = ""))
    ggsave(paste(plot_dir,plot_dir_timeseries_SWE_rho_HS, station_name_ggplot,".png",sep = ""), width = 40, height = 20, units = "cm")
  }
  
}
