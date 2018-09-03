rm(list = ls())

library(devtools)
install_github("bridachristian/SnowProfileProcessing")
library(SnowProfileProcessing)
library(XML)
library(hydroGOF)
library(reshape2)
library(ggplot2)
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
plot_dir_scatterplot = "Scatterplot_density_estim_vs_measur/"
plot_dir_timeseries = "Timeseries_density_estim_vs_measur/"

station_name_name = "Localita"
station_code_name = "Codice.Stazione"
date_name = "Data"
density_measured_name = "density"
density_estimated_name =  "density_from_grain_hardness"

grain_shape1_name = "FomaGrani1"
grain_shape2_name = "FomaGrani2"

new_profiles_table_dir = list_inpt$part3$new_profiles_table_dir
new_profiles_table_file = list_inpt$part3$new_profiles_table_file

# ~~~~~~ Software start here: ~~~~~~~~ 

# ~ ~ ~ ~ Reading Data ~ ~ ~ ~ 

new_profiles_table = read.csv(paste(new_profiles_table_dir, new_profiles_table_file,sep = ""),stringsAsFactors = F )

df_density = data.frame(new_profiles_table[which(colnames(new_profiles_table) == station_name_name)],
                        new_profiles_table[which(colnames(new_profiles_table) == station_code_name)],
                        new_profiles_table[which(colnames(new_profiles_table) == date_name)],
                        new_profiles_table[which(colnames(new_profiles_table) == density_measured_name)],
                        new_profiles_table[which(colnames(new_profiles_table) == density_estimated_name)], 
                        new_profiles_table[which(colnames(new_profiles_table) == grain_shape1_name)],
                        new_profiles_table[which(colnames(new_profiles_table) == grain_shape2_name)], 
                        stringsAsFactors = F)
 
colnames(df_density) = c("Station","station.code","Date", "dens_meas", "dens_estim", "grain1", "grain2")

# ~ ~ ~ ~ Grain classify ~ ~ ~ ~ 

grain_shape1 = as.numeric(substring(text = df_density$grain1,first = 1,last = 1))
grain_shape2 = as.numeric(substring(text = df_density$grain2,first = 1,last = 1))

df_tmp_grains= data.frame(grain_shape1,grain_shape2)

df_grains_ordered = matrix(ncol = 2, nrow = nrow(df_density))
colnames(df_grains_ordered) = c("F_min", "F_max")

for(i in 1:nrow(df_density)){
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

grain_shape3 = rep(NA, times = nrow(df_density))

grain_shape3[which(df_grains_ordered$F_min == df_grains_ordered$F_max)] = df_grains_ordered$F_min[which(df_grains_ordered$F_min == df_grains_ordered$F_max)]
grain_shape3[which(df_grains_ordered$F_min != df_grains_ordered$F_max & df_grains_ordered$F_min == df_grains_ordered$F_max-1 )] = paste(df_grains_ordered$F_min[which(df_grains_ordered$F_min != df_grains_ordered$F_max & df_grains_ordered$F_min == df_grains_ordered$F_max-1 )], "-",df_grains_ordered$F_max[which(df_grains_ordered$F_min != df_grains_ordered$F_max & df_grains_ordered$F_min == df_grains_ordered$F_max-1)],sep = "")
grain_shape3[which(df_grains_ordered$F_min != df_grains_ordered$F_max & df_grains_ordered$F_min != df_grains_ordered$F_max-1 )] = NA
grain_shape3[grain_shape3 ==Inf] = NA

df_density = cbind(df_density,grain_shape3)
colnames(df_density)[8] = "grain_class"

df_new = df_density[which(!is.na(df_density$dens_meas)),]
df_new2 = df_new[which(!is.na(df_new$dens_estim)),]

station_code = as.character(unique(df_new2$station.code))

st =station_code[1]

# ~ ~ ~ ~ Plots: scatterplot and timeseries: observation and estimation of density  ~ ~ ~ ~ 

for (st in station_code){
  
  # ~ ~ ~ ~ Subset data filtering by station ~ ~ ~ ~ 
  
  df_new3 = df_new2[which(df_new2$station.code == st),]
  station_name_ggplot = paste(df_new3$Station[1], "-",st,sep = " ")
  
  # ~ ~ ~ ~ Evaluate etimation: compare with observation ~ ~ ~ ~
  
  rmse = round(x = rmse(obs = df_new3$dens_meas, sim = df_new3$dens_estim),digits = 2)
   
  # ~ ~ ~ ~ Adding default parameter (for ggplot setting) ~ ~ ~ ~
  
  temp_df = df_density[1:17,]
  temp_df$grain_class  = c("1","1-2","2","2-3","3","3-4","4","4-5","5","5-6","6","6-7","7","7-8","8","8-9","9")
  temp_df$dens_meas = rep(-100, times = nrow(temp_df))
  temp_df$dens_estim = rep(-100, times = nrow(temp_df))
  
  df_new3 = rbind(temp_df,df_new3)
  
  # ~ ~ ~ ~ Plot 1: scatterplot observation vs estimation  ~ ~ ~ ~ 
  
  shape_palette  = c(3,4,92,13,20,12,0,14,24,10,1,11,86,89,15,8,6)
  color_palette = c("#00FF00","#11c511","#228B22","#90a072","#FFB6C1","#d6c7d4","#ADD8E6","#5f77f1","#0000FF","#800080","#FF0000","#ff008c","#FF00FF","#738cff","#00FFFF","#8080c0","#FF0080")
  
  g1 = ggplot(df_new3, aes(x = dens_meas, y=dens_estim))+ 
    geom_abline(slope = 1,intercept = 0, color = "red", linetype = 2)+
    # geom_point(aes(shape = grain_class, color = grain_class),size  = 3 )+
    geom_point(aes(color = grain_class),size  = 3 )+
    ggtitle(label = station_name_ggplot, subtitle = "Surveys 2012-2018")+
    scale_x_continuous(name = "Density Measured [kg/m3]",limits = c(0,1100))+
    scale_y_continuous(name = "Density Estimated [kg/m3]",limits = c(0,1100))+
    # scale_shape_manual(values = shape_palette)+
    scale_color_manual(name = "Grain type",values = color_palette)+
    # geom_smooth(method = "lm")+
    theme_bw()+annotate(geom = "text", label = paste("RMSE =", rmse),x = 800,y = 1100 )
  
  g1
  
  # ~ ~ ~ ~ Plot 1: Save in plot_dir_scatterplot folder  ~ ~ ~ ~ 
  
  if(dir.exists(paste(plot_dir,plot_dir_scatterplot,sep = ""))){
    ggsave(paste(plot_dir,plot_dir_scatterplot, station_name_ggplot,".png",sep = ""), width = 40, height = 20, units = "cm")
  }else{
    dir.create(paste(plot_dir,plot_dir_scatterplot,sep = ""))
    ggsave(paste(plot_dir,plot_dir_scatterplot, station_name_ggplot,".png",sep = ""), width = 40, height = 20, units = "cm")
  }
  
  # ~ ~ ~ ~ Plot 2: prepare data and classify season  ~ ~ ~ ~ 
  
  longdata = melt(df_new3)
  longdata$Date = as.POSIXct(longdata$Date, tz = "Etc/GMT-1")
  month = as.numeric(format(longdata$Date, format = "%m"))
  year = as.numeric(format(longdata$Date, format = "%Y"))
  
  season = rep(NA, times = nrow(longdata))
  
  season[which(month %in% c(10,11,12))] = paste(year[which(month %in% c(10,11,12))],year[which(month %in% c(10,11,12))]+1, sep = "-")
  season[which(month %in% c(1,2,3,4,5,6))] = paste(year[which(month %in% c(1,2,3,4,5,6))]-1,year[which(month %in% c(1,2,3,4,5,6))], sep = "-")
  
  newDate = format(x = longdata$Date, format = "%m-%d")
  
  newDate[which(month %in% c(10,11,12))] = paste("2015-",newDate[which(month %in% c(10,11,12))],sep = "")
  newDate[which(month %in% c(1,2,3,4,5,6))] = paste("2016-",newDate[which(month %in% c(1,2,3,4,5,6))],sep = "")
  
  newDate = as.POSIXct(x = newDate,format = "%Y-%m-%d", tz = "Etc/GMT-1")
  longdata = cbind(longdata,season,newDate)
  longdata = longdata[!is.na(longdata$Date),]
  
  # ~ ~ ~ ~ Adding default parameter (for ggplot setting)  ~ ~ ~ ~ 
  
  df_longdata_out =  longdata[1:6,]
  df_longdata_out$Date = rep(as.POSIXct("1990-01-01 00:00",tz = "Etc/GMT-1")) 
  df_longdata_out$variable = rep(c("dens_meas", "dens_estim"))
  df_longdata_out$value = rep(NA)
  df_longdata_out$season = as.factor(c("2012-2013","2013-2014","2014-2015","2015-2016","2016-2017","2017-2018"))  
  df_longdata_out$newDate = rep(as.POSIXct("1990-01-01", tz = "Etc/GMT-1"))
  
  longdata = rbind(longdata, df_longdata_out)
  
  start_plot = as.POSIXct("2015-11-01", tz = "Etc/GMT-1")
  end_plot = as.POSIXct("2016-06-15", tz = "Etc/GMT-1")
  
  # ~ ~ ~ ~ Plot 2: timeseries observation vs estimation  ~ ~ ~ ~ 
  
  g2 = ggplot(longdata,aes(x = newDate, y = value,shape = variable))+theme_bw()+
    geom_point(aes(color = grain_class),size = 3)+  facet_wrap(~season) +
    ggtitle(label = station_name_ggplot, subtitle = "Surveys 2012-2018")+
    xlab("Date")+
    ylab("Density [kg/cm3]")+
    scale_x_datetime(limits = c(start_plot,end_plot), date_breaks = "2 months", date_minor_breaks = "1 month",date_labels = "%b %d")+
    scale_y_continuous(limits = c(0,1100),breaks = seq(from = 0, to = 1100, by = 200))+
    scale_color_manual(name = "Grain type",values = color_palette)+
    scale_shape_manual(values = c(4,20), name = "Density",labels = c("Measured", "Estimated"))
    

  g2
  
  # ~ ~ ~ ~ Plot 2: Save in plot_dir_scatterplot folder  ~ ~ ~ ~ 
  
  if(dir.exists(paste(plot_dir,plot_dir_timeseries,sep = ""))){
    ggsave(paste(plot_dir,plot_dir_timeseries, station_name_ggplot,".png",sep = ""), width = 40, height = 20, units = "cm")
  }else{
    dir.create(paste(plot_dir,plot_dir_timeseries,sep = ""))
    ggsave(paste(plot_dir,plot_dir_timeseries, station_name_ggplot,".png",sep = ""), width = 40, height = 20, units = "cm")
  }
}

