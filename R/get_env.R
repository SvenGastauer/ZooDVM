#' Get environmental data from netCDF file
#' @description Currently reads in Temperature, Salinity and fluorescence information as recorded by the Zooglider CTD
#' @param ncpath folder that contains the netCDF files
#' @param mission name or index number of the mission to be used
#' @export
#' @import  ncdf4
#' @return dataframe contianing the temperature, salinity and fluorescence data per dive and depth interval
#' @author Sven Gastauer
#'
get_env <- function(ncpath, mission){
  if (is.numeric(mission)){
    mission = get_mission(ncpath, ending='*.nc', patterns=NULL)[mission]
  }
  message(Sys.time(),': Selected mission - ',mission)

  nc_data <- ncdf4::nc_open(paste0(ncpath, '/' ,mission))

  message(Sys.time(),': Getting GPS')
  t_end <- ncvar_get(nc_data, "gps/UTC_time_fix_end")
  ori = strsplit(ncatt_get(nc_data, "gps/UTC_time_fix_end")$units,'seconds since ')[[1]][2]
  t_end = as.POSIXct(t_end,origin=ori)
  attr(t_end,'tzone') = 'UTC'

  tmp <- ncvar_get(nc_data, "Environment/temperature")
  sal <- ncvar_get(nc_data, "Environment/salinity")
  fl <- ncvar_get(nc_data, "Environment/fluorescence")
  dep <- ncvar_get(nc_data, "Environment/Depth")
  dive <- ncvar_get(nc_data, "Environment/Dive")

  temp <- reshape2::melt(tmp)
  temp$sal <- reshape2::melt(sal)$value
  temp$fluo <- reshape2::melt(fl)$value
  temp$Dive <- dive[temp$Var1]
  temp$Depth <- dep[temp$Var2]
  temp <-temp[!is.nan(temp$value),]
  names(temp)[names(temp)=='value'] <- 'temp'
  temp$Depth_r <- round(temp$Depth/1)*1
  temp <- temp[,!names(temp) %in% c('Var1','Var2')]
  temp <- temp[!is.na(temp$Depth),]
  temp <- temp[order(temp$Dive),]
  temp <- temp[,c('Dive','Depth','Depth_r','temp','sal','fluo')]

  return(temp)
}

#' Plots the environmental data extracted from netCDF files
#' @param data dataframe from get_env() function
#' @param var name of the variable in data to be plotted, defaults to 'temp', options are 'temp', 'sal', 'fluo'
#' @param name string tha tis used as a title for the legend , defaults to '',
#' @param cmap COlormap to be used, defaults to 'RdYlBu'
#' @param lims either NULL or two values decriubing the min and max of the plotted variable. NULL will take the min and max of the data, defaults to NULL.
#' @export
#' @import ggplot2
#' @return ggplot plot
#' @author Sven Gastauer
#'
plot_env <- function(data, var='temp', name='', cmap='RdYlBu', lims=NULL){
  if(is.null(lims) | length(lims)<2){lims=c(min(data[var]), max(data[var]))}
  p <- ggplot(data=data,aes_string(x='Dive', y='Depth_r', fill=var))+
    geom_tile()+
    scale_y_reverse()+
    scale_fill_distiller(palette = cmap, na.value = 'transparent',
                         limits=lims,
                         name=name, oob=scales::squish)+
    scale_x_continuous(expand=c(0.01,0.01))+
    xlab('Dive #') + ylab('Depth [m]')+
    theme_classic()+
    theme(text=element_text(size=16),
          legend.position = 'top',
          legend.text = element_text(size=10))
  return(p)
}

#' Joinf acoustic and environmental data
#' @param sv Sv dataframe
#' @param env ENvironment dataframe
#' @return combined dataframe
#' @import tidyr
#' @export
#' @author Sven Gastauer
env_for_ac<- function(sv,env){return(left_join(sv,env))}
