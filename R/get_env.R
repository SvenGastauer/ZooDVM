#' Get environmental data from netCDF file
#' @description Currently reads in Temperature, Salinity and fluorescence information as recorded by the Zooglider CTD
#' @param ncpath folder that contains the netCDF files
#' @param mission name or index number of the mission to be used
#' @export
#' @import  ncdf4
#' @return dataframe contianing the lon, lat, time, temperature, salinity and fluorescence data per dive and depth interval
#' @author Sven Gastauer
#'
function(ncpath, mission){
  if (is.numeric(mission)){
    mission = get_mission(ncpath, ending='*.nc', patterns=NULL)[mission]
  }
  message(Sys.time(),': Selected mission - ',mission)

  nc_data <- ncdf4::nc_open(paste0(ncpath, '/' ,mission))

  message(Sys.time(),': Getting GPS')

  t_end <- ncvar_get(nc_data, "gps/UTC_time_fix_end")
  ori = strsplit(ncatt_get(nc_data, "gps/UTC_time_fix_end")$units,'seconds since ')[[1]][2]
  t_end = as.POSIXct(t_end,origin=ori, tz= 'UTC')
  attr(t_end,'tzone') = 'UTC'

  t_start <- ncvar_get(nc_data, "gps/UTC_time_fix_start")
  ori = strsplit(ncatt_get(nc_data, "gps/UTC_time_fix_start")$units,'seconds since ')[[1]][2]
  t_start = as.POSIXct(t_start,origin=ori, tz='UTC')
  attr(t_start,'tzone') = 'UTC'

  t_mid = t_start + (t_end - t_start)/2


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

  md = temp%>%group_by(Dive)%>%summarize(Dstart = max(Depth))
  temp$Time = do.call('rbind',
                      lapply(as.list(unique(temp$Dive)),
                             FUN=function(i)data.frame(approx(c(md$Dstart[i],0),
                                                              c(t_mid[i],t_end[i]),
                                                              xout=temp$Depth[temp$Dive==unique(temp$Dive)[i]])$y)))
  temp$Time = temp$Time[[1]]
  temp$Time = as.POSIXct(temp$Time, origin='1970-01-01', tz='UTC')

  Lon_start <- ncvar_get(nc_data, "gps/lon_start")
  Lon_end <- ncvar_get(nc_data, "gps/lon_end")
  temp$Lon = do.call('rbind',
                     lapply(as.list(unique(temp$Dive)),
                            FUN=function(i)data.frame(approx(c(as.numeric(t_start[i]),as.numeric(t_end[i])),
                                                             c(Lon_start[i],Lon_end[i]),
                                                             xout=as.numeric(temp$Time[temp$Dive==unique(temp$Dive)[i]]))$y)))

  temp$Lon = temp$Lon[[1]]

  Lat_start <- ncvar_get(nc_data, "gps/lat_start")
  Lat_end <- ncvar_get(nc_data, "gps/lat_end")
  temp$Lat = do.call('rbind',
                     lapply(as.list(unique(temp$Dive)),
                            FUN=function(i)data.frame(approx(c(as.numeric(t_start[i]),as.numeric(t_end[i])),
                                                             c(Lat_start[i],Lat_end[i]),
                                                             xout=as.numeric(temp$Time[temp$Dive==unique(temp$Dive)[i]]))$y)))

  temp$Lat = temp$Lat[[1]]


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
