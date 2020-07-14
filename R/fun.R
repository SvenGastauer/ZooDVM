
#' Prepare Sv data and gps for plotting with plot_sv
#' @param data Sv dataframe
#' @param gps dataframe from netCDF Sat GPS data
#' @import  dplyr
#' @import  tidyr
#' @return dataframe containing the information needed for plotting with plot_sv
#' @export
prepare_data <- function(data, gps){
  d1 = gather(data,'Dive','Sv',-Depth_r,-variable)
  d1$Dive = as.numeric(as.character(substr(d1$Dive,2,nchar(d1$Dive))))
  gps$Dive = seq(min(d1$Dive), min(d1$Dive) + nrow(gps)-1)
  d1 = merge(gps,d1, by='Dive')

  return(d1)
}

#' get available files and missions in a given folder
#' @param path path that contains the zonar sv files
#' @param patterns patterns considered, defaults to c('200kHz', '1000kHz','Delta')
#' @export
#' @return list contiang the paths to the files for each pattern and a list of detected missions
get_mission <- function(path, patterns = c('200kHz', '1000kHz','Delta'), ending='*.csv'){
  if (length(patterns)>0){
    p = lapply(as.list(patterns),
               FUN=function(x) list.files(path=path, pattern=glob2rx(paste0('*',x,ending))))
    names(p) = patterns
    p[['missions']] = unique(unlist(lapply(p, FUN = function(x) unlist(strsplit(x,'_sv_'))[seq(1,2 * length(x),2)])))
    message(Sys.time(), ': a total of ', length(p[['missions']]), ' missions were found.')
  }else{
    p = list.files(path=path, pattern=ending)
    message(Sys.time(), ': a total of ', length(p), ' missions were found.')
  }
  return(p)
}

#' Plot sv data with day/night bar
#' @param d1 dataframe contians the sv, dpeth and dive information
#' @param gps dataframe containing lat, lon, and time information
#' @param col color scheme for the day/night bar defaults to "Night" = "black", "Dusk/Dawn" = "gray", "Day" = "yellow"
#' @param cmaps colour maps for Sv data and Sv delta if needed, defaults to c("RdYlBu",'RdBu')
#' @param svmin minimum value for Sv color, defaults to -85
#' @param svmax maximum value for Sv color, defaults to -45
#' @param deltamin minimum value for delta Sv, defaults to -10
#' @param deltamax maximum value for delta Sv, defaults to +10
#' @export
#' @import ggplot2
#' @author Sven Gastauer
#' @return list of plots
plot_sv <- function(d1, gps=NULL,
                    cols=c("Night" = "black", "Dusk/Dawn" = "gray", "Day" = "yellow"),
                    cmaps=c("RdYlBu",'RdBu'),
                    svmin=-85,svmax=-45,deltamin=-10,deltamax=10){
  if(unique(d1$variable) %in% c('1000kHz','200kHz','Sv','TS')){lims=c(svmin,svmax); cmap = cmaps[1]}
  else{lims=c(deltamin,deltamax); cmap=cmaps[2]}
  p<-ggplot()+
    geom_tile(data=d1,aes(x=Dive, y=Depth_r,fill=Sv))+
    scale_y_reverse()+
    scale_fill_distiller(palette = cmap, na.value = 'transparent',
                         limits=lims,
                         name=paste('Sv',unique(d1$variable)), oob=scales::squish)+
    scale_x_continuous(expand=c(0.01,0.01))+
    xlab('Dive #') + ylab('Depth [m]')+
    theme_classic()+
    theme(text=element_text(size=16),
          legend.position = 'top')
if (!is.null(gps)){
  p <- p + geom_segment(data=gps,aes(x = Dive, xend = Dive + 1, y = -5, yend = -5, colour=sun),show.legend = FALSE, size=1,alpha=1)+
    scale_colour_manual(values = cols, name='')
}
  return(p)
}

#' Read the processed Zonar files
#'This function reads all acoustic Sv data for a given mission
#'It takes a path where the csv files containing the Sv informaiton are located and a mission reference.
#'The mission reference can be given as a number or character
#' @param path character path to where the csv files are located
#' @param mission character or integer if a character is provided, this has to be the name of the mission as it is contained in the csv files name, if a number is provded the ith mission contained in the path will be used
#' @param ncpath path to the nc files, defaults to NULL. If NULL the parent folder of the csv path is assumed to contain a folder called nc_zonar, which contains the ZOnar netCDF files
#' @export
#' @author Sven Gastauer
#'
get_sv <- function(path, mission, ncpath=NULL){

  if (is.numeric(mission)){
   mission = get_mission(path)$missions[mission]

  }
  message(Sys.time(),': Selected mission - ',mission)

  fs = list.files(path = path, pattern = glob2rx(paste0(mission,'*_sv*', '*.csv')), full.names = TRUE)

  data = lapply(as.list(fs),FUN = function(x) read.csv(x))
  names(data) <- unlist(strsplit(unlist(strsplit(fs,'_sv_'))[seq(2,2*length(fs),2)],'_vres'))[seq(1,2*length(fs),2)]
  for (i in 1:length(data)){data[[i]]$variable = names(data)[i]}

  #get gps and time per dive
  if(is.null(ncpath)){ncpath=file.path(dirname(path),"nc_zonar/")}
  gps = get_daynight(ncpath, mission)

  dlist = lapply(data, FUN = function(x) prepare_data(x,gps))

  gps$Dive <- seq(min(dlist[[1]]$Dive),min(dlist[[1]]$Dive)+nrow(gps)-1,1)

  p <- lapply(dlist,FUN =function(x) plot_sv(x,gps))

  return(list('data'=dlist,'plots'=p,'gps'=gps))

}

#' Gets GPS, daytime and day/night for a given missin per dive
#' @param ncpath path to the ZOnar netCDF files
#' @param mission string which is the name of the mission to be loaded
#' @return dataframe conatining per dive start and end lon, lat, and time, as well as day/dusk/dawn/nigth for each dive, based on the mean location and time
#' @import ncdf4
#' @import suncalc
#' @export
#' @author Sven Gastauer
#'
get_daynight <- function(ncpath, mission){
  fn = list.files(path=ncpath,glob2rx(paste0(mission,'*.nc')), full.names = TRUE)
  nc_data <- ncdf4::nc_open(fn)

  t_end <- ncdf4::ncvar_get(nc_data, "gps/UTC_time_fix_end")
  ori = strsplit(ncatt_get(nc_data, "gps/UTC_time_fix_end")$units,'seconds since ')[[1]][2]
  t_end = as.POSIXct(t_end,origin=ori)
  attr(t_end,'tzone') = 'UTC'

  t_start <- ncdf4::ncvar_get(nc_data, "gps/UTC_time_fix_start")
  ori = strsplit(ncatt_get(nc_data, "gps/UTC_time_fix_start")$units,'seconds since ')[[1]][2]
  t_start = as.POSIXct(t_start,origin=ori)
  attr(t_start,'tzone') = 'UTC'

  lon_start <- ncdf4::ncvar_get(nc_data, "gps/lon_start")
  lon_end <- ncdf4::ncvar_get(nc_data, "gps/lon_end")
  lat_start <- ncdf4::ncvar_get(nc_data, "gps/lat_start")
  lat_end <- ncdf4::ncvar_get(nc_data, "gps/lat_end")
  gps =data.frame(Time_start =t_start,
                  Time_end = t_end,
                  Lon_start = lon_start,
                  Lon_end = lon_end,
                  Lat_start = lat_start,
                  Lat_end = lat_end)

  time = as.POSIXct(rowMeans(cbind(as.numeric(gps[,'Time_start']),as.numeric( gps[,'Time_end']))), origin='1970-01-01')
  attr(time, 'tzone') = 'UTC'
  lon = rowMeans(gps[,c('Lon_start','Lon_end')])
  lat = rowMeans(gps[,c('Lat_start','Lat_end')])
  sr <- do.call(rbind,apply(as.matrix(1:length(time)), 1, function(x)
    getSunlightPosition(time[x], lat[x], lon[x])))
  gps$alt <- sr$altitude * 180 / pi
  gps$azimuth <- sr$azimuth * 180 / pi
  gps$sun <- cut(gps$alt,
                breaks=c(-Inf, -12, 0, Inf),
                labels=c("Night","Dusk/Dawn","Day"))
  return(gps)
}

#' Function to get the estimated depth of the DVM based on Sv values and a cutoff percentage
#' @param sv dataframe containing Sv and depth information for each dive
#' @param cutval float, describing the cumulative percentage at which the cut should be made
#' @export
#' @import dplyr
#' @return  dataframe contains the per dive dvm depth
#' @author Sven Gastauer
get_dvm <- function(sv, cutval){
  dvm = sv %>%
    group_by(Dive) %>%
    arrange(Dive,Perc) %>%
    filter(Perc >= cutval) %>%
    slice(1) %>% # takes the first occurrence if there is a tie
    ungroup()
  return(dvm)
}


#' Get dvm informaiton from Sv data
#' @param ac_group_sub Sv dataframe
#' @param vmin minimum Sv value to be considered, lower values will be set to the minimum value, defaults to -85
#' @param vmax maximum Sv value to be considered, higher values will be set to the maximum value, defaults to -45
#' @param perc cutoff cumulative percentage, defaults to 65
#' @param dcut depth cut off value, deeper values will be ignored, defaults to 300 m
#' @param scut surface cutoff value, shallower values will be ignored, defaults to 20 m
#' @param dr depth resolution, defaults to 1
#' @param dskip number of dives to skip at the start, defaults to 0
#' @param dend number of dives to skip at the end, defaults to 0
#' @export
#' @import dplyr
pdvm <- function(ac_group_sub,vmin=-85,vmax=-45,perc=65,dcut=300,scut=20,dr=1,dskip=0,dend=0, updown='surface'){
  ac_group_sub = ac_group_sub[ac_group_sub$Dive > min(ac_group_sub$Dive)+dskip,]
  ac_group_sub = ac_group_sub[ac_group_sub$Dive < max(ac_group_sub$Dive)-dend,]
  ac_group_sub$Sv[ac_group_sub$Sv>vmax] = vmax
  ac_group_sub$Sv[ac_group_sub$Sv<vmin] = vmin
  ac_group_sub = ac_group_sub[ac_group_sub$Depth_r <= dcut,]
  ac_group_sub = ac_group_sub[ac_group_sub$Depth_r >= scut,]
  ac_group_sub$Depth = round(ac_group_sub$Depth_r/dr)*dr

  if (updown=='surface'){
    ac_group_sub = ac_group_sub[order(ac_group_sub$Dive,ac_group_sub$Depth_r),]
  }else{
    ac_group_sub = ac_group_sub[order(ac_group_sub$Dive,-ac_group_sub$Depth_r),]
  }

  ac_group_sub$Sv_lin=10**(ac_group_sub$Sv/10)


  abc = na.omit(data.frame(ac_group_sub)) %>%
    group_by(Dive) %>%
    #mutate(Sv_lin = mean(10^(Sv/10)))%>%
    #summarise(cs_200 = NASC200) %>%
    mutate(Perc = cumsum(100*Sv_lin/sum(Sv_lin)))

  dvm = get_dvm(sv=abc,perc)
  return(dvm)
}

#' DVM summary
#' @param dvm davm dataframe from pdvm
#' @export
#' @import dplyr
#'
dvmsum <- function(dvm){
  dvmsum <- dvm %>%
    group_by(sun) %>%
    summarise(Depth = mean(Depth_r,),
              sdDepth = sd(Depth_r),
              N=n(),.groups = 'drop')
  dvmsum$dpsd=dvmsum$Depth+dvmsum$sdDepth
  dvmsum$dmsd=dvmsum$Depth-dvmsum$sdDepth

  return(dvmsum)
}


#' Gets GPS and daytime for a given misson per dive
#' @param ncpath path to the ZOnar netCDF files
#' @param mission string or integer which is the name of the mission to be loaded
#' @return dataframe containing per dive mean, start and end lon, lat, and time for each dive
#' @import ncdf4
#' @export
#' @author Sven Gastauer
#'
get_gps <- function(ncpath, mission){
  if (is.numeric(mission)){
    mission = get_mission(ncpath, ending='*.nc', patterns=NULL)[mission]

  }
  message(Sys.time(),': Selected mission - ',mission)


  message(Sys.time(),': Getting GPS')
  nc_data <- ncdf4::nc_open(paste0(ncpath, '/' ,mission))
  t_end <- ncdf4::ncvar_get(nc_data, "gps/UTC_time_fix_end")
  ori = strsplit(ncatt_get(nc_data, "gps/UTC_time_fix_end")$units,'seconds since ')[[1]][2]
  t_end = as.POSIXct(t_end,origin=ori)
  attr(t_end,'tzone') = 'UTC'

  t_start <- ncdf4::ncvar_get(nc_data, "gps/UTC_time_fix_start")
  ori = strsplit(ncatt_get(nc_data, "gps/UTC_time_fix_start")$units,'seconds since ')[[1]][2]
  t_start = as.POSIXct(t_start,origin=ori)
  attr(t_start,'tzone') = 'UTC'

  lon_start <- ncdf4::ncvar_get(nc_data, "gps/lon_start")
  lon_end <- ncdf4::ncvar_get(nc_data, "gps/lon_end")
  lat_start <- ncdf4::ncvar_get(nc_data, "gps/lat_start")
  lat_end <- ncdf4::ncvar_get(nc_data, "gps/lat_end")
  Dive <- seq(1,length(lon_start))
  var = rep(c('start', 'stop'), each=length(lon_end))

  gps =data.frame(Dive = c(Dive,Dive),
                  var = var,
                  Time = c(t_start,  t_end),
                  Lon = c(lon_start,lon_end),
                  Lat = c(lat_start, lat_end))


  message(Sys.time(),': Getting sun position/azimuth and Day/Night info')
  sr <- do.call(rbind,apply(as.matrix(1:length(gps$Time)), 1, function(x)
    getSunlightPosition(gps$Time[x], gps$Lat[x], gps$Lon[x])))
  gps$alt <- sr$altitude * 180 / pi
  gps$azimuth <- sr$azimuth * 180 / pi
  gps$sun <- cut(gps$alt,
                 breaks=c(-Inf, -12, 0, Inf),
                 labels=c("Night","Dusk/Dawn","Day"))


  gps$mission = mission
  gps = gps[order(gps$Dive),]

  return(gps)
}

#' Plot mission map with bathymetry
#' @param gps dataframe contianing grps information, generated by get_gps
#' @param bathy TRUE/FALSE to add bathymetry from marmap package or not, defaults to TRUE
#' @param daynight TRUE/FALSE color code points by day, night or dusk/dawn, defaults to TRUE
#' @param startend TRUE/FALSE shape points for start and end points of dive, defaults to TRUE
#' @return ggplot
#' @import marmap
#' @import ggplot2
#' @export
#' @author Sven Gastauer
#'
plot_gps <- function(gps, bathy=TRUE, daynight=TRUE, startend=TRUE){
  library(marmap)
  if (bathy){
    miss.bath <- getNOAA.bathy(lon1 = (min(gps$Lon)-0.1), lon2 = ceiling(max(gps$Lon)+0.1),
                             lat1 = floor(min(gps$Lat)-0.1), lat2 = ceiling(max(gps$Lat)+0.1))
    p <- autoplot(miss.bath, geom = c("raster","contour"), colour = "white", interpolate=TRUE) +
    scale_fill_gradientn(name='',
                         values = scales::rescale(c(min(fortify.bathy(miss.bath)$z),
                                                    -.1, 0, max(fortify.bathy(miss.bath)$z))),
                         colors = c("steelblue4", "#C7E0FF", "grey50", "grey80"))+
      geom_line(data=gps,aes(x=Lon, y=Lat),col='gray')+
      ylab('Latitude')+xlab('Longitude')+
      theme_classic()+
      theme(text=element_text(size=16),
            axis.text.x=element_text(angle=75,hjust=0, vjust=0),
            legend.text = element_text(size=10))
  }else{
    p <- ggplot(data=gps,aes(x=Lon, y=Lat, group=Dive),col='gray')+geom_line()+
      #scale_x_continuous(limits=c(min(gps$Lon)-0.1, max(gps$Lon)+0.1))+
      #scale_y_continuous(limits=c(min(gps$Lat)-0.1, max(gps$Lat)+0.1), expand=c(0,0))+
      ylab('Latitude')+xlab('Longitude')+
      theme_classic()+
      theme(text=element_text(size=16),
            axis.text.x=element_text(angle=75,hjust=0, vjust=0),
            legend.text = element_text(size=10))
  }
  if (daynight & startend)(p <- p + geom_point(data=gps,aes(x=Lon, y=Lat, col=sun, shape=var), size=0.9))
  if (daynight & !startend)(p <- p + geom_point(data=gps,aes(x=Lon, y=Lat, col=sun), size=0.9))
  if (!daynight & startend)(p <- p + geom_point(data=gps,aes(x=Lon, y=Lat, shape=var), size=0.9))
  if (daynight){p<-p + scale_color_manual(values=c('black','lightgray', 'yellow'), name='')}
  if(startend){p<-p+scale_shape_manual(values=c(4,19), name='')}

  return(p)
}

#' Gets the standard nc path from a given csv path
#' @description  This assumes that the standard file structure is kept intact
#' @param csvpath path to the ZOnar csv files
#' @export
#' @author Sven Gastauer
#' @return path to nc files
ncpath_from_csvpath <-function(csvpath){
  file.path(dirname(csvpath),"nc_zonar/")
}


