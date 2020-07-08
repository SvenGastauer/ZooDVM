
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
get_mission <- function(path, patterns = c('200kHz', '1000kHz','Delta')){
  p = lapply(as.list(patterns),
             FUN=function(x) list.files(path=path, pattern=glob2rx(paste0('*',x,'*.csv'))))
  names(p) = patterns
  p[['missions']] = unique(unlist(lapply(p, FUN = function(x) unlist(strsplit(x,'_sv_'))[seq(1,2 * length(x),2)])))
  message(Sys.time(), ': a total of ', length(p[['missions']]), ' missions were found.')
  return(p)
}

#' Plot sv data with day/night bar
#' @param d1 dataframe contians the sv, dpeth and dive information
#' @param gps dataframe containing lat, lon, and time information
#' @param col color scheme for the day/night bar defaults to "Night" = "black", "Dusk/Dawn" = "gray", "Day" = "yellow"
#' @param cmaps colour maps for Sv data and Sv delta if needed, defaults to c("RdYlBu",'RdBu')
#' @export
#' @import ggplot2
#' @author Sven Gastauer
#' @return list of plots
plot_sv <- function(d1, gps,cols=c("Night" = "black", "Dusk/Dawn" = "gray", "Day" = "yellow"),cmaps=c("RdYlBu",'RdBu')){
  if(unique(d1$variable) %in% c('1000kHz','200kHz')){lims=c(-85,-45); cmap = cmaps[1]}
  else{lims=c(-10,10); cmap=cmaps[2]}
  p<-ggplot()+
    geom_tile(data=d1,aes(x=Dive, y=Depth_r,fill=Sv))+
    scale_y_reverse()+
    scale_fill_distiller(palette = cmap, na.value = 'transparent',
                         limits=lims,
                         name=paste('Sv',unique(d1$variable)), oob=scales::squish)+
    scale_x_continuous(expand=c(0.01,0.01))+
    xlab('Dive #') + ylab('Depth [m]')+
    geom_segment(data=gps,aes(x=Dive,xend=Dive+1, y=-3, yend=-3, colour=sun),show.legend = FALSE, size=3,alpha=0.5)+
    scale_colour_manual(values = cols, name='')+
    theme_classic()+
    theme(text=element_text(size=16),
          legend.position = 'top')
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

  return(list('data'=dlist,'plots'=p))

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

  t_end <- ncvar_get(nc_data, "gps/UTC_time_fix_end")
  ori = strsplit(ncatt_get(nc_data, "gps/UTC_time_fix_end")$units,'seconds since ')[[1]][2]
  t_end = as.POSIXct(t_end,origin=ori)
  attr(t_end,'tzone') = 'UTC'

  t_start <- ncvar_get(nc_data, "gps/UTC_time_fix_start")
  ori = strsplit(ncatt_get(nc_data, "gps/UTC_time_fix_start")$units,'seconds since ')[[1]][2]
  t_start = as.POSIXct(t_start,origin=ori)
  attr(t_start,'tzone') = 'UTC'

  lon_start <- ncvar_get(nc_data, "gps/lon_start")
  lon_end <- ncvar_get(nc_data, "gps/lon_end")
  lat_start <- ncvar_get(nc_data, "gps/lat_start")
  lat_end <- ncvar_get(nc_data, "gps/lat_end")
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

  dvm = sv[order(sv$Dive,sv$Depth),] %>%
    group_by(Dive) %>%
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
pdvm <- function(ac_group_sub,vmin=-85,vmax=-45,perc=65,dcut=300,scut=20,dr=1,dskip=0,dend=0){
  ac_group_sub = ac_group_sub[ac_group_sub$Dive > min(ac_group_sub$Dive)+dskip,]
  ac_group_sub = ac_group_sub[ac_group_sub$Dive < max(ac_group_sub$Dive)-dend,]
  ac_group_sub$Sv[ac_group_sub$Sv>vmax] = vmax
  ac_group_sub$Sv[ac_group_sub$Sv<vmin] = vmin
  ac_group_sub = ac_group_sub[ac_group_sub$Depth_r <= dcut,]
  ac_group_sub = ac_group_sub[ac_group_sub$Depth_r >= scut,]
  ac_group_sub$Depth = round(ac_group_sub$Depth_r/dr)*dr

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

