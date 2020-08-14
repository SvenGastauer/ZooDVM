library(plotly)
library(ZooDVM)
require(ggplot2)
library(dplyr)
library(marmap)
library(dplyr)

datadir <-'C:\\Users\\sven\\Documents\\Zonar\\data\\csv_zonar\\'
roidir <- 'C:\\Users\\sven\\Documents\\Zonar\\data\\csv_zc_roi\\'

#get missions
mm = get_mission(datadir)
sel = grep('*LJ*',mm$missions)
sel=16
mission = mm$missions[sel]

#get Sv data
ncpath = ncpath_from_csvpath(datadir)
missions = list.files(ncpath_from_csvpath(datadir), pattern='*nc')

Sv = get_sv(datadir,sel)
env = get_env(ncpath = ncpath, sel)

Sv1000 = Sv$data$`1000kHz`

get_inter <-function(data,vstart,vend){
  tt = data.frame(do.call(cbind, lapply(as.list(unique(data$Dive)),
                           function(x) seq(data[data$Dive==x,vstart][1],
                               data[data$Dive==x,vend][1],
                               length=length(data$Dive[data$Dive==x])))))
  return(stack(tt)$values)
}

Sv1000$Time = as.POSIXct(get_inter(Sv1000,'Time_start','Time_end'), origin='1970-01-01', tz='UTC')
Sv1000$Lon  = get_inter(Sv1000,'Lon_start', 'Lon_end')
Sv1000$Lat  = get_inter(Sv1000,'Lat_start', 'Lat_end')
Sv1000 = Sv1000[!is.na(Sv1000$Sv),]
bath = marmap::getNOAA.bathy(lon1=floor(min(sv$Lon)),
                             lon2=ceiling(max(sv$Lon)),
                             lat1=floor(min(sv$Lat)), lat2=ceiling(max(sv$Lat)),
                             resolution = 1)
b=fortify.bathy(bath)

dd='C:\\Users\\sven\\Documents\\Zonar\\data\\bath\\'

utm_zone = as.numeric(read.table(paste0(dd,'s_ca_25mbathy.prj'), nrows=8, header=FALSE)[2,2])

bres= read.asciigrid(fname = paste0(dd,'s_ca_25mbathy.asc'), proj4string=CRS(paste0("+proj=utm +zone=",utm_zone)))
bres = spTransform(bres, CRS('+proj=longlat'))
bmat = bres@data

coord=bres@coords
coord[,1] = round(coord[,1],3)
coord[,2] = round(coord[,2],3)

bbmat = as.matrix(bmat,length(unique(coord[,1])), length(unique(coord[,2])))
lons = coord[,1]
lats=coord[,2]
#bres=as.data.frame(bres)
bb=bres
bres = as.data.frame(bres)
names(bres) = c('BD','lon','lat')
bres$lon <- round(bres$lon,3)
bres$lat <- round(bres$lat,3)
bres = bres[bres$lon < max(Sv1000$Lon)+0.05,]
bres = bres[bres$lat < max(Sv1000$Lat)+0.05,]
bres = bres[bres$lon > min(Sv1000$Lon)-0.05,]
bres = bres[bres$lat > min(Sv1000$Lat)-0.05,]
head(bres)
#bres$BD[bres$BD < -800] = NA
bres = bres[complete.cases(bres),]



latlon <- data.frame(lat=pj$y, lon=pj$x)
print(latlon)
image(bres)

sdnc = ncdf4::nc_open(paste0(dd,'sd.nc'))
bbb=ncdf4::ncvar_get(sdnc,'Band1')
blon=ncdf4::ncvar_get(sdnc,'lon')
blon > max(sv$Lon)+0.05
blat=ncdf4::ncvar_get(sdnc,'lat')

b = read.csv(paste0(dd,'sd.xyz'), header = FALSE)
names(b) = c('x','y','z')
summary(b)
b$x[b$x > max(Sv1000$Lon)+0.05] = NA
b$y[b$y > max(Sv1000$Lat)+0.05] = NA
b$x[b$x < min(Sv1000$Lon)-0.05] = NA
b$y[b$y < min(Sv1000$Lat)-0.05] = NA

b$z[b$z > (0)] = 0
b=b[complete.cases(b),]
b=data.frame(b%>%spread(x,z))
names(b) = as.numeric(substr(names(b),3, nchar(names(b))))
rownames(b) = b[,1]
b=b[,2:length(b)]

b=data.frame(bres%>%group_by(lon,lat)%>%summarise(D=mean(BD)))
b%>%spread([lon,lat])

p <- plot_ly( z = ~b, y=~lats,x=lons, type = "surface") %>%
  add_trace(data = Sv1000, x = ~Lon, y = ~Lat, z = ~-Depth_r, mode = "markers", type = "scatter3d",
            marker = list(size = 1, color = ~Sv, colorscale = 'Portland', showscale = TRUE))
p%>%layout(scene = list(
  xaxis = list(nticks = 20),
  zaxis = list(nticks = 4),
  camera = list(eye = list(x = 0, y = -1, z = 0.5)),
  aspectratio = list(x = 1, y = 1.2, z = 0.5)))

