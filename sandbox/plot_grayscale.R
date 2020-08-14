setwd('C:\\Users\\sven\\Documents\\Zonar\\data\\gray\\')
ncdir = 'C:\\Users\\sven\\Documents\\Zonar\\data\\nc_zonar\\'

library(ZooDVM)

ncpath=ncdir
mission=16

if (is.numeric(mission)){
  mission = get_mission(ncpath, ending='*.nc', patterns=NULL)[mission]
}
message(Sys.time(),': Selected mission - ',mission)

nc_data <- ncdf4::nc_open(paste0(ncpath, '/' ,mission))

message(Sys.time(),': Getting Seabed')
bd = ncvar_get(nc_data, "z/z")/10
d = ncvar_get(nc_data, "z/Dive#")
bin = ncvar_get(nc_data, "z/bin")
bddf = data.frame('Dive'=d,'BDepth'=bd,'Bin'=bin)
bd = bddf %>% group_by(Dive)%>%summarise(BDepth=max(BDepth))

gray <- read.csv('LJ2.csv')

library(ggplot2)
library(dplyr)
res=5
gray$depth_r = round(gray$pressure_dbar/res)*res
gray$Dive = as.numeric(substr(gray$dive_number, 2,nchar(gray$dive_number)))
gdm = gray %>% group_by(Dive, depth_r)%>%summarise(gray=median(na.omit(gray_val)))


p<-ggplot()+geom_tile(data=gdm, aes(x=Dive, y=depth_r, fill=256-gray),na.value='transparent')+
  scale_y_reverse(limit=c(500,0))+
  theme_classic()+
  scale_fill_gradientn(colors=rev(pals::brewer.rdylbu(15)),limits=c(21,24), oob=scales::squish)+
  #scale_fill_gradient(low='white',high='black',limits=c(22,26), oob=scales::squish)+
  xlab('Dive #')+ylab('Depth [m]')

p <- p + geom_path(data=bd,aes(x=Dive,y=BDepth))
library(plotly)

ggplotly(p)
