setwd('C:\\Users\\sven\\Documents\\Zonar\\data\\gray\\')
gray <- read.csv('LJ2.csv')

library(ggplot2)
library(dplyr)
res=.5
gray$depth_r = round(gray$pressure_dbar/res)*res
gray$Dive = as.numeric(substr(gray$dive_number, 2,nchar(gray$dive_number)))
gdm = gray %>% group_by(Dive, depth_r)%>%summarise(gray=median(na.omit(gray_val)))

p<-ggplot(data=gdm, aes(x=Dive, y=depth_r, fill=256-gray))+geom_tile(na.value='transparent')+
  scale_y_reverse()+
  theme_classic()+
  scale_fill_gradientn(colors=rev(pals::brewer.rdylbu(15)),limits=c(21,24), oob=scales::squish)+
  #scale_fill_gradient(low='white',high='black',limits=c(22,26), oob=scales::squish)+
  xlab('Dive #')+ylab('Depth [m]')

library(plotly)

ggplotly(p)
