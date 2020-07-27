## ----warning=FALSE, message=FALSE---------------------------------------------
library(ZooDVM)

check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, INSTALL_opts = '--no-lock')
  sapply(pkg, require, character.only = TRUE)
}
check.packages(c('mgcViz','mgcv','rpart','rpart.plot'))
library(mgcViz)
library(mgcv)
library(rpart)
library(rpart.plot)

datadir <-'C:\\Users\\sven\\Documents\\Zonar\\data\\csv_zonar\\'
roidir <- 'C:\\Users\\sven\\Documents\\Zonar\\data\\csv_zc_roi\\'


## ----warning=FALSE, message=FALSE---------------------------------------------
#get missions
mm = get_mission(datadir)
sel = grep('*PtSur*',mm$missions)
mission = mm$missions[sel]

#get Sv data
Sv = get_sv(datadir,sel)

#get GPS info
gps = get_gps( ZooDVM::ncpath_from_csvpath(datadir), sel)

#gen environmental data
env = get_env( ZooDVM::ncpath_from_csvpath(datadir), sel)


## ----getrois, warning=FALSE, message=FALSE------------------------------------
#get ROIs
#which mission to select?
selroi = grep('*PtSur*',list.files(roipath_from_csvpath(datadir)))
rois = get_roi_counts(roipath_from_csvpath(datadir), selroi)

#plot ROI data
plot_roi(rois,roisel=NULL, skipdive=NULL,dres=1)


## ----roisize, warning=FALSE---------------------------------------------------
plot_roi(rois, roisel='ROI125')

## ----warning=FALSE, message=FALSE---------------------------------------------
#join Sv and ROI data
acroi <- join_ac_roi(Sv, rois)

## -----------------------------------------------------------------------------

roivars = names(acroi)[grep('ROI',names(acroi))]

for(v in roivars){
  sub = acroi[acroi[,v]>0,c('Sv200kHz','Sv1000kHz',v)]%>%gather(Freq,Sv,-eval(v))
  p<-ggplot(data=sub)+
    geom_density(aes(Sv, group=Freq, col=Freq))+
    scale_colour_viridis_d(name='')+
    ggtitle(v)+
    theme_classic()+
    theme(legend.position = 'top')
  print(p)
}


## -----------------------------------------------------------------------------

roivars = names(acroi)[grep('ROI',names(acroi))]

sub = acroi[,c('SvDelta',roivars)]%>%gather(ROI,Count,-SvDelta)
sub = sub[sub$Count>0,]
  p<-ggplot(data=sub)+
    geom_density(aes(SvDelta, group=ROI, col=ROI))+
    scale_colour_viridis_d(name='')+
    ggtitle(v)+
    theme_classic()+
    theme(legend.position = 'top')
  print(p)
  

## ----message=FALSE,results="asis", warning=FALSE------------------------------
#make gams and plots

#set depth cut value
dcut=200
#select daytime to analyze:
daytime = 'Day' 

#loop through the different ROIs
for(v in c('ROI25','ROI45','ROI75','ROI125','ROI200')){
    cat("  ###",  v," for ", daytime,"  ")

  g1 =mgcv::gam(data=acroi[acroi$Depth<dcut & acroi$sun==daytime,],eval(parse(text=v))~s(Sv200kHz)+s(Sv1000kHz)+s(Sv200kHz, Sv1000kHz))
  summary(g1)
  
  b <- getViz(g1)
  print(plot(b, allTerms = T) +  l_fitRaster() + l_fitContour() + l_points(alpha=0.2)+ l_fitLine(linetype = 1, size=1.2) + l_fitContour() + 
          l_ciLine(colour = 2) +
          scale_fill_gradientn(colors=rev(pals::brewer.rdylbu(12)) , na.value='transparent',oob=scales::squish)+theme_classic(), pages = 1)
}


## ----message=FALSE,results="asis", warning=FALSE------------------------------
#make gams and plots

#set depth cut value
dcut=200
#select daytime to analyze:
daytime = 'Day' 
rvars =c('ROI25','ROI45','ROI75','ROI125','ROI200')
#loop through the different ROIs
for(v in c('Sv200kHz','Sv1000kHz','SvDelta')){
    cat("  ###",  v," for ", daytime,"  ")

  g1 =mgcv::gam(data=acroi[acroi$Depth<dcut & acroi$sun==daytime,],eval(parse(text=v))~s(ROI25)+s(ROI200)+s(ROI25, ROI200)+s(ROI25, ROI125)+s(ROI25, ROI75)+s(ROI75, ROI200))
  summary(g1)
  
  b <- getViz(g1)
  print(plot(b, allTerms = T) +  l_fitRaster() + l_fitContour() + l_points(alpha=0.2)+ l_fitLine(linetype = 1, size=1.2) + l_fitContour() + 
          l_ciLine(colour = 2) +
          scale_fill_gradientn(colors=rev(pals::brewer.rdylbu(12)) , na.value='transparent',oob=scales::squish)+theme_classic(), pages = 1)
}


## -----------------------------------------------------------------------------

m1 <- rpart(
  formula = (ROI25) ~ Sv1000kHz + Sv200kHz + SvDelta,
  data    = acroi)
m1$variable.importance
rpart.plot(m1,type=1)

## -----------------------------------------------------------------------------


m1 <- rpart(
  formula = (ROI200) ~ Sv1000kHz + Sv200kHz + SvDelta,
  data    = acroi)
m1$variable.importance
rpart.plot(m1,type=1)

