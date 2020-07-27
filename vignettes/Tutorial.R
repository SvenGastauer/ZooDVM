## ----devt, warning=FALSE, message=FALSE---------------------------------------
if('devtools' %in% installed.packages()){
  message(Sys.time(), ': devtools is already available, no further action required. The install version is ',packageVersion('devtools'))
}else{
  install.packages('devtools',dependencies = TRUE, INSTALL_opts = '--no-lock')
}
library(devtools)

## ----chkpkg_fun,warning=FALSE, message=FALSE----------------------------------
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, INSTALL_opts = '--no-lock')
  sapply(pkg, require, character.only = TRUE)
}

## ----chkpkg,warning=FALSE, message=FALSE--------------------------------------
check.packages(c('ggplot2','dplyr','suncalc','ncdf4','pracma', 'tidyr', 'marmap','knitr','interp','mgcViz','mgcv'))


## ----check_pack,warning=FALSE, message=FALSE----------------------------------
if(!('ZooDVM' %in% installed.packages())){
  devtools::install_github('SvenGastauer/ZooDVM', build_vignettes = TRUE, dependencies = TRUE)
}else{message(Sys.time(), ': ZooDVM is already installed')}

if(!('ZooScatR' %in% installed.packages())){
  devtools::install_github('AustralianAntarcticDivision/ZooScatR', build_vignettes = TRUE, dependencies = TRUE)
}else{message(Sys.time(), ': ZooScatR is already installed')}

library(ZooDVM)
library(ZooScatR)
library(ggplot2) #for other plotting needs
library(dplyr) # for fast and clean data manipulation
library(tidyr) # for clean data manipulation


## ----devtools, eval=FALSE-----------------------------------------------------
#   install.packages('devtools',dependencies = TRUE, INSTALL_opts = '--no-lock')

## ----rcpp, eval=FALSE---------------------------------------------------------
#   install.packages('Rcpp',dependencies = TRUE, INSTALL_opts = '--no-lock')

## ----get_miss-----------------------------------------------------------------
path = 'C:\\Users\\sven\\Documents\\Zonar\\data\\csv_zonar\\'
mm = get_mission(path)

## -----------------------------------------------------------------------------
sel=15
mission = mm$missions[sel]
Sv = get_sv(path,sel)

## -----------------------------------------------------------------------------
Sv$plots$`1000kHz`

## -----------------------------------------------------------------------------
Sv$plots$`200kHz`

## -----------------------------------------------------------------------------
Sv$plots$`Delta`

## -----------------------------------------------------------------------------
plot_sv(Sv$data$`1000kHz`,cmaps="RdBu", svmax=-55, svmin=-80)

## -----------------------------------------------------------------------------
plot_sv(Sv$data$`1000kHz`,gps=Sv$gps,cmaps="RdBu")

## ----medianfilter-------------------------------------------------------------
med3x3 = filter2d(data=Sv$data$`1000kHz`, 
                  x=3,y=3, #set the x and y window size
                  xval='Dive', #default is 'Dive' which is the name of the x axis variable
                  yval='Depth_r',#default is 'Depth_r' which is the name of the y axis variable 
                  val='Sv', #default is 'Sv' which is the name of the fill variable
                  log=TRUE, #default is TRUE which means that the input variable is in log space
                  fun = 'median') #the function to be computed
plot_sv(med3x3, nam=paste(str_replace_all(mission,'_',' '), expression("- Median 3x3 ", Sv['1000kHz'])))

## ----meanfilter---------------------------------------------------------------
mean5x5 = filter2d(data=Sv$data$`1000kHz`, x=5,y=5,fun = 'mean')
plot_sv(mean5x5, Sv$gps, nam=paste(str_replace_all(mission,'_',' '), expression("- Mean 5x5 ", Sv['1000kHz'])))

## ----anomaly1-----------------------------------------------------------------
ano1 = anomaly(Sv$data$`1000kHz`, fun='mean')
#plotting the scaled anomaly
plot_sv(ano1)
#plotting the normal anomaly
plot_sv(ano1, variable = 'SvAnomal')

#if we want to apply another filter to the anomaly calculation it is recommended to set replace=TRUE, which replaces the Sv values with the variable v which can be set to be either SvAnomal or scaledAnomaly
ano_re = anomaly(Sv$data$`1000kHz`, fun='mean', replace = TRUE, v='SvAnomal')
ano_re_med = filter2d(ano_re,x=3,y=3,fun='median')
plot_sv(ano_re_med, svmin = -10, svmax=10, nam=paste(str_replace_all(mission,'_',' '), expression("- Median 3x3 Anomaly", Sv['1000kHz'])))

## -----------------------------------------------------------------------------
dvm = pdvm(Sv$data$`1000kHz`,
         vmin=-85,
         vmax=-58,
         perc=25,
         dcut=280,scut=15, 
         dskip=3,
         dend=3,
         updown='bottom')

p=Sv$plots$`1000kHz`
p+geom_line(data=dvm, aes(x=Dive, y=Depth),size=1)

## ----get_ncpath---------------------------------------------------------------

ncpath = ncpath_from_csvpath(path)

## -----------------------------------------------------------------------------
calinfo = get_accal(ncpath, sel)
knitr::kable(calinfo)

## ----get_gps------------------------------------------------------------------
sel = 15 #selected mission
gps = get_gps(ncpath, mission=sel)

## ----warning=FALSE, message=FALSE---------------------------------------------
plot_gps(gps, bathy=TRUE, daynight=TRUE,startend=TRUE)

## -----------------------------------------------------------------------------
plot_gps(gps, bathy=FALSE, daynight=TRUE,startend=TRUE)

## ----get_env------------------------------------------------------------------
sel = 15 #selected mission
env = get_env(ncpath, mission=sel)

## -----------------------------------------------------------------------------
plot_env(env,'temp', 'Temperature')

## -----------------------------------------------------------------------------
plot_env(env,'sal', 'Salinity')

## -----------------------------------------------------------------------------
plot_env(env,'fluo', 'Fluorescence', lims=c(0, 1500))

## ----clines-------------------------------------------------------------------
clines=env %>%
   group_by(Dive) %>%
   summarise(thermo_c=thermocline(temp,Depth, tmax=NULL),
             fluo_c=thermocline(fluo,Depth, tmax=500, tmin=10,r=5),
             sal_c=thermocline(sal,Depth,r=5,tmin=33.4, tmax=34, cw='w'))

## ----plot_thermoc-------------------------------------------------------------
p<-ggplot()+geom_tile(data=env, aes(y=Depth_r, x=Dive, fill=temp))+
   scale_y_reverse()+theme_classic()+
   scale_fill_gradientn(colors=rev(pals::brewer.rdylbu(15)))+xlab('Dive #')+ylab('Depth [m]')
 p+geom_line(data.frame(clines),mapping=aes(x=Dive,y=thermo_c, color='Temperature'),size=1)+
   geom_line(data.frame(clines),mapping=aes(x=Dive,y=fluo_c, color='Fluorescence'),size=1)+
   geom_line(data.frame(clines),mapping=aes(x=Dive,y=sal_c, color='Salinity'),size=1)+
   scale_color_manual(values = c('Temperature' = 'black',
                                 'Fluorescence' = 'gray',
                                 'Salinity' = 'darkgray'), name='')
 

## -----------------------------------------------------------------------------
p<-ggplot()+geom_tile(data=Sv$data$`1000kHz`, aes(y=Depth_r, x=Dive, fill=Sv))+
   scale_y_reverse()+theme_classic()+
   scale_fill_gradientn(colors=rev(pals::brewer.rdylbu(15)), na.value='transparent')+xlab('Dive #')+ylab('Depth [m]')
 p+geom_line(data.frame(clines),mapping=aes(x=Dive,y=thermo_c, color='Temperature'),size=1)+
   geom_line(data.frame(clines),mapping=aes(x=Dive,y=fluo_c, color='Fluorescence'),size=1)+
   geom_line(data.frame(clines),mapping=aes(x=Dive,y=sal_c, color='Salinity'),size=1)+
   scale_color_manual(values = c('Temperature' = 'black',
                                 'Fluorescence' = 'darkgray',
                                 'Salinity' = 'blue'), name='')

