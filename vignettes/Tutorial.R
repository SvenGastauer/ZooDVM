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
kable(calinfo)

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
plot_env(env,'fluo', 'Fluorescence', lims=c(100, 1500))

## -----------------------------------------------------------------------------
# 
# 
# env_r <- env %>% group_by(Dive,Depth_r) %>% summarise(temp =mean(temp), sal=mean(sal), fluo=mean(fluo))
# envSv=left_join(Sv$data$`1000kHz`,env)
# 
# dvm = pdvm(ac_group=Sv$data$`1000kHz`,
#          vmin=-85,
#          vmax=-60,
#          perc=65,
#          dcut=280,scut=10, dskip=3,dend=3)
# 
# 
# esurf <- env[env$Depth_r<=50 & env$Depth_r>=10,] %>% group_by(Dive) %>%summarise(temp_surf=mean(temp),
#                                                       sal_surf=mean(sal),
#                                                       fluo_surf = mean(fluo))
# ddif = 10
# dvmd <- data.frame(Dive=dvm$Dive, d0=dvm$Depth - ddif, d1 = dvm$Depth + ddif,dvm=dvm$Depth)
# 
# envdvm <- full_join(env,dvmd)
# envdvm <- full_join(envdvm,esurf)
# envdvm = full_join(envdvm, gps)
# subsv = Sv$data$`1000kHz`
# head(subsv)
# 
# envsum <- na.omit(envdvm[envdvm$Depth_r >= envdvm$d0 & envdvm$Depth_r<= envdvm$d1, ]) %>%
#   group_by(Dive) %>% 
#   summarise(Depth =mean(Depth_r),
#             Dive=mean(Dive),
#             DVM = mean(dvm),
#             temp=mean(temp),
#             sal = mean(sal),
#             fluo=mean(fluo),
#             temp_surf = mean(temp_surf),
#             sal_surf = mean(sal_surf),
#             fluo_surf = mean(fluo_surf),
#             d0=mean(d0),
#             d1=mean(d1),
#             dt = first(sun),
#             lon = mean(Lon),
#             lat = mean(Lat))
# 
# library(mgcv)
# envsum=data.frame(envsum)
# get_gam <- function(envsum, dn){
#   sub = envsum[envsum$dt==unique(envsum$dt)[dn],]
#   g1=mgcv::gam(data=sub, 
#                DVM~s(temp)+s(sal)+s(fluo_surf))
#   return(g1)
# }
# 
# daygam <- get_gam(envsum, 1)
# nightgam <- get_gam(envsum, 3)
# 
# library(mgcViz)
# daygam=mgcv::gam(data=envsum, 
#                DVM~s(temp_surf)+s(sal_surf)+s(fluo_surf) + s(sal)+s(temp)+dt)
# viz <- getViz(daygam)
# print(plot(viz, allTerms = T), pages = 1)
# 
# plot(gam(data=envsum, -DVM~s(fluo_surf)))


## -----------------------------------------------------------------------------
# ggplot(data=envsum, aes(x=fluo_surf, DVM))+geom_point()+facet_grid(.~dt)+geom_smooth(method='loess')+scale_y_reverse()
# 
# plot(viz) + l_dens(type = "cond", alpha=0.5) + 
#   scale_fill_gradient(low = "white", high = "black", oob=scales::squish, na.value = 'transparent', name='Density')+
#  l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
#   l_fitLine() +
#    l_ciLine(mul = 5, colour = "black", linetype = 2) +
#    l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()

