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


