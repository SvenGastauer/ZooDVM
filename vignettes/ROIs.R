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

bdir = 'Z:\\zonar_sven\\zonar\\data\\'

datadir <-paste0(bdir, 'csv_zonar\\')
roidir <- paste0(bdir, 'csv_zc_roi\\')


