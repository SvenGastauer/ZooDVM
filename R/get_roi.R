
#' Gets the standard ZC ROI path from a given csv path
#' @description  This assumes that the standard file structure is kept intact
#' @param csvpath path to the ZOnar csv files
#' @export
#' @author Sven Gastauer
#' @return path to ROI csv files
roipath_from_csvpath <-function(csvpath){
  file.path(dirname(csvpath),"roi_counts/")
}

#' Gets the ROI counts for a given mission
#' @param roidir path to the csv files contianing the ROI informaiton
#' @param mission numeric or character, pointing to the filename to be considered
#' @import data.table
#' @export
#' @returndataframe contiaining the ROI counts for each Dive and Depth interval
get_roi_counts <- function(roidir, mission){
  require(data.table)
  if (is.numeric(mission)){
    mission = get_mission(roidir, ending='*.csv', patterns=NULL)[mission]
  }
    message(Sys.time(),': Selected mission - ',mission)
  x=substr(mission,1,nchar(mission)-3);ending='.csv'
  fn = list.files(roidir, pattern=glob2rx(paste0('*',x,'*')),full.names = TRUE)#, substr(mission, 1 , nchar(mission)-3),'*'))

  rois = data.frame(fread(fn), header = T)
  for(i in rev(grep('Total',names(rois)))){
    rois[,i] = rois[,i] - rois[,i+1]
  }
  names(rois)[(grep('Total',names(rois)))] <- c('ROI25','ROI45','ROI75','ROI100','ROI75','ROI125','ROI200')
  names(rois)[(grep('Temperature',names(rois)))] <- 'Temperature'
  names(rois)[(grep('Unix',names(rois)))] <- 'UnixTime'
  names(rois)[(grep('Rho',names(rois)))] <- 'Rho'
  names(rois)[(grep('Pressure',names(rois)))] <- 'Pressure'
  return(rois)
}
