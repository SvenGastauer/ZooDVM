
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
get_roi_counts(roidir, mission){
  require(data.table)
  if (is.numeric(mission)){
    mission = get_mission(roidir, ending='*.csv', patterns=NULL)[mission]
  }
  message(Sys.time(),': Selected mission - ',mission)

  rois = data.frame(fread(paste0(roidir,'/',mission), header = T))
  for(i in 10:4){
    rois[,i] = rois[,i] - rois[,i+1]
  }
  names(rois)[1:11] <- c('fn','SensitivityEdgeCOunt','SensitivityLevel','ROI25','ROI45','ROI75','ROI100','ROI75','ROI125','ROI200','Dive')
  return(rois)
}
head(rois)
rois%>%
