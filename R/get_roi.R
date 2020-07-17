
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
#' @return dataframe contiaining the ROI counts for each Dive and Depth interval
get_roi_counts <- function(roidir, mission){
  require(data.table)
  if (is.numeric(mission)){
    mission = get_mission(roidir, ending='*.csv', patterns=NULL)[mission]
  }
    message(Sys.time(),': Selected mission - ',mission)
  x=mission;ending='.csv'
  fn = list.files(roidir, pattern=glob2rx(paste0('*',x,'*')),full.names = TRUE)#, substr(mission, 1 , nchar(mission)-3),'*'))

  rois = data.frame(fread(fn), header = T)
  rcols = grep('Total',names(rois))
  rcols=rcols[1:length(rcols)-1]
  for(i in rev(rcols)){
    rois[,i] = rois[,i] - rois[,i+1]
  }
  names(rois)[(grep('Total',names(rois)))] <- c('ROI25','ROI45','ROI75','ROI100','ROI125','ROI150','ROI200')
  names(rois)[(grep('Temperature',names(rois)))] <- 'Temperature'
  names(rois)[(grep('Unix',names(rois)))] <- 'UnixTime'
  names(rois)[(grep('Rho',names(rois)))] <- 'Rho'
  names(rois)[(grep('Pressure',names(rois)))] <- 'Pressure'
  return(rois)
}

#' Plots ROI data in a grid
#' @description  This assumes that the standard file structure is kept intact
#' @param rois dataframe contianing the roi information
#' @param roisel names of rois to be considered, NULL keeps all ROI classes, can be for example c('ROI25', 'ROI75','ROI200')
#' @param skipdive dive for which data should be excluded, NULL keeps all dives, could be for example c(41,43)
#' @param dres Depth resolution in m, default is 1
#' @export
#' @import tidyverse
#' @import ggplot2
#' @import gridExtra
#' @import pals
#' @author Sven Gastauer
#' @return ROI plots

plot_roi <- function(rois,roisel=NULL, skipdive=NULL,dres=1){
  rois$Depth_r = round(rois$Pressure/dres)*dres

  roinames = names(rois)[grepl('ROI',names(rois))]

  if (is.null(roisel)){roisel = roinames}
  if(is.null(skipdive)){skipdive = 0}

  roi_gg <-rois %>% group_by(Dive,Depth_r) %>% summarize_at(roinames, sum)%>%
    gather(variable, value, -c(Dive,Depth_r))
  roi_gg = roi_gg[roi_gg$variable %in% roisel,]
  roi_gg = roi_gg[!(roi_gg$Dive %in% skipdive),]

  plot_func <- function(df, name) {
    ggplot(data = df, aes(x = Dive, y = Depth_r, fill = value)) +
      geom_tile() +
      xlab('')+
      ylab('')+
      scale_y_reverse()+
      scale_fill_gradientn(colors=rev(pals::brewer.rdylbu(15)), name=name)+
      theme_classic()+
      theme(legend.text = element_text(size=10),
            legend.title = element_text(size=10))
    }

  nested_rois <- roi_gg %>%
    group_by(variable) %>%
    nest() %>%
    mutate(plots = map2(data, variable, plot_func))
  gridExtra::grid.arrange(grobs = nested_rois$plots,
                          bottom = "Dive #",
                          left = "Depth [m]")
}
