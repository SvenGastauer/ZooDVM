#' Get thermocline or another *cline
#' @description Computes slopes (m_x) between each input value pair as
#' \deqn{ m_x = \frac{temp_{x+1} - temp_x}{depth_{x+1} - depth_x}}{%
#' m_x = (temp_[x+1] - temp_x) / (depth_[x+1] - depth_x)}
#' The maximum slope (max([m_1,...m_n])) is assumed to represent a thermocline. \cr
#' Slopes in positive or negative direction or in a single direction can be considered.\cr
#' A threshold range around the maximum slope can be defined. \cr
#' Rolling median values can be used to minimze the influence from few outliers.\cr
#' Maximum or minimum values as well as depths to be considered can be defined}
#' @param temps array of temperatures, must be the same length ans depths
#' @param depths array of depths, must be the same length as temps
#' @param tmax set a maximum value to be considered, all values above this threshold will be set to tmax, NULL for no maximum, defaults NULL
#' @param tmin set a minimum value to be considered, all values below this threshold will be set to tmin, NULL for no minimum, defaults NULL
#' @param pac proportion to be considered a thermocline around the maximum slope detected, defaults 5% = 0.05
#' @param r defines the window size of rolling median to be computed, this is handy if there are a few single outliers, in the dataset, defaults to 1 which is a rolling median with a window size of 1, which equals all raw values
#' @param cw defines if positive, negative or both slopes should be considered, cw = both directions, c=from warm to cold (large to low value), w = from cold to warm only (low to large value), defaults c
#' @param dmax defines a maximum depth to be considered, greater depths will be ignored, defaults NULL, no maximum depth
#' @param dmin defines a minimum depth to be considered, shallower depths will be ignored, defaults NULL, no minimum depth
#' @import zoo
#' @export
#' @author Sven Gastauer
#'
thermocline = function(temps, depths, tmax=NULL, tmin=NULL, pac=0.05, r=1, cw='c', dmax=NULL, dmin=NULL){
  #set tmax
  depths = depths[!is.na(temps)]
  temps = temps[!is.na(temps)]
  temps =  temps[!is.na(depths)]

  if(!is.null(tmax)){temps[temps>=tmax] = tmax}
  if(!is.null(tmin)){temps[temps<=tmin] = tmin}
  if(!is.null(dmax)){
    temps=temps[depths<=dmax]
    depths = depths[depths<=dmax]
  }
  if(!is.null(dmin)){
    temps=temps[depths>=dmin]
    depths = depths[depths>=dmin]
  }else{dmin=0}
  #Get slopes
  slopes = diff(zoo::rollmedian(temps,r))/diff(zoo::rollmedian(depths,r))

  if(cw=='c'){
    slopes[slopes>0] = 0
  }else{
    if(cw=='w'){
      slopes[slopes<0] = 0
    }
  }
  slopes <- abs(slopes)
  inds <- which(slopes < (max(slopes[is.finite(slopes)])*(1+pac)) & slopes > (max(slopes[is.finite(slopes)])*(1-pac)))

  if(length(inds)>0){
    return(zoo::rollmedian(depths,r)[inds[1]])
  }else{
    return(dmin)
  }
}
