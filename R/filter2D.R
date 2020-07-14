#' Applies a 2D filter to the data
#' @param data data containing the x, y and dat avalues
#' @param x window size in x direction, defaults 3
#' @param y window size in y direction, defaults 3
#' @param xval the variable in data that describes the x axis, defaults 'Dive'
#' @param yval the varibale in data that describes the y axis, defaults 'Depth_r'
#' @param val the variable that describes the value variable in data, defaults 'Sv'
#' @param log if TRUE the value variable will be linearised andbefore the running calculation and backtransformed before being returned, defaults TRUE
#' @param fun string describing the funciton to be performed, defaults to 'mean', other examples would be meidan, max, min, sd, var,...
#' @export
#' @import raster
#'
filter2d <- function(data, x=3,y=3,xval='Dive',yval='Depth_r', val='Sv', log=TRUE,fun='mean'){
  sub <- data[,c(xval,yval,val)]

  if(log==TRUE){sub[,val] <- 10^(sub[,val]/10)}

  ra = rasterFromXYZ(sub)
  ra2=as.data.frame(focal(ra, matrix(1, x, y), eval(parse(text=fun)), pad = T, padValue = 0))
  names(ra2) = val

  if (log ==TRUE){ra2[,val] <- 10 * log10(ra2[,val])}

  dives = ceiling(ra@extent@xmin):floor(ra@extent@xmax)
  depths = ceiling(ra@extent@ymin):floor(ra@extent@ymax)
  ra2[,xval]= rep(dives, times=length(depths))
  ra2[,yval]= rep(rev(depths), each=length(dives))
  #ra2$variable=val
  ra2 = left_join(data[,names(data)!=val],ra2)

  return(ra2)
}

#' Simple anomaly filter
#' @description Computes a mean or median or whatever funciton is selected on a per depth bin basis for the entire mission (in the linear domain, back-transformed into log-space), which will then be subtracted from each value point, to compute an anomaly matrix. Additionally these values will then be scaled to fall within -1 and 1 for easier interpretation.
#' @param data data containing the x, y and dat avalues
#' @param fun string describing the funciton to be performed, defaults to 'mean', other examples would be meidan, max, min, sd, var,...
#' @export
#' @import dplyr
#'
anomaly <- function(data, fun='median', replace=FALSE, v='SvANomal'){
  ano = data %>%
    na.omit()%>%
    group_by(Dive,Depth_r) %>%
    summarise(Svlin=10^(Sv/10),
              Sv=Sv)%>%
    group_by(Depth_r) %>%
    na.omit()%>%
    mutate(SvAnomal = (Sv - 10*log10(eval(parse(text=fun))(Svlin))))
  ano$scaledAnomaly = (ano$SvAnomal - min(ano$SvAnomal)) / (max(ano$SvAnomal) - min(ano$SvAnomal)) * 2 -1
  data = left_join(data,ano[, c('Dive','Depth_r','SvAnomal','scaledAnomaly')])
  if(replace==TRUE){data$Sv = data[,v]}
  data$variable = 'Scaled Anomaly'
  return(data)
}
