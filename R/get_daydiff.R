#' Computes the difference between neighbouring day or night data, night for day and day for night
#' @param data Sv data
#' @param fun character either mean or median, defualts to median
#' @param days_before How many days before the given data point to include, defaults 1
#' @param days_after How many days after the given data point to include, defaults 1
#' @param replace Boolean, replaces Sv by diff or not, default FALSE
#' @export
#' @import dplyr
#'
get_daydiff = function(data, fun='median', days_before=1, days_after=1, replace=FALSE, date_var='Time_start'){
  data$date = as.numeric(format(as.Date(data[,date_var]), "%j"))
  sun = data$sun
  night = data[data$sun == 'Night',]
  day = data[data$sun == 'Day',]

  if (fun=='median'){
    pnight=lapply(as.list(unique(data$date)),
                  FUN=function(x) night %>%
                    filter(date %in%
                             (x-days_before):(x+days_after))%>%
                    group_by(Depth_r)%>%
                    summarise(Svmean_night = 10*log10(median(10^(na.omit(Sv)/10))),
                              date=x))%>%
      bind_rows()

    pday=lapply(as.list(unique(data$date)),
                FUN=function(x) day %>%
                  filter(date %in% (x-days_before):(x+days_after))%>%
                  group_by(Depth_r)%>%
                  summarise(Svmean_day = 10 * log10(median(10^(na.omit(Sv)/10))),
                            date=x))%>%
      bind_rows()

  }else if(fun=='mean'){
    pnight=lapply(as.list(unique(data$date)),
                  FUN=function(x) night %>%
                    filter(date %in%
                             (x-1):(x+1))%>%
                    group_by(Depth_r)%>%
                    summarise(Svmean_night = 10*log10(mean(10^(na.omit(Sv)/10))),
                              date=x))%>%
      bind_rows()

    pday=lapply(as.list(unique(data$date)),
                FUN=function(x) day %>%
                  filter(date %in% (x-1):(x+1))%>%
                  group_by(Depth_r)%>%
                  summarise(Svmean_day = 10 * log10(mean(10^(na.omit(Sv)/10))),
                            date=x))%>%
      bind_rows()

  }

  p = left_join(pnight, pday)
  #data = data%>%group_by(Dive,Depth_r, sun)%>%summarise(Sv=10*log10(mean(10^(Sv/10))),
  #                                                      Date = mean(date))

  data = data[,!names(data) %in% c('Svmean_day','Svmean_night','diff')]
  data=data %>%group_by(Dive,Depth_r, sun)
  data=left_join(data,p)
  data$Svmean=NA
  data$Svmean[data$sun=='Day'] =  data$Svmean_night[data$sun=='Day']
  data$Svmean[data$sun=='Night'] =  data$Svmean_day[data$sun=='Night']
  data$diff = 10 * log10((10^(data$Sv / 10) - 10^(data$Svmean / 10)))# - min(na.omit(10^(p2$Sv/10) - 10^(p2$Svmean/10))))
  if(replace==TRUE){data$Sv=data$diff}
  data$variable = 'Difference'
  return(data)
}
