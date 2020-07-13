#' Get acoustic calibration information from netCDF file
#' @description
#' @param ncpath folder that contains the netCDF files
#' @param mission name or index number of the mission to be used
#' @export
#' @import  ncdf4
#' @return dataframe containing  acoustic calibraiton information
#' @author Sven Gastauer
#'
get_accal <- function(ncpath, mission){
  if (is.numeric(mission)){
    mission = get_mission(ncpath, ending='*.nc', patterns=NULL)[mission]
  }
  message(Sys.time(),': Selected mission - ',mission)

  nc <- ncdf4::nc_open(paste0(ncpath, '/' ,mission))

  ac_cal = data.frame(
    Frequency = ncvar_get(nc, 'Calibration/Frequency'),
    Counts_per_dB = ncvar_get(nc, 'Calibration/gn'),
    nBin = ncvar_get(nc, 'Calibration/nBin'),
    nSamples = ncvar_get(nc, 'Calibration/nScan'),
    Tau = ncvar_get(nc, 'Calibration/tau'),
    Beam_deg = ncvar_get(nc, 'Calibration/beam_deg'),
    tPing = ncvar_get(nc, 'Calibration/tPing'),
    tScan = ncvar_get(nc, 'Calibration/tScan'),
    blank = ncvar_get(nc, 'Calibration/blank'),
    dt = ncvar_get(nc, 'Calibration/dt'),
    Source_Level = ncvar_get(nc, 'Calibration/SoureLevel'),
    Gain_system = ncvar_get(nc, 'Calibration/Gain'),
    Gain_cal = ncvar_get(nc, 'Calibration/TSGain'),
    Noise_cal = ncvar_get(nc, 'Calibration/CalNoise')
  )

  ac_cal = data.frame(t(ac_cal))
  names(ac_cal) <- c('Beam 1', 'Beam 2')
  ac_cal$units = ''
  ac_cal$Description = ''
  add_ud <- function(data=ac_cal,vnam,unit,desc){
    data$units[row.names(data) == vnam] <- unit
    data$Description[row.names(data) == vnam] <- desc
    return(data)
  }

  ac_cal <- add_ud(ac_cal, 'Frequency','kHz','center Frequency')
  ac_cal <- add_ud(ac_cal, 'Counts_per_dB','Counts','# Counts to translate raw counts into dB re V')
  ac_cal <- add_ud(ac_cal, 'nBin','#','# of points to average per bin')
  ac_cal <- add_ud(ac_cal, 'nSamples','#','# scans to take')
  ac_cal <- add_ud(ac_cal, 'Tau','ms','Pulse Duration')
  ac_cal <- add_ud(ac_cal, 'Beam_deg','degrees','3 dB Beam Width')
  ac_cal <- add_ud(ac_cal, 'tPing','ms','Interval between pings')
  ac_cal <- add_ud(ac_cal, 'tScan','ms','duration to take scans')
  ac_cal <- add_ud(ac_cal, 'blank','micro-sec','Time between end of transmit and first scan')
  ac_cal <- add_ud(ac_cal, 'dt','micro-sec','Period between scans (1/sampling rate in Hz)')
  ac_cal <- add_ud(ac_cal, 'Source_Level','dB re m2','Source level approximation from calibration')
  ac_cal <- add_ud(ac_cal, 'Gain_system','dB re V','System gain defined by electronics')
  ac_cal <- add_ud(ac_cal, 'Gain_cal','dB re m2','Calibration derived gain')
  ac_cal <- add_ud(ac_cal, 'Noise_cal','#','Raw noise counts during calibration')

  return(ac_cal)
}
 # knitr::kable(ac_cal, "latex", caption = "Acoustic calibration values", booktabs = T) %>%
   # kable_styling(latex_options = c("striped", "hold_position"))
