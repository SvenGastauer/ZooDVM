% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun.R
\name{pdvm}
\alias{pdvm}
\title{Get dvm informaiton from Sv data}
\usage{
pdvm(
  ac_group_sub,
  vmin = -85,
  vmax = -45,
  perc = 65,
  dcut = 300,
  scut = 20,
  dr = 1,
  dskip = 0,
  dend = 0,
  updown = "surface"
)
}
\arguments{
\item{ac_group_sub}{Sv dataframe}

\item{vmin}{minimum Sv value to be considered, lower values will be set to the minimum value, defaults to -85}

\item{vmax}{maximum Sv value to be considered, higher values will be set to the maximum value, defaults to -45}

\item{perc}{cutoff cumulative percentage, defaults to 65}

\item{dcut}{depth cut off value, deeper values will be ignored, defaults to 300 m}

\item{scut}{surface cutoff value, shallower values will be ignored, defaults to 20 m}

\item{dr}{depth resolution, defaults to 1}

\item{dskip}{number of dives to skip at the start, defaults to 0}

\item{dend}{number of dives to skip at the end, defaults to 0}
}
\description{
Get dvm informaiton from Sv data
}
