% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun.R
\name{get_daynight}
\alias{get_daynight}
\title{Gets GPS, daytime and day/night for a given missin per dive}
\usage{
get_daynight(ncpath, mission)
}
\arguments{
\item{ncpath}{path to the ZOnar netCDF files}

\item{mission}{string which is the name of the mission to be loaded}
}
\value{
dataframe conatining per dive start and end lon, lat, and time, as well as day/dusk/dawn/nigth for each dive, based on the mean location and time
}
\description{
Gets GPS, daytime and day/night for a given missin per dive
}
\author{
Sven Gastauer
}
