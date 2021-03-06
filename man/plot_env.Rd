% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_env.R
\name{plot_env}
\alias{plot_env}
\title{Plots the environmental data extracted from netCDF files}
\usage{
plot_env(data, var = "temp", name = "", cmap = "RdYlBu", lims = NULL)
}
\arguments{
\item{data}{dataframe from get_env() function}

\item{var}{name of the variable in data to be plotted, defaults to 'temp', options are 'temp', 'sal', 'fluo'}

\item{name}{string tha tis used as a title for the legend , defaults to '',}

\item{cmap}{COlormap to be used, defaults to 'RdYlBu'}

\item{lims}{either NULL or two values decriubing the min and max of the plotted variable. NULL will take the min and max of the data, defaults to NULL.}
}
\value{
ggplot plot
}
\description{
Plots the environmental data extracted from netCDF files
}
\author{
Sven Gastauer
}
