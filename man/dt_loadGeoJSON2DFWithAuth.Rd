% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{dt_loadGeoJSON2DFWithAuth}
\alias{dt_loadGeoJSON2DFWithAuth}
\title{load geojson from url (username/password protected) into a data.frame object. this is particularly useful to handle geojson wfsurl with null geometry}
\usage{
dt_loadGeoJSON2DFWithAuth(url, username, password)
}
\arguments{
\item{url}{A geojson url string}

\item{username}{username required to access url}

\item{password}{password required to access url}
}
\value{
A data.frame object
}
\description{
load geojson from url (username/password protected) into a data.frame object. this is particularly useful to handle geojson wfsurl with null geometry
}
