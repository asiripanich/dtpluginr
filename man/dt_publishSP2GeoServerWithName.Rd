% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{dt_publishSP2GeoServerWithName}
\alias{dt_publishSP2GeoServerWithName}
\title{publish a sp object to geoserver with wms stlying information}
\usage{
dt_publishSP2GeoServerWithName(
  spobj,
  layerprefix = "my_stl_",
  layerdisplyname = ""
)
}
\arguments{
\item{spobj}{a sp object. \emph{IMPORTANT: Each column of spobj MUST NOT have attribute name exceeding length (int)10, failed to do so may result in NO Style being generated on GeoServer}}

\item{layerprefix}{a prefix for layer name, will make the generated data layers in GeoServer more identifiable}

\item{displayname}{a readable display name for data layer}
}
\value{
a list contains single element to be included in geolayers
}
\description{
publish a sp object to geoserver with wms stlying information
}
