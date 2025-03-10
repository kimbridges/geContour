% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/contour_to_earth.R
\name{create_contour_kmz}
\alias{create_contour_kmz}
\title{Create a KMZ file bundling the PNG and KML}
\usage{
create_contour_kmz(
  kml_filename,
  png_filename,
  output_filename = "contour_overlay"
)
}
\arguments{
\item{kml_filename}{The KML file to include}

\item{png_filename}{The PNG file to include}

\item{output_filename}{The filename for the KMZ (without extension)}
}
\value{
The filename of the created KMZ
}
\description{
Create a KMZ file bundling the PNG and KML
}
