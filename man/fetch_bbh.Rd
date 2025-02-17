% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io.R
\name{fetch_bbh}
\alias{fetch_bbh}
\title{Retrieve BBH data from the online portal}
\usage{
fetch_bbh(
 
    uri = "https://opendata.arcgis.com/datasets/5fd6f3e57d794a409d72f47d78f15a32_0.geojson",
  form = c("raw", "data")[2]
)
}
\arguments{
\item{uri}{character, the URI of the dataset}

\item{form}{character, one of 'raw' to retrieve a list will all components or
'data' to retrieve just the data of interest}
}
\value{
a list or a tibble depending upon the value of \code{form}
}
\description{
Retrieve BBH data from the online portal
}
\seealso{
\href{https://dmr-maine.opendata.arcgis.com/datasets/5fd6f3e57d794a409d72f47d78f15a32_0/explore?showTable=true}{DMR Data Portal}
}
