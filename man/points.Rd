% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/points.R
\name{points}
\alias{points}
\title{Return centroid points at a specified level and area from the ONS OG API}
\usage{
points(
  lookup,
  within = NULL,
  within_names = NULL,
  within_codes = NULL,
  return_width = c("tidy", "full", "minimal"),
  lookup_year = NULL,
  within_year = NULL,
  country_filter = c("UK|GB|EW|EN|SC|WA", "UK", "GB", "EW", "EN", "SC", "WA"),
  resolution = c("BGC", "BSC", "BUC", "BFC", "BFE"),
  option = NULL,
  crs = 4326
)
}
\arguments{
\item{lookup}{character. Lower level area code eg "lsoa", "wd", "lad".
Equivalent to the \code{lookup} parameter in \code{bounds()}.}

\item{within}{character. Higher level area code eg "lad", "cty". Equivalent
to the \code{within} parameter in \code{bounds()}.}

\item{within_names, within_codes}{character. In order to restrict data
returned to a specific area, either \code{within_names} or \code{within_codes} must
be provided. Otherwise all boundaries within the country at that level will
be retrieved. Use place names eg "Essex" to restrict to a certain
geographical area. Use ONS area codes eg "W02000103" likewise (this is
useful with wards, where there are many that share identical names). A
vector of multiple names or codes can be supplied.}

\item{return_width}{character. How many of the possible columns in the
returned table to keep. Options are "tidy", "full" or "minimal".}

\item{lookup_year}{numeric or character. A specific year for data relating
to parameter \code{x}, if needed. Defaults to \code{NULL}, which will return the most
recent data.}

\item{within_year}{numeric or character. A specific year for data relating
to parameter \code{y}, if needed. Defaults to \code{NULL}, which will return the most
recent data.}

\item{country_filter}{character. Open Geography datasets are sometimes
available just within certain countries. Specify a country code if you want
your results restricted to a certain country only - eg "WA" for Wales, "EW"
for England and Wales. By default returns all options.}

\item{resolution}{character. See argument options for available options. How
generalised should the boundary be, and whether coastal boundaries should
adhere to the coastline or to the full territorial extent. BGC by default
(G = Generalised (20m), C = limited to the coastline.) F indicates Full
resolution; S indicates Super-generalised (200m); U indicates Ultra-
generalised (500m) boundary resolution. Use E instead of C for full
extent boundaries (e.g. BFE). Not all combinations are available.}

\item{option}{numeric. Defaults to 1, which means that the URL will just be
the first one from the list of possible services resulting from the level
and year filters above. If this does not give you what you want, you can
run the script again with a different option from the list.}

\item{crs}{the Coordinate Reference System (CRS) code to use. 4326 by
default.}
}
\value{
an \code{sfc} tibble (data frame with geometry)
}
\description{
Return centroid points at a specified level and area from the ONS OG API
}
