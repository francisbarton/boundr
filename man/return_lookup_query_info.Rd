% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/return_lookup_query_info.R
\name{return_lookup_query_info}
\alias{return_lookup_query_info}
\title{Return lookup query URL and lower and higher field codes}
\usage{
return_lookup_query_info(
  lookup,
  within,
  lookup_year,
  within_year,
  country_filter,
  option,
  chatty
)
}
\arguments{
\item{lookup}{character. Lower level area code eg "lsoa", "wd", "lad".
Equivalent to the \code{lookup} parameter in \code{bounds()}.}

\item{within}{character. Higher level area code eg "lad", "cty". Equivalent
to the \code{within} parameter in \code{bounds()}.}

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

\item{option}{numeric. Defaults to 1, which means that the URL will just be
the first one from the list of possible services resulting from the level
and year filters above. If this does not give you what you want, you can
run the script again with a different option from the list.}

\item{chatty}{Boolean. Whether to print feedback on the 'decisions' the
function has taken about which table to query. Default \code{TRUE} when the
function is run in an interactive session, \code{FALSE} otherwise.}
}
\value{
A list of length 3: the query URL, the lower level (\code{lookup}) field
code (eg \code{lsoa11cd}), and the higher level (\code{within}) field code.
}
\description{
Return lookup query URL and lower and higher field codes
}
