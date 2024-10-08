% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lookup_query_info.R
\name{process_query_info}
\alias{process_query_info}
\title{Just another piece of the pipeline}
\usage{
process_query_info(
  query_info,
  within_names,
  within_codes,
  return_width,
  query_opt
)
}
\arguments{
\item{within_names, within_codes}{character. In order to restrict data
returned to a specific area, either \code{within_names} or \code{within_codes} must
be provided. Otherwise all available boundaries at that level will be
retrieved. Use place names eg "Essex" to restrict to a certain
geographical area. Or use ONS area codes eg "W02000103" likewise (this is
useful with wards, where there are many that share identical names).
To use this argument to filter \code{within_level}, the \code{within_level} must be
specified! Otherwise \code{{boundr}} will apply them to \code{lookup_level} instead.
See examples.
Vectors of multiple names or multiple codes can be supplied.
If you supply both \code{within_names} and \code{within_codes}, only \code{within_names}
will be used; (\code{within_codes} will be ignored).}
}
\description{
Just another piece of the pipeline
}
\keyword{internal}
