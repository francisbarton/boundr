% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_geometry_to_table.R
\name{arrange_service_nms_by_res}
\alias{arrange_service_nms_by_res}
\title{Arrange geo schema by preferred resolutions (only applies when no user pref)}
\usage{
arrange_service_nms_by_res(x, r = res_codes())
}
\arguments{
\item{x}{schema tibble}

\item{r}{vector of resolution codes e.g. "BGC" "BFE"}
}
\description{
Arrange geo schema by preferred resolutions (only applies when no user pref)
}
\keyword{internal}
