% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query_functions.R
\name{table_data_req}
\alias{table_data_req}
\title{Build an API request for table data}
\usage{
table_data_req(ids, url, fields, ...)
}
\arguments{
\item{ids}{the IDs of the data to be requested}

\item{url}{The API URL to query}

\item{fields}{which fields to include in the table returned}

\item{...}{any arguments to be passed to \code{opengeo_api_req()}}
}
\description{
Build an API request for table data
}
