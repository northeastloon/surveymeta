% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.R
\name{get_vars}
\alias{get_vars}
\title{get variables}
\usage{
get_vars(base_url, survey_id)
}
\arguments{
\item{base_url}{The base url for the catelog (for IHSN this is http://catalog.ihsn.org/index.php/api/catalog/)}

\item{survey_id}{The survey id. Corresponds to idno returned from \code{get_surveys}}
}
\value{
a list with the result and error. Result is the dataframe of available variables. Err is a logical variable and is TRUE when the api request failed.
}
\description{
\code{get_vars} Returns a dataframe listing the available variables for a specified survey
}
