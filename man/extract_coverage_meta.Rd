% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{extract_coverage_meta}
\alias{extract_coverage_meta}
\title{extract metadata fields data relevant to analysis of survey coverage}
\usage{
extract_coverage_meta(meta)
}
\arguments{
\item{meta}{A metadata response object (represented by an r list) from the '/catalog/{IDNo}/variables/{Surveyid}' endpoint}
}
\value{
a dataframe
}
\description{
\code{extract_coverage_meta} selectively extracts fields related to survey coverage.
}
