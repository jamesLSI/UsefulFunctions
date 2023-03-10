% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/usefulFunctionsPackage.R
\name{companies_house_function}
\alias{companies_house_function}
\title{Companies House Data}
\usage{
companies_house_function(crn_list)
}
\arguments{
\item{crn_list}{is a tibble of companies house reference numbers}
}
\value{
a tibble containing information from the companies house API
}
\description{
Companies House Data
}
\examples{
crn_list <- data \%>\%
    distinct(crn)
companies_house_function(crn_list = crn_list)
}
