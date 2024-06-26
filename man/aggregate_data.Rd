% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregate_data.R
\name{aggregate_data}
\alias{aggregate_data}
\title{Aggregate the data in time and space}
\usage{
aggregate_data(data, time_acc = "year", space_acc = 10)
}
\arguments{
\item{time_acc}{year, season, month, day. Defaults to year.}

\item{space_acc}{year, season, month, day. Defaults to 10.}
}
\value{
a tibble containing variables x, metric (name) and value, and possibly xcat
}
\description{
Aggregate the data in time and space
}
\examples{
data=readr::read_csv("data-raw/data_Lhassa.csv")
dat=aggregate_data(data, time_acc="year")
}
