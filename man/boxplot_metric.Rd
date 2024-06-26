% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boxplot_metric.R
\name{boxplot_metric}
\alias{boxplot_metric}
\title{Plots a metric through var_x with boxplots}
\usage{
boxplot_metric(
  dat,
  x = "x_space",
  fill = "x_space_cat",
  facets = "x_time_cat",
  scale_y = "identity",
  add_means = FALSE
)
}
\arguments{
\item{fill}{the name of the variable used to fill with colors}

\item{scale_y}{the scale for Y variable (defaults to "identity")}

\item{add_means}{wheter to add means (as red dots)}

\item{data}{the data table}
}
\value{
boxplots showing the values of the metric through X
}
\description{
Plots a metric through var_x with boxplots
}
\examples{
data=readr::read_csv("data/data_ganga.csv")
dat=get_metric(data,breaks_time="1998;2007",breaks_space="60")
boxplot_metric(dat)
boxplot_metric(dat,scale_y="log10", add_means=T)
boxplot_metric(dat,scale_y="log10", add_means=T,facets="not")
boxplot_metric(dat,scale_y="log10", fill="x_space")
}
