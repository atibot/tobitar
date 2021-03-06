% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Summarize Fatality Analysis Reporting System data for the years}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{A vector of years}
}
\value{
A data.frame of the number of fatality cases per
each month (rows) for the years(columns)
}
\description{
This function summarizes Fatality Analysis Reporting System
data for the years (using the \code{years} argument) and returns
a data.frame of the number of fatality cases (accidents) per each month
for the years specified.
}
\note{
Any invalid year in the vector of years is ignored for
summary, with warning messages "invalid year: yyyy"
}
\examples{
\dontrun{
head(fars_summarize_years(c(2013,2014,2015)))
}

}
