% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fiscal_month_to_date.R
\name{fiscal_quarter_to_date}
\alias{fiscal_quarter_to_date}
\title{Fiscal quarter to date}
\usage{
fiscal_quarter_to_date(fiscal_quarter, use_quarter_end = TRUE)
}
\arguments{
\item{fiscal_quarter}{(character) Date in fiscal quarter format (e.g., "FY22 - Apr").}

\item{use_quarter_end}{(character) Use the last day of the quarter? Default is TRUE, set to FALSE to use the first day of the quarter.}
}
\value{
(date) lubridate date object in "yyyy-mm-dd" format.
}
\description{
Fiscal quarter to date
}
