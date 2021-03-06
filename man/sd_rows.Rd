% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manip-row-operations.R
\name{sd_rows}
\alias{sd_rows}
\title{Take the standard deviation across multiple columns for each row of a \code{data.frame}}
\usage{
sd_rows(data, string, kind = "starts")
}
\arguments{
\item{data}{a \code{data.frame} to use for taking the standard deviation across rows}

\item{string}{a string that can be used in \code{dplyr}'s \code{select} function.}

\item{kind}{a string indicating which type of \code{dplyr} select helper functions.
Appropriate values are "starts", "ends", or "matches".}
}
\value{
a \code{data.frame} with the newly created variable
}
\description{
Take the standard deviation across multiple columns for each row of a \code{data.frame}
}
\section{Details}{

This function, and those specified below in 'See Also', work best when used
inside a call to \code{dplyr}'s \code{mutate()} for creating new variables.
The point of the function is to make it easy to use regular expressions to
select variables for aggregation, such as an average score for a number of
questionnaire items or the sum of correct responses.
}

\seealso{
\code{\link{sum_rows}} for row summing, \code{\link{sum_missings}} for summing
  missing values, and \code{\link{average_rows}} for row averaging across rows.
}
