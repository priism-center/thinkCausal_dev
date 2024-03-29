% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_detect_column_types.R
\name{clean_detect_column_types}
\alias{clean_detect_column_types}
\title{Return a list of the column types}
\usage{
clean_detect_column_types(.data)
}
\arguments{
\item{.data}{a dataframe}
}
\value{

}
\description{
Categorizes the types of columns in a dataframe by categorical or continuous.
}
\examples{
X <- data.frame(X1 = 1:5, X2 = rnorm(5), X3 = LETTERS[1:5], X4 = as.factor(LETTERS[1:5]))
clean_detect_column_types(X)
}
