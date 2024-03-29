% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_progress_bar.R
\name{create_progress_bar}
\alias{create_progress_bar}
\title{Create a progress bar}
\usage{
create_progress_bar(progress)
}
\arguments{
\item{progress}{integer between 0 and 100 representing the fill percentage}
}
\value{
HTML
}
\description{
Create a bootstrap HTML progress bar filled. `progress` specifies the fill percentage of the bar.
}
\examples{
create_progress_bar(80)
}
\author{
Joe Marlo
}
