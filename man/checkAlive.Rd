% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/instances.R
\name{checkAlive}
\alias{checkAlive}
\title{Check whether or not an EC2 instance is running.}
\usage{
checkAlive(instance)
}
\arguments{
\item{instance}{An instance objected created with
\code{\link{launchInstance}} or \code{\link{getInstanceDescription}}.}
}
\value{
TRUE or FALSE.  All conditions except the "running" state return
  FALSE (including AWS API and connection errors).
}
\description{
Check whether or not an EC2 instance is running.
}
