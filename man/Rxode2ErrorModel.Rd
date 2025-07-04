% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/error_model_rxode2.R
\name{Rxode2ErrorModel}
\alias{Rxode2ErrorModel}
\title{Create a new Rxode2 error model.}
\usage{
Rxode2ErrorModel(
  add = NULL,
  prop = NULL,
  combined1 = FALSE,
  combined2 = FALSE,
  endpoint = NULL
)
}
\arguments{
\item{add}{additive error related variable}

\item{prop}{proportional error related variable}

\item{combined1}{combined1 enabled}

\item{combined2}{combined2 enabled}

\item{endpoint}{related endpoint}
}
\value{
an Rxode2 error model
}
\description{
Create a new Rxode2 error model.
}
