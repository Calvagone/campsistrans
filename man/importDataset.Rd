% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nonmem_dataset_parser.R
\name{importDataset}
\alias{importDataset}
\title{Import a NONMEM dataset and prepare it for the qualification.}
\usage{
importDataset(
  campsistrans,
  covariates = NULL,
  etas = FALSE,
  etas_zero = FALSE,
  campsis_id = FALSE
)
}
\arguments{
\item{campsistrans}{campsistrans object}

\item{covariates}{covariates vector. If provided, only these covariates are kept in dataset.
NULL is default (all covariates are kept)}

\item{etas}{import estimated ETA's that were output by $TABLE in control stream.
By default, ETA's are not imported. Please set it to TRUE to enable this feature.}

\item{etas_zero}{if previous argument is set to FALSE, etas_zero set to TRUE will
all ETA's from model to 0 (useful to simulate model without IIV)}

\item{campsis_id}{rework ID column for simulation with CAMPSIS (ID must start at 1 and must be consecutive).
Default is FALSE. Please set it to TRUE if you wish a simulation ID. If TRUE, original ID column is
preserved in column 'ORIGINAL_ID'.}
}
\value{
a data frame
}
\description{
Import a NONMEM dataset and prepare it for the qualification.
}
