% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chisq.R
\name{cpg_chisq_chromosome}
\alias{cpg_chisq_chromosome}
\title{Perform chi-squared test for chromosome location of cpgs}
\usage{
cpg_chisq_chromosome(
  x,
  array_type = c("450K", "EPIC"),
  exclude = NULL,
  return_test = FALSE
)
}
\arguments{
\item{x}{vector of cpgs}

\item{array_type}{Type of array annotation data to use. Must be one of
"450K" or "EPIC", defaults to "450K".}

\item{exclude}{Vector of chromosomes to be excludes. Must be prefixed with
"chr".}

\item{return_test}{If true will return test object from chisq.test. Defaults
to FALSE.}
}
\value{
ggplot2 object
}
\description{
The NULL hypoothesis is that the cpgs would be randomly picked between all
cpgs in the array.
}
\examples{
cpg_chisq_chromosome(sample_cpgs)

# Exclude sex chromosomes
cpg_chisq_chromosome(sample_cpgs, exclude = c("chrX", "chrY"))

cpg_chisq_chromosome(sample_cpgs, return_test = TRUE)
}
