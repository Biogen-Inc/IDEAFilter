% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shiny_vector_filter_numeric_few.R
\name{shiny_vector_filter_numeric_few}
\alias{shiny_vector_filter_numeric_few}
\title{A vector filter for numeric variables with only a few choices}
\usage{
shiny_vector_filter_numeric_few(
  input,
  output,
  session,
  x = shiny::reactive(factor()),
  filter_na = shiny::reactive(FALSE),
  verbose = FALSE
)
}
\arguments{
\item{input}{requisite shiny module field specifying incoming ui input
reactiveValues}

\item{output}{requisite shiny module field capturing output for the shiny
data filter ui}

\item{session}{requisite shiny module field containing the active shiny
session}

\item{x}{The TODO}

\item{filter_na}{The \code{logical} TODO}

\item{verbose}{a \code{logical} value indicating whether or not to print log
statements out to the console}
}
\value{
a \code{\link[shiny]{reactiveValues}} list containing a logical
  vector called "mask" which can be used to filter the provided vector and an
  element "code" which is the expression used to generate the mask.
}
\description{
A vector filter for numeric variables with only a few choices
}
\keyword{internal}
