% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/open_fsk.R
\name{read_visualization}
\alias{read_visualization}
\title{Reads the visualization script in an FSK model}
\usage{
read_visualization(file_dir, check = FALSE, filename = "visualization.R")
}
\arguments{
\item{file_dir}{path to the file.}

\item{check}{Whether to make checks. FALSE by default.}

\item{filename}{Name of the file whith the information (visualization.R by default).}
}
\value{
A character string with the contents of the R file.
}
\description{
Reads the visualization script in an FSK model
}
