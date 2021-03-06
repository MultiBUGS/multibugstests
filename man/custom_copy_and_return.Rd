% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/files.R
\name{custom_copy_and_return}
\alias{custom_copy_and_return}
\title{Copy a custom file to current working directory}
\usage{
custom_copy_and_return(type, filename, examples_dir)
}
\arguments{
\item{filename}{An arbitrary character string, containing the file name}

\item{examples_dir}{A character vector (length 1), containing the path to
the Examples directory in the MultiBUGS directory}
}
\value{
The path to the just-created file
}
\description{
Copies a custom Examples file to the current working directory and
return the path to the just-created file
}
