% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readxls.R
\name{readxls}
\alias{readxls}
\title{Read bunch of Excel files and unmess}
\usage{
readxls(xlsfiles)
}
\arguments{
\item{xls.files}{a vector with xls files}
}
\value{
list object with cleaned xls files
}
\description{
Set a path to a folder with Excel files. 
This function will then read them and clean the first mess (empty columns, empty rows, illegal column names).
}

