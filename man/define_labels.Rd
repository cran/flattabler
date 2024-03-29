% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pivot_table.R
\name{define_labels}
\alias{define_labels}
\alias{define_labels.pivot_table}
\title{Define the quantity of rows and columns that contain labels}
\usage{
define_labels(pt, n_col, n_row)

\method{define_labels}{pivot_table}(pt, n_col, n_row)
}
\arguments{
\item{pt}{A \code{pivot_table} object.}

\item{n_col}{A number, quantity of columns containing pivot table labels.}

\item{n_row}{A number, quantity of rows containing pivot table labels.}
}
\value{
A \code{pivot_table} object.
}
\description{
A pivot table should only contain label rows and columns, and an array of
values, usually numeric data. This function defines the quantity of rows and
columns that contain labels.
}
\examples{

pt <- pt_ex |> define_labels(n_col = 2, n_row = 2)

}
\seealso{
\code{\link{pivot_table}}

Other pivot table definition functions: 
\code{\link{get_page}()},
\code{\link{pivot_table}()},
\code{\link{set_page}()}
}
\concept{pivot table definition functions}
