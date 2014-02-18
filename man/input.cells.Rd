\name{input.cells}
\alias{input.cells}
\title{give the user the column codes}
\usage{
  input.cells(table_code, range_of_number)
}
\arguments{
  \item{table_code}{one unique code for table}

  \item{range_of_number}{the range of number, such as
  c(1:4)}
}
\value{
  return the vector of column codes
}
\description{
  Get some cells by input code of table and range of
  number.
}
\author{
  Yiming Li
}
\seealso{
  \code{\link{extract.table}}
}
\examples{
  input.cells("KS101EW",c(1:3))
}

