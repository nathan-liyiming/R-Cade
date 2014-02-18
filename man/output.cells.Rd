\name{output.cells}
\alias{output.cells}
\title{get the output cells by function}
\usage{
  output.cells(lists)
}
\arguments{
  \item{lists}{function name with cells}
}
\value{
  return the data.frame type
}
\description{
  Output cells by setting some manipulation of
  input.cells.
}
\author{
  Yiming Li
}
\seealso{
  \code{\link{extract.table}}
}
\examples{
  output.cells(list(c("rowSums","KS101EW0001","KS101EW0002")))
}

