\name{extract.table}
\alias{extract.table}
\title{extract the subtable by rows and cells}
\usage{
  extract.table(geography, input.cells,
    output.cells = NULL,output.cell.names = NULL)
}
\arguments{
  \item{geography}{the data.frame use \code{\link{area.lookup}}}

  \item{input.cells}{the vector of codes what the user
  knew or use \code{\link{input.cells}} function.}

  \item{output.cells}{default is \code{NULL}, the vector
  of codes what the user knew or use
  \code{\link{output.cells}} function (default is
  input.cells).}

  \item{output.cell.names}{default is \code{NULL}, the vector of names
  for column, use the corresponding name (default is
  code).}
}
\value{
  return subtable
}
\description{
  Extract the sub-table and output it or aggregation by
  some ways, such as cells or rows.
}
\author{
  Yiming Li
}
\seealso{
  \code{\link{area.lookup}}, \code{\link{input.cells}} and
  \code{\link{output.cells}}
}
\examples{
  extract.table(geography=area.lookup(code="E00033251",level="oa"),input.cells=input.cells("KS101EW",c(1:2)),
output.cells=output.cells(list(c("rowSums","KS101EW0001","KS101EW0002"))),cname=c("district","code","sumColumn"))

  extract.table(geography=area.lookup(name=c("Liverpool","Newcastle upon Tyne"),level="oa"),input.cells=input.cells("KS102EW",c
(2:6,14:17)),output.cells=output.cells(list(c("rowSums",input.cells("KS102EW",c(2:6,14:17))))))
}

