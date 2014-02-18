\name{meta.cells}
\alias{meta.cells}
\title{get the meta of cells}
\usage{
  meta.cells(code=NULL, measurement=NULL,unit=NULL,title=NULL)
}
\arguments{
  \item{code}{the vector, define the table codes}
  \item{measurement}{the vector, define the measurement key}
  \item{unit}{the vector, define the unit key}
  \item{title}{the vector, define the title ley}
}
\value{
  return the data.frame
}
\description{
  Captures the relevant metadata of column code.
}
\author{
  Yiming Li
}
\seealso{
  \code{\link{input.cells}}
}
\examples{
  meta.table("KS101EW0001")
  
  meta.table("KS102EW0001",title=NA)
  
  meta.table(code="KS101EW",title=c("age","Age"))
}

