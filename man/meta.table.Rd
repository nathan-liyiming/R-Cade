\name{meta.table}
\alias{meta.table}
\title{get the meta of tables}
\usage{
  meta.table(code=NULL, title=NULL,population=NULL,annotation=NULL)
}
\arguments{
  \item{code}{the vector, define the table codes}
  \item{title}{the vector, define the title key}
  \item{population}{the vector, define the population key}
  \item{annotation}{the vector, define the annotation ley}
}
\value{
  return the data.frame
}
\description{
  Captures the relevant metadata of table code.
}
\author{
  Yiming Li
}
\examples{
  meta.table("KS101EW")
  
  meta.table("KS102EW",population=NA)
  
  meta.table(code="EW",title=c("age","Age"))
}

