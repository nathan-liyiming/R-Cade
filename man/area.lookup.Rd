\name{area.lookup}
\alias{area.lookup}
\title{extract the subtable by rows and cells}
\usage{
  area.lookup(name=NULL,level=NULL,code=NULL,higher.geog=NULL,higher.codes=NULL,higher.names=NULL,
              report.level=F, report.code=T, report.name=T,wild.search=T, best.fit=T)
}
\arguments{
  \item{name}{character string to search for within area names contained in master lookup table }
  
  \item{level}{geographical level(s) for search and output}
  
  \item{code}{character string to search for within unique ONS area codes contained in master lookup table }
  
  \item{higher.geog}{geographical level(s) for supply of names and codes of geographical units within which output areas are nested }
  
  \item{higher.codes}{geographical level(s) for supply of names of geographical units within which output areas are nested (if higher.names != NULL, over-rides higher.geog)}
  
  \item{higher.names}{geographical level(s) for supply of codes of geographical units within which output areas are nested (if higher.codes != NULL, over-rides higher.geog)}
  
  \item{report.level}{include a column for level in returned dataframe}
  
  \item{report.code}{include a column for area code in returned dataframe }    
                                                                   
  \item{report.names}{ include a column for area name in returned dataframe }
  
  \item{wild.search}{whether searches should be conducted using wild card  or exact match  approach.}
  
  \item{best.fit}{whether to search the "best fit" or "exact match" version of master lookup table. Applies only if level="OA" AND the relevant best-fit geography has been supplied by ONS.}     
  
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
\examples{
area.lookup()

area.lookup(level="region")

area.lookup(level="region",report.level=TRUE)

area.lookup(name="pool")

area.lookup(name="Mary",level="ward", higher.geog="ALL")

area.lookup(name="Mary",level="ward", higher.geog=c("Region","District"))

area.lookup(code="E05")

area.lookup(code=c("E05000887","E05000896"),level="oa",
report.name=F)

area.lookup(code=c("E05000887"),level="oa")
}
