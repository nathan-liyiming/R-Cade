##' @title download the tables online
##' @description Download all the census into local space.
##' @param directory the local directory for user to store data
##' @return no return and only download the tables
##' @author Yiming Li

#  Copyright (C) 2013 University of Liverpool

download <- function(directory){
  tryCatch(
  {
  # Create a list of tables
  table_list <- c("KS101EW", "KS102EW", "KS103EW", "KS104EW", "KS105EW", "KS106EW", 
                  "KS107EW", "KS201EW", "KS202EW", "KS204EW", "KS205EW", "KS206EW", "KS207WA", 
                  "KS208WA", "KS209EW", "KS301EW", "KS401EW", "KS402EW", "KS403EW", "KS404EW", 
                  "KS405EW", "KS501EW", "KS601EW", "KS602EW", "KS603EW", "KS604EW", "KS605EW", 
                  "KS606EW", "KS607EW", "KS608EW", "KS609EW", "KS610EW", "KS611EW", "KS612EW", 
                  "KS613EW")
  
  table_list <- tolower(paste(table_list, "_2011_oa", sep = ""))
  
  

  # Set a download folder for the census CSV files setwd()
  setwd(directory)

  
  # Download Files
  for (n in 1:length(table_list)) {
    file <- as.character(table_list[n])
    temp <- tempfile(fileext = ".zip")
    download.file(paste("http://www.nomisweb.co.uk/output/census/2011/", file, 
                        ".zip", sep = ""), temp)
    unzip(temp, junkpaths = TRUE)
    unlink(temp)
    remove(file)
  }
  remove(table_list)
  
  },
  # error report
  error=function(e) {
    message("Error: Please input the right working directory.")
    message("\t\t\t\t\t\t\tWe only accept legal directory.")
  })
}