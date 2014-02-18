##' @title get all the cells codes
##' @description Output all the cells codes for special table code.
##' @param table_code the vector, special table codes
##' @return no return and only output them
##' @author Yiming Li

#  Copyright (C) 2013 University of Liverpool

table.cells <- function(table_code){
  tryCatch(
  {
  
  csv_file <- read.csv(paste(table_code,"DESC0.CSV",sep=""))
  
  # output all the cells codes by table codes
  writeLines("=======================================")
  for (i in 1:nrow(csv_file)){
    writeLines(as.character(csv_file[i,1]))
  }
  writeLines("=======================================")
  
  remove(csv_file)
  },
  
  error=function(e) {
    message("Error: Please input the right table code.")
    message("\t\t\t\t\t\t\tWe only accept table code listed in the current directory.")
  })
}
  