##' @title give the user the column codes
##' @description Get some cells by input code of table and range of number.
##' @param table_code one unique code for table
##' @param range_of_number the range of number, such as c(1:4)
##' @return return the vector of column codes
##' @seealso \code{\link{extract.table}}
##' @author Yiming Li

#  Copyright (C) 2013 University of Liverpool

input.cells <- function(table_code,range_of_number){
  tryCatch(
  {
  
    # read the description file
    csv_file <- read.csv(paste(table_code,"DESC0.CSV",sep=""))
  
    # return the cells by range
    return(as.vector(csv_file[[1]])[range_of_number])
    
    remove(csv_file)
  },

  # error report
  error=function(e) {
    message("Error: Please input the right table code or range.")
    message("\t\t\t\t\t\t\tWe only accept table code listed in the current directory.")
  })
}