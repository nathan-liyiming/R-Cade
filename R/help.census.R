##' @title give the user the function list
##' @description List all the functions and corresponding to description in the package.
##' @return no return and only output the function list or data frame
##' @author Yiming Li

#  Copyright (C) 2013 University of Liverpool

help.census <- function(){
  tryCatch(
  {
  
  # hint the user to choose
  writeLines("====================================")
  writeLines("Two ways to list all the functions:")
  writeLines("1. Output in console as list")
  writeLines("2. Output in console as data frame")
  writeLines("====================================")
  

  n <- readline("Please input the number: ")

  # judge the input number or character
  if (as.integer(n)==1){
    help.census.txt <- help.functions
    writeLines("-------------------------------------------------------------------")
    for (i in 1:nrow(help.census.txt)){
      
      cat("FUN:\t",as.character(help.census.txt[i,1]),"\n")
      cat("DES:\t",as.character(help.census.txt[i,2]),"\n")
      
      writeLines("-------------------------------------------------------------------")
    }
    remove(help.census.txt)
  } else if(as.integer(n)==2){
    # write the file
    print(help.functions)
  } else{
    # error report
    message("Error: Please input the right number.")
    message("\t\t\t\t\t\t\tWe only accept 1 or 2.")
  } 
  
  },
  # error report
  error=function(e) {
    message("Error: Please input the right number.")
    message("\t\t\t\t\t\t\tWe only accept 1 or 2.")
  })
  
}