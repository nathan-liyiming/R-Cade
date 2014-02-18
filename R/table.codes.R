##' @title get all the table codes
##' @description Output all the table codes for user.
##' @return no return and only output them
##' @author Yiming Li

#  Copyright (C) 2013 University of Liverpool

table.codes <- function(){
  # Create a list of tables
  table_list <- c("KS101EW", "KS102EW", "KS103EW", "KS104EW", "KS105EW", "KS106EW", 
                  "KS107EW", "KS201EW", "KS202EW", "KS204EW", "KS205EW", "KS206EW", "KS207WA", 
                  "KS208WA", "KS209EW", "KS301EW", "KS401EW", "KS402EW", "KS403EW", "KS404EW", 
                  "KS405EW", "KS501EW", "KS601EW", "KS602EW", "KS603EW", "KS604EW", "KS605EW", 
                  "KS606EW", "KS607EW", "KS608EW", "KS609EW", "KS610EW", "KS611EW", "KS612EW", 
                  "KS613EW")
#   writeLines("=======================================")
#   for (i in 1:length(table_list)){
#     writeLines(table_list[i])
#   }
#   writeLines("---------------------------------------")
#   writeLines("Eg: kS101EW-table 101 England and Wales")
#   writeLines("=======================================")
  
  return(table_list)
  remove(table_list)
}