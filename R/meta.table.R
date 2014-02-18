##' @title get the meta of tables
##' @description Captures the relevant metadata of table code.
##' @param code the vector, define the table codes, can be \code{NULL}
##' @param title the vector, define the title name to select, can be \code{NULL}
##' @param population the vector, define the population name to select, can be \code{NULL}
##' @param annotation the vector, define the annotation name to select, can be \code{NULL}
##' @return no return and only output the meta names
##' @author Yiming Li

#  Copyright (C) 2013 University of Liverpool

meta.table <- function(code=NULL, title=NULL,population=NULL,annotation=NULL){
  tryCatch(
  {
    # read the meta table by table_code
    # output the imformation with table code
    if (is.null(code)){
      table_code <- c("KS101EW", "KS102EW", "KS103EW", "KS104EW", "KS105EW", "KS106EW", 
                      "KS107EW", "KS201EW", "KS202EW", "KS204EW", "KS205EW", "KS206EW", "KS207WA", 
                      "KS208WA", "KS209EW", "KS301EW", "KS401EW", "KS402EW", "KS403EW", "KS404EW", 
                      "KS405EW", "KS501EW", "KS601EW", "KS602EW", "KS603EW", "KS604EW", "KS605EW", 
                      "KS606EW", "KS607EW", "KS608EW", "KS609EW", "KS610EW", "KS611EW", "KS612EW", 
                      "KS613EW")
    }else if(nchar(code[1])!=7){
      table_code <- c("KS101EW", "KS102EW", "KS103EW", "KS104EW", "KS105EW", "KS106EW", 
                      "KS107EW", "KS201EW", "KS202EW", "KS204EW", "KS205EW", "KS206EW", "KS207WA", 
                      "KS208WA", "KS209EW", "KS301EW", "KS401EW", "KS402EW", "KS403EW", "KS404EW", 
                      "KS405EW", "KS501EW", "KS601EW", "KS602EW", "KS603EW", "KS604EW", "KS605EW", 
                      "KS606EW", "KS607EW", "KS608EW", "KS609EW", "KS610EW", "KS611EW", "KS612EW", 
                      "KS613EW")
      code <- paste("^.*",code,sep="") 
      table_code <- table_code[grepl(paste(code,collapse="|"),table_code)]
      
    }else{
      table_code = code
    }
    table_list <- paste(table_code,"META0.CSV",sep="")
    temp = data.frame()
    for (m in 1:length(table_list)){
      csv_file <- read.csv(table_list[m])
      temp = rbind(temp,csv_file)
    }
    
    ntitle = T
    if (!is.null(title)){
      if (is.na(title[1])){
        ntitle = F 
      }else{
        title <- paste("^.*",title,sep="")
        temp <- temp[grepl(paste(title,collapse="|"),temp[[2]]),]
      }
    }
    
    npopulation = T
    if (!is.null(population)){
      if (is.na(population[1])){
        npopulation = F 
      }else{
        population <- paste("^.*",population,sep="")
        temp <- temp[grepl(paste(population,collapse="|"),temp[[3]]),]
      } 
    }
    
    nannotation = T
    if (!is.null(annotation)){
      if (is.na(annotation[1])){
        nannotation = F 
      }else{
        annotation <- paste("^.*",annotation,sep="")
        temp <- temp[grepl(paste(annotation,collapse="|"),temp[[4]]),]
      }  
    }
    temp <- temp[c(T,ntitle,npopulation,nannotation)]
    colnames(temp) <- c("code","title","polulation","annotation")[c(T,ntitle,npopulation,nannotation)]
  
    remove(csv_file)
    remove(table_list)
  
  return(data.frame(temp,row.names=NULL))
  },
  # error report
    error=function(e) {
      message("Error: Please input the right codes.")
      message("\t\t\t\t\t\t\tWe only accept legal codes in the current working directory.")
  })
}