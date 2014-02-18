##' @title get the meta of columns
##' @description Captures the relevant metadata of column code.
##' @param code the vector, define the table codes or column codes, can be \code{NULL}
##' @param measurement the vector, define the measurement name to select, can be \code{NULL}
##' @param unit the vector, define the unit name to select, can be \code{NULL}
##' @param title the vector, define the title name to select, can be \code{NULL}
##' @return the data.frame
##' @author Yiming Li

#  Copyright (C) 2013 University of Liverpool

meta.cell <- function(code=NULL,measurement=NULL,unit=NULL,title=NULL){
  tryCatch(
  {
    # read the description table by column_code
    # output the imformation with column codes
    if (is.null(code)){
      table_code <- c("KS101EW", "KS102EW", "KS103EW", "KS104EW", "KS105EW", "KS106EW", 
                      "KS107EW", "KS201EW", "KS202EW", "KS204EW", "KS205EW", "KS206EW", "KS207WA", 
                      "KS208WA", "KS209EW", "KS301EW", "KS401EW", "KS402EW", "KS403EW", "KS404EW", 
                      "KS405EW", "KS501EW", "KS601EW", "KS602EW", "KS603EW", "KS604EW", "KS605EW", 
                      "KS606EW", "KS607EW", "KS608EW", "KS609EW", "KS610EW", "KS611EW", "KS612EW", 
                      "KS613EW")
    }else if(nchar(code[1])!=11){
      table_code <- c("KS101EW", "KS102EW", "KS103EW", "KS104EW", "KS105EW", "KS106EW", 
                      "KS107EW", "KS201EW", "KS202EW", "KS204EW", "KS205EW", "KS206EW", "KS207WA", 
                      "KS208WA", "KS209EW", "KS301EW", "KS401EW", "KS402EW", "KS403EW", "KS404EW", 
                      "KS405EW", "KS501EW", "KS601EW", "KS602EW", "KS603EW", "KS604EW", "KS605EW", 
                      "KS606EW", "KS607EW", "KS608EW", "KS609EW", "KS610EW", "KS611EW", "KS612EW", 
                      "KS613EW")
      code <- paste("^.*",code,sep="") 
      table_code <- table_code[grepl(paste(code,collapse="|"),table_code)]
      
    }else{
      table_code=code
    }
    table_list <- paste(substr(table_code,1,7),"DESC0.CSV",sep="")
    
    temp = data.frame()
    for (m in 1:length(table_list)){
      csv_file <- read.csv(table_list[m])
      temp = rbind(temp,csv_file)
      
      if (!is.null(code)){
        if (nchar(code[m])==11){
           temp <- temp[grepl(code[m],temp[[1]]),]
        }
      }

      remove(csv_file)
    }
    
    nmeasurement = T
    if (!is.null(measurement)){
      if (is.na(measurement[1])){
        nmeasurement = F 
      }else{
        measurement <- paste("^.*",measurement,sep="")
        temp <- temp[grepl(paste(measurement,collapse="|"),temp[[2]]),]
      } 
    }
    
    nunit = T
    if (!is.null(unit)){
      if (is.na(unit[1])){
        nunit = F 
      }else{
        unit <- paste("^.*",unit,sep="")
        temp <- temp[grepl(paste(unit,collapse="|"),temp[[3]]),]
      }  
    }
    
    ntitle = T
    if (!is.null(title)){
      if (is.na(title[1])){
        ntitle = F 
      }else{
        title <- paste("^.*",title,sep="")
        temp <- temp[grepl(paste(title,collapse="|"),temp[[4]]),]
      }
    }
    
    temp <- temp[c(T,nmeasurement,nunit,ntitle)]
    colnames(temp) <- c("code","measurement","unit","title")[c(T,nmeasurement,nunit,ntitle)]
    
    remove(table_list)
    return(data.frame(temp,row.names=NULL))
  },
  error=function(e) {
    # error report
    message("Error: Please input the right codes.")
    message("\t\t\t\t\t\t\tWe only accept legal codes in the current working directory.")
  })
}