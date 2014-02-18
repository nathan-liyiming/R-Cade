##' @title extract the subtable by rows and columns
##' @description Extract the sub-table and output it or aggregation by some ways, such as columns or rows.
##' @param geography required target geography
##' @param input.cells the vector of codes what the user knew or use \code{\link{input.cells}} function.
##' @param output.cells default is \code{NULL}, the vector of codes what the user knew or use \code{\link{output.cells}} function (default is input.columns).
##' @param output.cell.names default is \code{NULL}, the vector of names for column, use the corresponding name (default is code).
##' @return return subtable
##' @author Yiming Li

#  Copyright (C) 2013 University of Liverpool

extract.table <- function(geography,input.cells,
                          output.cells=NULL,
                          output.cell.names=NULL){
  tryCatch(
{
  input.columns = input.cells
  output.columns = output.cells
  cname = output.cell.names
  
  if (ncol(geography) ==1){
    input.rows = geography[[1]]
    output.rows = NULL
  }else{
    input.rows = geography[[2]]
    output.rows = "n"
  }
  
  
  # judge whether the user has input output columns or rows
  # CASE 1
  if (is.null(output.columns)&is.null(output.rows)){
    temp <- as.data.frame(input.rows)
    colnames(temp) <- "GeographyCode"
  
    # read the columns and combine them
    for(i in 1:length(input.columns)){
      file <- read.csv(paste(substr(input.columns[i],1,7),"DATA.CSV",sep=""))
      col <- file[[as.vector(input.columns[i])]]
      match_col <- col[match(temp[, "GeographyCode"],file[, "GeographyCode"])]
    
      name <- colnames(temp)
      temp <- data.frame(temp,match_col)
      colnames(temp) <- c(as.vector(name),as.character(input.columns[i])) 
    
      remove(file)
      remove(col)
      remove(match_col)
      remove(name)
    }
  
    # change the column names
    if (!is.null(cname)){
      colnames(temp) <- as.vector(cname)
    }
    
    return(temp)
  }else{
    # CASE 2
    # ignore the input columns and only conside the output columns
    if(is.null(output.rows)){
      temp <- as.data.frame(input.rows)
      colnames(temp) <- "GeographyCode"
      m <- match(temp[, "GeographyCode"],output.columns[, "GeographyCode"])
      
      for(i in 2:ncol(output.columns)){
        match_col <- output.columns[[i]][m]
        temp <- data.frame(temp,match_col)
        
        remove(match_col)
      }
      colnames(temp) <- as.vector(colnames(output.columns)) 
      
      # change the column names
      if (!is.null(cname)){
        colnames(temp) <- as.vector(cname)
      }
      
      return(temp)
    }
    
    # CASE 3
    # by output rows, get the corresponding "GeographyCode" and combine them
    if(is.null(output.columns)){
      temp <- as.data.frame(input.rows)
      colnames(temp) <- "GeographyCode"
      
      
      # read the columns and combine them
      for(i in 1:length(input.columns)){
        file <- read.csv(paste(substr(input.columns[i],1,7),"DATA.CSV",sep=""))
        col <- file[[as.vector(input.columns[i])]]
        match_col <- col[match(temp[, "GeographyCode"],file[, "GeographyCode"])]
        
        name <- colnames(temp)
        temp <- data.frame(temp,match_col)
        colnames(temp) <- c(as.vector(name),as.character(input.columns[i])) 
        
        remove(file)
        remove(col)
        remove(match_col)
        remove(name)
      }
      
      # find the corresponding to output.rows
      
      names <- colnames(temp)
      
      if (ncol(geography) ==3){
        temp[[1]] <- geography[[3]]
        temp[[1]] <- as.numeric(temp[[1]])/100
        # multiplication
        temp[2:ncol(temp)] <- temp[[1]]*temp[2:ncol(temp)]
      }
      
      temp[[1]] <- geography[[1]]
      colnames(temp) <- c(as.character("target.code"),as.vector(names)[-1])
      names <- colnames(temp)
      
      # use the aggregate by output.rows
      temp <- aggregate(temp[2:ncol(temp)],by=list(temp[[1]]),sum)
      
      colnames(temp) <- names
      
      
      # change the column names
      if (!is.null(cname)){
        colnames(temp) <- as.vector(cname)
      }
      
      return(temp)
    }
    
    # CASE 4:
    # Combine by output rows and columns
    temp <- as.data.frame(input.rows)
    colnames(temp) <- "GeographyCode"
    m <- match(temp[, "GeographyCode"],output.columns[, "GeographyCode"])
    
    # read the columns and combine them
    for(i in 2:ncol(output.columns)){
      match_col <- output.columns[[i]][m]
      temp <- data.frame(temp,match_col)
      
      remove(match_col)
    }
    colnames(temp) <- as.vector(c("GeographyCode",colnames(output.columns)[-1]))
    
    # find the corresponding to output.rows
    
    names <- colnames(temp)
    
    if (ncol(geography) ==3){
      temp[[1]] <- geography[[3]]
      temp[[1]] <- as.numeric(temp[[1]])/100
      # multiplication
      temp[2:ncol(temp)] <- temp[[1]]*temp[2:ncol(temp)]
    }
    
    temp[[1]] <- geography[[1]]
    colnames(temp) <- c(as.character("target.code"),as.vector(names)[-1])
    names <- colnames(temp)
    
    # use the aggregate by output.rows
    temp <- aggregate(temp[2:ncol(temp)],by=list(temp[[1]]),sum)
    
    colnames(temp) <- names
    
    
    # change the column names
    if (!is.null(cname)){
      colnames(temp) <- as.vector(cname)
    }
    
    return(temp)
  }

  
},
# error report
error=function(e) {
  message("Error: Please input the right form.")
  message("------------------------------------------------------------------------------")
  message("E.g.,extract.table(cname=c(\"a\",\"b\",\"c\"),\ninput.rows=input.rows(c(\"Liverpool\",\"Wirral\")),\ninput.columns=input.columns(\"KS101EW\",c(1:2)),\noutput.rows=\"district\",\noutput.columns=output.columns(list(c(\"rowSums\",\"KS101EW0001\",\"KS101EW0002\"))))")
  message("------------------------------------------------------------------------------")
})
}