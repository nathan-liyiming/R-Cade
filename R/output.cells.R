##' @title get the output cells by function
##' @description Output cells by setting some manipulation of input.cells.
##' @param lists function name with cells
##' @return return the data.frame type
##' @seealso \code{\link{extract.table}}
##' @author Yiming Li

#  Copyright (C) 2013 University of Liverpool

output.cells <- function(lists){
  tryCatch(
  {
    # judge whether it is right form
    if(is.list(lists)&length(lists)!=0){
      temp <- data.frame()
      
      # in the element of lists, it may store the functurn name with cells
      # or only one column
      for (i in 1:length(lists)){
        
        if(length(lists[[i]])>1){
          # CASE 1: function with cells
          
          # first one and we need to set the "GeographyCode"
          if (i==1){
            temp <- as.data.frame(read.csv(paste(substr(lists[[i]][2],1,7),"DATA.CSV",sep=""))[[1]])
            colnames(temp) <- "GeographyCode"
          }
          
          # remove the first function name and combine other cells
          c <- as.vector(lists[[i]])[-1]
          f <- as.data.frame(read.csv(paste(substr(lists[[i]][2],1,7),"DATA.CSV",sep=""))[[1]])
          colnames(f) <- "GeographyCode"
          title <- as.character()
          for (j in 1:length(c)){
            t <- paste(substr(c[j],1,7),"DATA.CSV",sep="")
            csv_file <- read.csv(t)
            
            f <- data.frame(f,csv_file[[as.character(c[j])]][match(f[, "GeographyCode"],
                                                                   csv_file[, "GeographyCode"])])
            
            title <- paste(title,c[j])
            
            remove(t)
            remove(csv_file)
          }
          
          name <- colnames(temp)
          # use the function to combine them 
          temp <- data.frame(temp,get(lists[[i]][1])(f[-1])[match(temp[, "GeographyCode"],
                                                                  f[, "GeographyCode"])])
          colnames(temp) <- c(as.vector(name),as.character(paste(lists[[i]][1],"(",title,")")))    
          remove(c)
          remove(f)
          remove(title)
        }else{
          # CASE 2: only one column
          
          # first one and we need to set the "GeographyCode"
          if (i==1){
            temp <- as.data.frame(read.csv(paste(substr(lists[[i]],1,7),"DATA.CSV",sep=""))[[1]])
            colnames(temp) <- "GeographyCode"
          }
          
          # read the column and combine them
          t <- paste(substr(lists[[i]],1,7),"DATA.CSV",sep="")
          csv_file <- read.csv(t)
          
          name <- colnames(temp)
          temp <- data.frame(temp,csv_file[[as.character(lists[[i]])]][match(temp[, "GeographyCode"],
                                                                             csv_file[, "GeographyCode"])])
          colnames(temp) <- c(as.vector(name),as.character(lists[[i]]))

          remove(t)
          remove(csv_file)
        }
      }
      
      return(temp)
    }else{
      # error report
      message("Error: Please input the right lists.")
      message("\t\t\t\t\t\t\tWe only accept legal lists,e.g.,list(c(\"rowSums\",\"KS101EW0001\",\"KS101EW0002\")).")
    }
  },
  # error report
  error=function(e) {
    message("Error: Please input the right lists.")
    message("\t\t\t\t\t\t\tWe only accept legal lists,e.g.,list(c(\"rowSums\",\"KS101EW0001\",\"KS101EW0001\")).")
  })
}