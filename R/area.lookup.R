##' @title query and hint the user to lookup what they need
##' @description Ask the user which country, region, county, district and ward to select. It is similar to \code{\link{sel.area}}. However, it must follow the query and input the choice and from higher level to low level. Until user input "exist" and it will exist the function query.
##' @param name
##' @param level
##' @param code
##' @param higher.geog
##' @param higher.codes
##' @param higher.names
##' @param report.level
##' @param report.code
##' @param report.name
##' @param wild.search
##' @param best.fit
##' @return the dataframe
##' @author Yiming Li

#  Copyright (C) 2013 University of Liverpool


area.lookup <- function(name=NULL, level=NULL, code=NULL,
                        higher.geog=NULL, higher.codes=NULL, higher.names=NULL,
                        report.level=F, report.code=T, report.name=T,
                        wild.search=T, best.fit=T){
  tryCatch(
{
  # read the ordered list and lookup table
  ordered <- level.list
  table <- lookup1
  
  if (is.null(name)&!is.null(code)){
    #(d)
    if (wild.search==F){
      wild = ""
    }else{
      wild="^.*"
    }
    temp <- data.frame()
    
    if (is.null(level)){
      
      if (!is.null(higher.geog)|!is.null(higher.codes)|!is.null(higher.names)){
        message("Error: Please input level when using higher.*.")
        return("Quit")
      }
      
      for(i in 1:nrow(ordered)){
        col <- as.character(ordered[i,2])
        col_code <- as.character(ordered[i,3])
        
        for(j in 1:nrow(table)){
          
          if(grepl(paste(paste(wild,code,sep=""),collapse="|"),as.character(table[j,col_code]),ignore.case = T)){
            temp<-rbind(temp,data.frame(as.character(ordered[i,1]),as.character(table[j,col_code]),as.character(table[j,col])))
          }
        }
      }
      
      temp <- temp[!duplicated(temp[3]),]
      
      temp <- temp[c(report.level,report.code,report.name)]
      colnames(temp) <- c("level","code","name")[c(report.level,report.code,report.name)]
      
    }else{
      
      if (level=="oa"){
        for(i in 1:nrow(ordered)){
          col <- as.character(ordered[i,2])
          col_code <- as.character(ordered[i,3])
          
          for(j in 1:nrow(table)){
            
            if(grepl(paste(paste(wild,code,sep=""),collapse="|"),as.character(table[j,col_code]),ignore.case = T)){
              if (best.fit==F){
                temp<-rbind(temp,data.frame(as.character(table[j,col_code]),as.character(table[j,1])))
              }else{
                temp<-rbind(temp,data.frame(as.character(table[j,col_code]),as.character(table[j,1]),as.character(table[j,"PERCENTAGE_BF"])))
              }
            }
          }
        }
        
        temp <- data.frame(temp,row.names=NULL)
        
        colnames(temp) <- c("target.code","oa.codes","pctg.fit")[c(report.code,T,best.fit)]
        
        return(temp)
      }
      
      if(!is.null(higher.geog)|!is.null(higher.codes)|!is.null(higher.names)){
        for(i in 1:nrow(ordered)){
          if(as.character(ordered[i,1])==as.character(level)){
            col <- as.character(ordered[i,2])
            col_code <- as.character(ordered[i,3])
            
            for(j in 1:nrow(table)){
              
              if(grepl(paste(paste(wild,code,sep=""),collapse="|"),as.character(table[j,col_code]),ignore.case = T)){
                temp<-rbind(temp,data.frame(                        as.character(table[j,2]),
                                                                    as.character(table[j,3]),
                                                                    as.character(table[j,4]),
                                                                    as.character(table[j,5]),
                                                                    as.character(table[j,6]),
                                                                    as.character(table[j,7]),
                                                                    as.character(table[j,8]),
                                                                    as.character(table[j,9]),
                                                                    as.character(table[j,10]),
                                                                    as.character(table[j,11])))
              }
            }
            
            
            break
          }
          if(i==nrow(ordered)){
            message("Error: Please input the right level.")
            return("Quit")
          }
        }
        
        
        
        country.code =F
        country.name=F
        region.code=F
        region.name=F
        county.code=F
        county.name=F
        district.code=F
        district.name=F
        ward.code=F
        ward.name=F
        
        number = 9
        
        if (level=="country"){
          country.code =T
          country.name=T
          number = 1
        }
        
        if (level=="region"){
          region.code =T
          region.name=T
          number = 3
        }
        
        if (level=="county"){
          county.code =T
          county.name=T
          number = 5
        }
        
        if (level=="district"){
          district.code =T
          district.name=T
          number = 7
        }
        
        if (level=="ward"){
          ward.code =T
          ward.name=T
          number = 9
        }
        
        temp <- temp[!duplicated(temp[[number]]),]
        
        if(!is.null(higher.geog)&is.null(higher.names)&is.null(higher.codes)){
          if (higher.geog[1]=="ALL"){
            country.code =T
            country.name=T
            region.code=T
            region.name=T
            county.code=T
            county.name=T
            district.code=T
            district.name=T
            ward.code=T
            ward.name=T
          }else{
            for (y in 1:length(higher.geog)){
              if (higher.geog[y]=="country"){
                country.code =T
                country.name=T
              }
              
              if (higher.geog[y]=="region"){
                region.code =T
                region.name=T
              }
              
              if (higher.geog[y]=="county"){
                county.code =T
                county.name=T
              }
              
              if (higher.geog[y]=="district"){
                district.code =T
                district.name=T
              }
              
              if (higher.geog[y]=="ward"){
                ward.code =T
                ward.name=T
              }
            }
          }
        }
        
        if (!is.null(higher.names)){
          if (higher.names[1]=="ALL"){
            country.name=T
            
            region.name=T
            
            county.name=T
            
            district.name=T
            
            ward.name=T
          }else{
            for (y in 1:length(higher.names)){
              if (higher.names[y]=="country"){
                country.name=T
              }
              
              if (higher.names[y]=="region"){
                region.name=T
              }
              
              if (higher.names[y]=="county"){
                county.name=T
              }
              
              if (higher.names[y]=="district"){
                district.name=T
              }
              
              if (higher.names[y]=="ward"){
                ward.name=T
              }
            }
          }
          
          
        }
        
        if(!is.null(higher.codes)){
          if (higher.codes[1]=="ALL"){
            country.code=T
            
            region.code=T
            
            county.code=T
            
            district.code=T
            
            ward.code=T
          }else{
            for (y in 1:length(higher.codes)){
              if (higher.codes[y]=="country"){
                country.code=T
              }
              
              if (higher.codes[y]=="region"){
                region.code=T
              }
              
              if (higher.codes[y]=="county"){
                county.code=T
              }
              
              if (higher.codes[y]=="district"){
                district.code=T
              }
              
              if (higher.codes[y]=="ward"){
                ward.code=T
              }
            }
          }
        }
        
        temp <- temp[c(country.code,country.name,region.code,region.name,county.code,county.name,district.code,district.name,ward.code,ward.name)]
        
        colnames(temp) <- c("country.code","country.name","region.code","region.name","county.code","county.name","district.code","district.name","ward.code","ward.name")[c(country.code,country.name,region.code,region.name,county.code,county.name,district.code,district.name,ward.code,ward.name)]
        
        return(data.frame(temp,row.names=NULL))
      }
      
      for (x in 1:length(level)){
        for(i in 1:nrow(ordered)){
          if(as.character(ordered[i,1])==as.character(level[x])){
            col <- as.character(ordered[i,2])
            col_code <- as.character(ordered[i,3])
            
            for(j in 1:nrow(table)){
              
              if(grepl(paste(paste(wild,code,sep=""),collapse="|"),as.character(table[j,col_code]),ignore.case = T)){
                temp<-rbind(temp,data.frame(as.character(ordered[i,1]),as.character(table[j,col_code]),as.character(table[j,col])))
              }
            }
            
            
            break
          }
          if(i==nrow(ordered)){
            message("Error: Please input the right level.")
            return("Quit")
          }
        }
      }
      
      temp <- temp[!duplicated(temp[3]),]
      temp <- temp[c(report.level,report.code,report.name)]
      if (length(level)==1){
        colnames(temp) <- c("level",paste(level,".code",sep=""),paste(level,".name",sep=""))[c(report.level,report.code,report.name)]
      }else{
        colnames(temp) <- c("level","code","name")[c(report.level,report.code,report.name)]
      }
      
    }
    
    return(data.frame(temp,row.names=NULL))
  }
  
  if (!is.null(name)&is.null(code)){
    #(C)
    if (wild.search==F){
      wild = ""
    }else{
      wild="^.*"
    }
    temp <- data.frame()
    
    if (is.null(level)){
      
      if (!is.null(higher.geog)|!is.null(higher.codes)|!is.null(higher.names)){
        message("Error: Please input level when using higher.*.")
        return("Quit")
      }
      
      for(i in 1:nrow(ordered)){
          col <- as.character(ordered[i,2])
          col_code <- as.character(ordered[i,3])
        
          for(j in 1:nrow(table)){
          
            if(grepl(paste(paste(wild,name,sep=""),collapse="|"),as.character(table[j,col]),ignore.case = T)){
              temp<-rbind(temp,data.frame(as.character(ordered[i,1]),as.character(table[j,col_code]),as.character(table[j,col])))
            }
          }
      }
    
      temp <- temp[!duplicated(temp[3]),]
    
      temp <- temp[c(report.level,report.code,report.name)]
      colnames(temp) <- c("level","code","name")[c(report.level,report.code,report.name)]
    
    }else{
      
      if (level=="oa"){
        for(i in 1:nrow(ordered)){
          col <- as.character(ordered[i,2])
          col_code <- as.character(ordered[i,3])
          
          for(j in 1:nrow(table)){
            
            if(grepl(paste(paste(wild,name,sep=""),collapse="|"),as.character(table[j,col]),ignore.case = T)){
              if (best.fit==F){
                temp<-rbind(temp,data.frame(as.character(table[j,col_code]),as.character(table[j,1])))
              }else{
                temp<-rbind(temp,data.frame(as.character(table[j,col_code]),as.character(table[j,1]),as.character(table[j,"PERCENTAGE_BF"])))
              }
            }
          }
        }

      
      temp <- data.frame(temp,row.names=NULL)
      
      colnames(temp) <- c("target.code","oa.codes","pctg.fit")[c(report.code,T,best.fit)]
      
      return(temp)
      }
      
      if(!is.null(higher.geog)|!is.null(higher.codes)|!is.null(higher.names)){
        for(i in 1:nrow(ordered)){
          if(as.character(ordered[i,1])==as.character(level)){
            col <- as.character(ordered[i,2])
            col_code <- as.character(ordered[i,3])
            
            for(j in 1:nrow(table)){
              
              if(grepl(paste(paste(wild,name,sep=""),collapse="|"),as.character(table[j,col]),ignore.case = T)){
                temp<-rbind(temp,data.frame(                        as.character(table[j,2]),
                                                                    as.character(table[j,3]),
                                                                    as.character(table[j,4]),
                                                                    as.character(table[j,5]),
                                                                    as.character(table[j,6]),
                                                                    as.character(table[j,7]),
                                                                    as.character(table[j,8]),
                                                                    as.character(table[j,9]),
                                                                    as.character(table[j,10]),
                                                                    as.character(table[j,11])))
              }
            }
            
            
            break
          }
          if(i==nrow(ordered)){
            message("Error: Please input the right level.")
            return("Quit")
          }
        }
        
        
        
        country.code =F
        country.name=F
        region.code=F
        region.name=F
        county.code=F
        county.name=F
        district.code=F
        district.name=F
        ward.code=F
        ward.name=F
        
        number = 9
        
        if (level=="country"){
          country.code =T
          country.name=T
          number = 1
        }
        
        if (level=="region"){
          region.code =T
          region.name=T
          number = 3
        }
        
        if (level=="county"){
          county.code =T
          county.name=T
          number = 5
        }
        
        if (level=="district"){
          district.code =T
          district.name=T
          number = 7
        }
        
        if (level=="ward"){
          ward.code =T
          ward.name=T
          number = 9
        }
        
        temp <- temp[!duplicated(temp[[number]]),]
        
        if(!is.null(higher.geog)&is.null(higher.names)&is.null(higher.codes)){
          if (higher.geog[1]=="ALL"){
            country.code =T
            country.name=T
            region.code=T
            region.name=T
            county.code=T
            county.name=T
            district.code=T
            district.name=T
            ward.code=T
            ward.name=T
          }else{
            for (y in 1:length(higher.geog)){
              if (higher.geog[y]=="country"){
                country.code =T
                country.name=T
              }
              
              if (higher.geog[y]=="region"){
                region.code =T
                region.name=T
              }
              
              if (higher.geog[y]=="county"){
                county.code =T
                county.name=T
              }
              
              if (higher.geog[y]=="district"){
                district.code =T
                district.name=T
              }
              
              if (higher.geog[y]=="ward"){
                ward.code =T
                ward.name=T
              }
            }
          }
        }
        
        if (!is.null(higher.names)){
          if (higher.names[1]=="ALL"){
            country.name=T
            
            region.name=T
            
            county.name=T
            
            district.name=T
            
            ward.name=T
          }else{
            for (y in 1:length(higher.names)){
              if (higher.names[y]=="country"){
                country.name=T
              }
              
              if (higher.names[y]=="region"){
                region.name=T
              }
              
              if (higher.names[y]=="county"){
                county.name=T
              }
              
              if (higher.names[y]=="district"){
                district.name=T
              }
              
              if (higher.names[y]=="ward"){
                ward.name=T
              }
            }
          }


        }
        
        if(!is.null(higher.codes)){
          if (higher.codes[1]=="ALL"){
            country.code=T
            
            region.code=T
            
            county.code=T
            
            district.code=T
            
            ward.code=T
          }else{
            for (y in 1:length(higher.codes)){
              if (higher.codes[y]=="country"){
                country.code=T
              }
              
              if (higher.codes[y]=="region"){
                region.code=T
              }
              
              if (higher.codes[y]=="county"){
                county.code=T
              }
              
              if (higher.codes[y]=="district"){
                district.code=T
              }
              
              if (higher.codes[y]=="ward"){
                ward.code=T
              }
            }
          }
        }
        
        temp <- temp[c(country.code,country.name,region.code,region.name,county.code,county.name,district.code,district.name,ward.code,ward.name)]
        
        colnames(temp) <- c("country.code","country.name","region.code","region.name","county.code","county.name","district.code","district.name","ward.code","ward.name")[c(country.code,country.name,region.code,region.name,county.code,county.name,district.code,district.name,ward.code,ward.name)]
        
        return(data.frame(temp,row.names=NULL))
      }
      
      for (x in 1:length(level)){
        for(i in 1:nrow(ordered)){
          if(as.character(ordered[i,1])==as.character(level[x])){
            col <- as.character(ordered[i,2])
            col_code <- as.character(ordered[i,3])
            
            for(j in 1:nrow(table)){
              
              if(grepl(paste(paste(wild,name,sep=""),collapse="|"),as.character(table[j,col]),ignore.case = T)){
                temp<-rbind(temp,data.frame(as.character(ordered[i,1]),as.character(table[j,col_code]),as.character(table[j,col])))
              }
            }
            
            
            break
          }
          if(i==nrow(ordered)){
            message("Error: Please input the right level.")
            return("Quit")
          }
        }
      }
      
      temp <- temp[!duplicated(temp[3]),]
      temp <- temp[c(report.level,report.code,report.name)]
      if (length(level)==1){
        colnames(temp) <- c("level",paste(level,".code",sep=""),paste(level,".name",sep=""))[c(report.level,report.code,report.name)]
      }else{
        colnames(temp) <- c("level","code","name")[c(report.level,report.code,report.name)]
      }
      
    }
    
    return(data.frame(temp,row.names=NULL))
  }
  
  if (!is.null(level)){
    # (b)
    if (level=="oa"){
      temp <- data.frame(table[[1]])
      temp <- data.frame(rep(NA,nrow(temp)),temp)
      
      colnames(temp) <- c("level","oa.code")
      return(temp)
    }
    
    for(i in 1:nrow(ordered)){
      if(as.character(ordered[i,1])==as.character(level)){
        col <- as.character(ordered[i,2])
        col_code <- as.character(ordered[i,3])
        break
      }
      if(i==nrow(ordered)){
        message("Error: Please input the right level.")
        return("Quit")
      }
    }

    temp <- data.frame(table[[col_code]],table[[col]])
    temp <- temp[!duplicated(temp[1]),]
    temp <- temp[""!=sub("(^ +)|( +$)","",temp[[1]]),]
    temp <- data.frame(rep(level,nrow(temp)),temp)
    
    temp <- temp[c(report.level,report.code,report.name)]
    colnames(temp) <- c("level",paste(level,".code",sep=""),paste(level,".name",sep=""))[c(report.level,report.code,report.name)]
    
    return(data.frame(temp,row.names=NULL))
  }
  
  # (a)
  temp <- data.frame(                as.character(table[[2]]),
                                     as.character(table[[3]]),
                                     as.character(table[[4]]),
                                     as.character(table[[5]]),
                                     as.character(table[[6]]),
                                     as.character(table[[7]]),
                                     as.character(table[[8]]),
                                     as.character(table[[9]]),
                                     as.character(table[[10]]),
                                     as.character(table[[11]]),
                                     as.character(table[[1]]))
  colnames(temp) <- c("country.code","country.name","region.code","region.name","county.code","county.name","district.code","district.name","ward.code","ward.name","oa.code")
  return(temp)
  
},
error=function(e) {
  # error report
  message("Error: Please input the right codes.")
  message("\t\t\t\t\t\t\tWe only accept legal codes.")
})
}