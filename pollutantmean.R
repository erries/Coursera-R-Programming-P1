pollutantmean <- function(directory,pollutant, id=1:332){
  ## 'directory is a chracter vector of lenth 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate"
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  ##source("pollutantmean.R")
  ##pollutantmean("specdata", "sulfate", 1:10)
  #### [1] 4.064
  ##pollutantmean("specdata", "nitrate", 70:72)
  #### [1] 1.706
  ##pollutantmean("specdata", "nitrate", 23)
  #### [1] 1.281
  mydata<- NULL
  for(i in id)
  {
    index<-i
    index<-toString(index)
    
    if(nchar(index)==1){
      index<-paste("00",index,sep="")
    }
    if(nchar(index)==2){
      index<-paste("0",index,sep="")
    }
    
    filename <- paste(directory,"/",index,".csv",sep="")
    
    #print(filename)
    
    colnames <- c("Date","sulfate","nitrate","id")
    
    mydata2 <- read.table(filename, sep=",",col.names=colnames,skip=1)
    
    #mydata2 <- read.table("specdata/001.csv",sep=",",col.names=colnames,skip=1)
    
    mydata <- rbind(mydata,mydata2)
  }
  mydata$Date <- as.Date(mydata$Date,format="%Y-%m-%d")
  
  
  
  ## Print the first value from the first column
  #tmp<-mydata[1]
  #tmp2<-tmp[,1]
  #tmp2[1]

  #mydata$pollutant
  pollutantvector<- mydata[,pollutant]
  mean(pollutantvector,na.rm=TRUE)
  
}