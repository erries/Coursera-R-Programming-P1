complete <- function(directory, id = 1:332){
  ## 'directory is a chracter vector of lenth 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## return a data frame of the form
  ## id nobs
  ## 1  117
  ## 2 1041
  ## ...
  ## where 'id' is the montior ID number and 'nobs' is the number of complete cases
  
  ##source("complete.R")
  ##complete("specdata", 1)
  ####   id nobs
  #### 1  1  117
  ##complete("specdata", c(2, 4, 8, 10, 12))
  ####   id nobs
  #### 1  2 1041
  #### 2  4  474
  #### 3  8  192
  #### 4 10  148
  #### 5 12   96
  ##complete("specdata", 30:25)
  ####   id nobs
  #### 1 30  932
  #### 2 29  711
  #### 3 28  475
  #### 4 27  338
  #### 5 26  586
  #### 6 25  463
  ##complete("specdata", 3)
  ####   id nobs
  #### 1  3  243
  
  mydata<- NULL
  output<- NULL
  outputcorr<- NULL
  #print(nrow(mydata))
  
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
    
    mydata <- read.table(filename, sep=",",col.names=colnames,skip=1)
    mydata$Date <- as.Date(mydata$Date,format="%Y-%m-%d")
    #mydata2 <- read.table("specdata/001.csv",sep=",",col.names=colnames,skip=1)
    
    counter<-0
    list<-1:nrow(mydata)
    for(y in list) {
      
      mydata_date<-mydata$Date[y]
      mydata_sulfate<-mydata$sulfate[y]
      mydata_nitrate<-mydata$nitrate[y]
      mydata_id<-mydata$id[y]
      
      if(mydata_id==i){
        if(!is.na(mydata_date) & !is.na(mydata_sulfate) & !is.na(mydata_nitrate)){
          counter <- counter +1
        }
      }
    }
    output<- rbind(output,c(i,counter))
    
  }
    
    colnames(output) <- c("id","nobs")
    output<-as.data.frame(output)
    
    output
    
    
    

}