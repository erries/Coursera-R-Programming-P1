corr <- function(directory, threshold = 0){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observered observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result
  
  ##source("corr.R")
  ##source("complete.R")
  ##cr <- corr("specdata", 150)
  ##head(cr)
  #### [1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.07589
  ##summary(cr)
  ####    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #### -0.2110 -0.0500  0.0946  0.1250  0.2680  0.7630
  ##cr <- corr("specdata", 400)
  ##head(cr)
  #### [1] -0.01896 -0.04390 -0.06816 -0.07589  0.76313 -0.15783
  ##summary(cr)
  ####    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #### -0.1760 -0.0311  0.1000  0.1400  0.2680  0.7630
  ##cr <- corr("specdata", 5000)
  ##summary(cr)
  ####    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #### 
  ##length(cr)
  #### [1] 0
  ##cr <- corr("specdata")
  ##summary(cr)
  ####    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #### -1.0000 -0.0528  0.1070  0.1370  0.2780  1.0000
  ##length(cr)
  #### [1] 323 
  
  
  mydata<- NULL
  output<- NULL
  outputcorr<- NULL
  #print(nrow(mydata))
  id<-1:332
  
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
    if (counter != 0) {
      outputcorr <- rbind(outputcorr,c(i,counter,cor(cbind(mydata$sulfate,mydata$nitrate),use="complete.obs")))
    }
    
  }
  
  colnames(output) <- c("id","nobs")
  output<-as.data.frame(output)
  outputcorr<-as.data.frame(outputcorr)
  colnames(outputcorr) <-c("id","nobs","trash1","correlation","dup","trash2")

  
  #print(outputcorr)
  
  outputcorr<-subset(outputcorr, select=c("id","nobs","correlation"))
  
  #print(outputcorr)
  
  outputcorr<-subset(outputcorr, outputcorr$nobs > threshold)
 
  outputcorr
  
  outputcorr$correlation 
  #outputcorr<-subset(outputcorr,)
 
   
}