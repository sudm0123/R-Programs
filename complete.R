complete<-function(directory,id=1:332){
  directory<-"/media/sudhanshu_malik/Kabaada Drive !!/Users/Sudhanshu/Desktop/Training/Working Directory/specdata"
  setwd(directory)
  files<-dir()
  data<-data.frame()
  for(i in id)
  {
    temp<-read.csv(files[i])
    good<-complete.cases(temp)
    temp1<-temp[good,]
    x<-c(i,nrow(temp1))
    data<-rbind(data,x)
  }
  setwd("/media/sudhanshu_malik/Kabaada Drive !!/Users/Sudhanshu/Desktop/Training/Working Directory")
  names(data)<-c("id","nobs")
  data
}