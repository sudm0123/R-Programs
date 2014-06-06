corr<-function(directory,threshold=0){
  directory<-"/media/sudhanshu_malik/Kabaada Drive !!/Users/Sudhanshu/Desktop/Training/Working Directory/specdata"
  setwd(directory)
  files<-dir()
  result<-vector("numeric")
  id=1:332
  for(i in id)
  {
    temp<-read.csv(files[i])
    good<-complete.cases(temp)
    temp1<-temp[good,]
    if(nrow(temp1)>threshold){
      x<-temp1[,2]
      y<-temp1[,3]
      result<-append(result,cor(x,y))     
    }
  }
  setwd("/media/sudhanshu_malik/Kabaada Drive !!/Users/Sudhanshu/Desktop/Training/Working Directory")
  result
}