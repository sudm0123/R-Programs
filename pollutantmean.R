pollutantmean<-function(directory,pollutant,id=1:332){
	directory<-"/media/sudhanshu_malik/Kabaada Drive !!/Users/Sudhanshu/Desktop/Training/Working Directory/specdata"
	setwd(directory)   
	files<-dir()
	data<-data.frame()
     	for(i in id)
       	{
        	   data<-rbind(data,read.csv(files[i]))
	}
     	mean<-mean(data[,pollutant],na.rm=TRUE)
  	setwd("/media/sudhanshu_malik/Kabaada Drive !!/Users/Sudhanshu/Desktop/Training/Working Directory")
  	mean
}