rankhospital <- function(state, outcome, num = "best") {
	setwd("/media/sudhanshu_malik/Kabaada Drive !!/Users/Sudhanshu/Desktop/Training/Working Directory/Data/2-Assignment_3")
	data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
	
	if(outcome=="heart attack"){column<-11}
	else if(outcome=="heart failure"){column<-17}
	else if(outcome=="pneumonia"){column<-23}
	else{
		stop("invalid outcome")
	}
	temp<-as.numeric(data[,column])
	temp_logic<-is.na(temp)
	state_test<-0
	rank_rate<-vector("character")
	rank_name<-vector("numeric")
	for(i in seq_len(nrow(data))){
		if(state==data[i,"State"]){
			if(!temp_logic[i]){
				rank_rate<-append(rank_rate,temp[i])
				rank_name<-append(rank_name,data[i,2])
				}	
			state_test<-1
		}
	}
	if(state_test==0){
		stop("invalid state")
	}
	else{
		rank_rate<-as.numeric(rank_rate)
		rank_z<-cbind(rank_rate,rank_name)
		rank<-rank_z[order(rank_rate,rank_name),]
		if(num=="best")num<-1
		else if(num=="worst")num<-nrow(rank)
		if(num>nrow(rank)){
			result<-NA
		}
		else{
			result<-as.vector(rank[num,"rank_name"])
		}
	}
	result
}