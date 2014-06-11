rankall <- function(outcome, num="best") {
	
	setwd("/media/sudhanshu_malik/Kabaada Drive !!/Users/Sudhanshu/Desktop/Training/Working Directory/Data/2-Assignment_3")
	read_data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
	data<-read_data[order(read_data$State),c(2,7,11,17,23)]
	state_dataframe<-as.data.frame(table(data[2]))
	state_frequency<-state_dataframe[,2]		
	state_name<-names(table(data[2]))
	final_result<-data.frame()
	
	if(outcome=="heart attack"){column<-3}
	else if(outcome=="heart failure"){column<-4}
	else if(outcome=="pneumonia"){column<-5}
	else{
		stop("invalid outcome")
	}
	temp<-as.numeric(data[,column])
	temp_logic<-is.na(temp)
	z<-1
	num1<-num
	for(i in seq_len(length(state_name))){##length(state_name)
		rank_rate<-vector("numeric")
		rank_name<-vector("character")
		for(j in seq_len(state_frequency[i])){##state_frequency[i]
			if(!temp_logic[z]){
				rank_rate<-append(rank_rate,temp[z])
				rank_name<-append(rank_name,data[z,1])
			}
			z<-z+1		
		}
		if(length(rank_rate)==0){
			rank_rate<-NA
			rank_name<-NA
		}
		rank_rate<-as.numeric(rank_rate)
		rank_z<-cbind(rank_rate,rank_name)
		rank<-rank_z[order(rank_rate,rank_name),]
		if(num=="best"){
			num<-1
		}
		else if(num1=="worst"){
			rank<-as.data.frame(rank)
			num<-nrow(rank)
		}
		if(num>length(rank_rate)){
			result<-NA
		}
		else{
			if(length(rank_rate)==1){
				result<-as.vector(rank["rank_name"])
			}
			else{
				
				result<-as.vector(rank[num,"rank_name"])
			}	
		}
		final_result[i,1]<-result
		final_result[i,2]<-state_name[i]
	}
	colnames(final_result)<-c("hospital","state")
	final_result
}