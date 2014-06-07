best<-function(state,outcome){
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
        name<-vector("character")
        small<-vector("numeric")
        small<-100
        for(i in seq_len(nrow(data))){
                if(state==data[i,"State"]){
                        if(!temp_logic[i]){
                        	if(small>temp[i]){
                        		small[1]<-temp[i]
                        		name[1]<-data[i,2]
                        	}
                       	}	
                        state_test<-1
                }
        }
        if(state_test==0){
        	stop("invalid state")
        }
        else{
       	for(i in seq_len(nrow(data))){
        		if(state==data[i,"State"]){
        			if(!temp_logic[i]){
        				if(small[1]==temp[i]){
        					small<-append(small,temp[i])
        					name<-append(name,data[i,2])
        				}
        			}
        		}
        	}
        }
        sort(name)
        name[1]
}