# Data binder

setwd("archive")

training_files <- list.files(,".csv")



v<-1

x<-1
while(x<75){
  
long_term_memory<- read.csv(training_files[v])
  
while(v < (x*5000)){
  
  long_term_memory <- rbind(read.csv(training_files[v]),long_term_memory)
  
  
  v<- v+1
}


setwd("~/GitHub/Uno")
write.csv(long_term_memory,paste0(x,"longtermdata.csv",sep=""))
setwd("archive")

x<-x+1
}



