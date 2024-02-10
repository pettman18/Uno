library(ReinforcementLearning)
library(dplyr)

gc()
setwd("~/GitHub/Uno")

setwd("archive")

# setwd("D:/archive")


training_files <- list.files(,".csv")
# 
# training_files <- list.files(,"longtermdata.csv")

long_term_memory<- read.csv(training_files[1])
# 
# v<-2
# # while(v < length(training_files)){
# while(v < 10000){
#         
#         long_term_memory <- rbind(read.csv(training_files[v]),long_term_memory)
#         
#         
#         v<- v+1
# }

print(long_term_memory)

long_term_memory <- sapply(long_term_memory,as.character)
long_term_memory <- data.frame(long_term_memory)
long_term_memory$data.points <- as.numeric(long_term_memory$data.points)
long_term_memory$data.turn_action <- as.character(long_term_memory$data.turn_action)
long_term_memory <- subset(long_term_memory, select = -c(state_win,data.state_win) )
long_term_memory$new_state <-as.character(paste0(long_term_memory$state_colour,long_term_memory$state_number,long_term_memory$state_wild, long_term_memory$hand_state))
long_term_memory$state <-as.character(paste0(long_term_memory$data.state_colour,long_term_memory$data.state_number,long_term_memory$data.state_wild, long_term_memory$data.hand_state))
long_term_memory <- long_term_memory %>% filter(,is.na(long_term_memory$data.points)==FALSE)
# 
# 
long_term_memory %>% select(data.state_colour, data.state_number ,data.state_wild ,data.hand_state, data.turn_action,state)

# long_term_memory <- long_term_memory[ -(long_term_memory$data.state_wild==FALSE & long_term_memory$data.turn_action==3  ) ,  ]
# long_term_memory <- long_term_memory[ -(long_term_memory$data.state_number==FALSE & long_term_memory$data.turn_action==2  ) ,  ]
# long_term_memory <- long_term_memory[ -(long_term_memory$data.state_colour==FALSE & long_term_memory$data.turn_action==1  ) ,  ]


setwd("~/GitHub/Uno")
save(long_term_memory,file =  "longtermdata.Rdata")


control <- list(alpha = 0.5, gamma = 0.5, epsilon = 0.1)

model <- ReinforcementLearning(data = long_term_memory, 
                               s = "state", 
                               a = "data.turn_action", 
                               r = "data.points", 
                               s_new = "new_state",
                               iter = 1, 
                               control = control
                               )

summary(model)


# model_old <- model
# new_model <-  model
str(long_term_memory)

 # model<- model_new
save(model,file =  "model.Rdata")
