library(ReinforcementLearning)

library(dplyr)
setwd("~/GitHub/Uno")

setwd("archive")

v<- 1
training_files <- list.files(,".csv")
short_term_memory <- read.csv(training_files[v])


# training_files <- list.files(,"longtermdata.csv")
short_term_memory_size <- nrow(short_term_memory)

# find average number of rows per game replace 5 
short_term_memory_start <- short_term_memory_size - (20 * runs )


short_term_memory<- tail(short_term_memory,short_term_memory_start )


print(short_term_memory)

short_term_memory <- sapply(short_term_memory,as.character)
short_term_memory <- data.frame(short_term_memory)
short_term_memory$data.points <- as.numeric(short_term_memory$data.points)
short_term_memory$data.turn_action <- as.character(short_term_memory$data.turn_action)
short_term_memory <- subset(short_term_memory, select = -c(state_win,data.state_win) )
short_term_memory$new_state <-as.character(paste0(short_term_memory$state_colour,short_term_memory$state_number,short_term_memory$state_wild, short_term_memory$hand_state))
short_term_memory$state <-as.character(paste0(short_term_memory$data.state_colour,short_term_memory$data.state_number,short_term_memory$data.state_wild, short_term_memory$data.hand_state))
short_term_memory <- short_term_memory %>% filter(,is.na(short_term_memory$data.points)==FALSE)
# 
# 
short_term_memory %>% select(data.state_colour, data.state_number ,data.state_wild ,data.hand_state, data.turn_action,state)

# short_term_memory <- short_term_memory[ -(short_term_memory$data.state_wild==FALSE & short_term_memory$data.turn_action==3  ) ,  ]
# short_term_memory <- short_term_memory[ -(short_term_memory$data.state_number==FALSE & short_term_memory$data.turn_action==2  ) ,  ]
# short_term_memory <- short_term_memory[ -(short_term_memory$data.state_colour==FALSE & short_term_memory$data.turn_action==1  ) ,  ]


setwd("~/GitHub/Uno")
save(short_term_memory,file =  "shorttermdata.Rdata")





control <- list(alpha = 0.5, gamma = 0.45, epsilon = 0.05)

model <- ReinforcementLearning(data = short_term_memory, 
                               s = "state", 
                               a = "data.turn_action", 
                               r = "data.points", 
                               s_new = "new_state",
                               iter =2, 
                               control = control,
                               model = model)

summary(model)


# model_old <- model
# # new_model <-  model
# str(short_term_memory)
# 
# # model<- model_new
# save(model,file =  "model.Rdata")
