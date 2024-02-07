library(ReinforcementLearning)


control <- list(alpha = 0.6, gamma = 0.15, epsilon = 0.1)

model_new <- ReinforcementLearning(data = long_term_memory, 
                               s = "state", 
                               a = "turn_action", 
                               r = "points", 
                               s_new = "new_state",
                               iter = 1, 
                               control = control
                               )

summary(model_new)


# model_old <- model
# new_model <-  model
str(long_term_memory)


save(model,file =  "model.Rdata")

long_term_memory <- long_term_memory[ -(long_term_memory$data.state_wild==FALSE & long_term_memory$turn_action==3  ) ,  ]
long_term_memory <- long_term_memory[ -(long_term_memory$data.state_number==FALSE & long_term_memory$turn_action==2  ) ,  ]
long_term_memory <- long_term_memory[ -(long_term_memory$data.state_colour==FALSE & long_term_memory$turn_action==1  ) ,  ]
