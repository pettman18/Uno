library(ReinforcementLearning)


control <- list(alpha = 0.1, gamma = 0.15, epsilon = 0.05)

model <- ReinforcementLearning(data = long_term_memory, 
                               s = "state", 
                               a = "turn_action", 
                               r = "points", 
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
