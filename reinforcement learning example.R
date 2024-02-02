library(ReinforcementLearning)


control <- list(alpha = 0.5, gamma = 0.3, epsilon = 0.1)

model <- ReinforcementLearning(data = long_term_memory, 
                               s = "state", 
                               a = "turn_action", 
                               r = "points", 
                               s_new = "new_state",
                               iter = 10, 
                               control = control
                               )

summary(model)


# model_old <- model
# new_model <-  model
