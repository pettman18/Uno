
# 
# get the state of the game
# 
# get the state of the game, action, reward, next state, done ( game over)


train_long_memory

train_short_memory

remember <- function( to write a state_old, action, reward,state_new,done) into a csv


get_action ( we should be able to use options and play card)


max_memory <- 100000
batch_size <- 1000
LR <- 0.001

n_games <- 0
epsilon <- 0
gamma <- 0 # discount rate
# memory <- 
# model
# trainer


plot_scores <- 0
plot_mean_scores<- 0
total_score <- 0
record <- 0



while n_game < total_games

 get the state of the game
 
 get the move played
 
 perform the move
 
(reward,done,score)

get the new state

# train short memory
train_short_memory(state_old, final_move,reward, state_new)

remember(state_old, final_move,reward, state_new)

if the game is over, lets traing the long memory
n_games <- n_games + 1
train_long_memory(state_old, final_move,reward, state_new)

print(n_game)
print(winner)


