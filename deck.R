

start <- Sys.time()
# training loop

training_loop <- 1

while (training_loop < 2){

source("uno_setup.R")

# if(training_loop%%100==0){ 
#   print("training")
# source("agent.R")
# source("model trainer.R")
# }
  
players <- 3

player1_cards <-load_player(1,all_player_cards)
player2_cards <-load_player(2,all_player_cards)
player3_cards <-load_player(3,all_player_cards)



player1_cards <- cbind(player=1,player1_cards)
player2_cards <- cbind(player=2,player2_cards)
player3_cards <- cbind(player=3,player3_cards)


game_cards <- rbind.data.frame(player1_cards,player2_cards,player3_cards)



# game loop

x<-1
while(x< 500){



skip_state <- FALSE
start_points<-find_points(game_cards,as.numeric(active_player))

player_cards <- filter(game_cards,game_cards$player==active_player)[2]
game_cards <- filter(game_cards,game_cards$player!=active_player)


# print(active_card)
# print(player_cards)

                
turn_cards <- player_turn(player_cards,active_card,active_deck,draw_amount,NULL,active_player,model )

active_card <- turn_cards[[1]]
player_cards <- turn_cards[[2]]
active_deck <- turn_cards[[3]]
card_played <- turn_cards[[4]]

# updates required



# if card change is true then update skip state from previous turn
if(card_played==TRUE){
draw_amount <- turn_cards[[5]]
direction <- turn_cards[[6]]
skip_state <- turn_cards[[7]]
w_colour <- turn_cards[[8]]
  

}else{
  
  draw_amount <- 0
  skip_state <- FALSE
  direction <- direction
  draw_amount <- 0
  
  
}


game_cards<- rbind(cbind(player=active_player,player_cards),game_cards)

end_points <- find_points(game_cards,active_player)



turn_options <- turn_cards[[9]]
turn_action <- turn_cards[[10]]
win <- turn_cards[[11]]


feedback <- start_points[2]-end_points[2]
# print(feedback)



if(win==TRUE){
  
  x<-10000000
  feedback <- feedback + 1000
  # print(feedback)
  # print(paste0("winner is ", active_player))
  
  # Writing to a CSV file
  write.table(
    x = data.frame(winner = active_player),
    file = "winners.csv",
    append = TRUE,
    col.names = !file.exists("winners.csv"), # Add column name only if file doesn't exist
    row.names = FALSE,
    sep = ",",
    quote = TRUE
  )
  # Sys.sleep(3)
}



if(active_player==1 & x>players){

data <- as.data.frame(read_csv("data.csv",show_col_types = FALSE))  

training_data<-ai_turn_learn(turn_action, turn_options, player_cards,active_player, x,feedback) 
training_data <-  as.data.frame(training_data)


training_data <- cbind(data$state_number,data$state_colour,data$state_wild,data$state_win,data$hand_state,training_data)

filename <- paste0(as.numeric(Sys.time()),training_loop,x,"traineddata.csv")
setwd("archive")
write.csv(training_data, filename, row.names = FALSE)
setwd("~/GitHub/Uno")
}



active_player <- find_next_player(active_player,players,1,skip_state)

x<-x+1
}

print(training_loop)

training_loop <- training_loop + 1
}

end<- Sys.time()

time_taken <- end- start
