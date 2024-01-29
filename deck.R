
source("uno_setup.R")

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
while(x< 12){



skip_state <- FALSE
start_points<-find_points(game_cards,as.numeric(active_player))  

player_cards <- filter(game_cards,game_cards$player==active_player)[2]
game_cards <- filter(game_cards,game_cards$player!=active_player)


print(active_card)
print(player_cards)



turn_cards <- player_turn(player_cards,active_card,active_deck,draw_amount )

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

feedback <- start_points[2]-end_points[2]
print(feedback)

turn_options <- turn_cards[[9]]
turn_action <- turn_cards[[10]]

if(active_player==1& x>players){

data <- as.data.frame(read_csv("data.csv",show_col_types = FALSE))  

training_data<-ai_turn_learn(turn_action, turn_options, player_cards,active_player, x,feedback) 
training_data <-  as.data.frame(training_data)


training_data <- cbind(training_data, data$state_number,data$state_colour,data$state_wild,data$state_win)

filename <- paste0(x,"traineddata.csv")
write.csv(training_data, filename, row.names = FALSE)

}

# Create an empty data frame with specified columns


 
active_player <- find_next_player(active_player,players,1,skip_state)

x<-x+1
}

