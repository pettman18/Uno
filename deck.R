
library(dplyr)
library(data.table)

deckmaker <- function(colour){
  initial <- substr(colour,1,1)
  colour <-c(paste0( initial,seq(1,9,1),sep=""))
  colour <- c(paste0(initial,"P2"),paste0(initial,"S"),paste0(initial,"R"),colour)
  colour <- rep(colour , each =2)
  colour <- c(paste0(initial,0),colour)
  colour <- c("W","W4",colour)
  
  return(colour)
  
}

full_deck <- function(){
green <- deckmaker("green")
blue <- deckmaker("blue")
red <- deckmaker("red")
yellow <- deckmaker("yellow")

deck <- c(green,red,blue,yellow )
length(deck)
return(deck)
}


shuffle <- function(deck){
  shuffled_deck <- sample(deck) 
  
  return(shuffled_deck)
}




deal<- function(players,shuffled_deck){
  
  cards_req <- players * 7 
  player_cards <- ""
  all_player_cards <-data.frame("player"=character(),
                                "cards"=character(), 
                                stringsAsFactors=FALSE) 
  
  player<- 1
  while(player<= players){
    
  player_cards <-data.frame(player, toString( shuffled_deck[seq(player,cards_req,players)]))
  colnames(player_cards) <- c("player","cards")
  all_player_cards <- rbind.data.frame(player_cards,all_player_cards)
  player <- player +1
  
  }
  
  shuffled_deck <- shuffled_deck[cards_req +1:(length(shuffled_deck)-cards_req)]
  
  return(c(all_player_cards, shuffled_deck))
  
}


draw_cards <- function(active_deck,cards_req){
  colnames(active_deck) <-"cards"
  active_card <- as.data.frame( active_deck$cards[c(1:cards_req)])
  active_deck <-as.data.frame( active_deck$cards[c(cards_req+1:(nrow(active_deck)-cards_req))])
  colnames(active_deck) <-"cards"
  
  output<-""
  output[[1]] <- active_card
  output[[2]] <- active_deck
  
  return(output)
  
}

draw_cards(active_deck, 1) 

find_playable_cards <- function(player_cards, active_card){

  colnames(player_cards) <- "cards"
  
  # create place for potential cards to live
  potential_cards <- vector("list", length = 3)
  
  # matching colour
  active_colour <- substr(active_card,1,1)
  colour_matches <- player_cards[active_colour == substr(player_cards$cards,1,1),]
  colour_matches <- as.vector(colour_matches)
  colour_matches <- list( unlist(colour_matches) %>% sort(,decreasing = TRUE))
  
  
  # matching number
  active_number <- substr(active_card,2,2)
  number_matches <- player_cards[active_number == substr(player_cards$cards,2,2),]
  number_matches <- as.vector(number_matches)
  
  # wild cards
  wild_cards <- player_cards["W" == substr(player_cards$cards,1,1),]
  wild_cards <- as.vector(wild_cards)
  
  
  playable_cards <- list(colour_matches,number_matches,wild_cards)
  
  return(playable_cards)
}



play_card<- function(options,action,active_card){
  
  outcome<- if(action=="N"){2}else{""}
  outcome<- if(action=="C"){1}else{outcome}
  outcome<- if(action=="D"){"pick up"}else{outcome}
  outcome<- if(action=="W"){3}else{outcome}
  
  choice <- unlist(options[outcome])[1]
  choice <-if(length(outcome)==0){active_card}else{choice}

  return(choice)
}



player_turn <- function(player_cards, active_card,action,active_deck){
  
options <- find_playable_cards(player_cards, active_card)


if(action=="D"){
new_card <- as.data.frame(draw_cards(active_deck, 1)[[1]])
colnames(new_card)<-"cards"
print(paste0("picked up a new cards: ",new_card))
active_deck <- draw_cards(active_deck, 1)[[2]]
player_cards <-rbind.data.frame(player_cards,new_card)
print(paste0("here are my cards ",player_cards))
options <- find_playable_cards(player_cards, active_card)
action <- readline("Enter: C for the same colour, N for same number, W for Wild, S for skip  ")
action <- toupper(action)
}

if(action=="S"){active_card <- active_card}else{
  
active_card <- as.character(play_card(options,action,active_card)[1])
}


# colnames(active_card) <- "cards"
if(action=="S"|action=="D"){player_cards <- player_cards}else{

player_cards <- player_cards[-which(player_cards$cards==active_card)[1],]
player_cards <- as.data.frame(player_cards)
colnames(player_cards) <- "cards"
}

output <- list(active_card,player_cards,active_deck)

return(output)

}

user_input<- function(){
action <- readline("Enter: C for the same colour, N for same number, W for Wild, D for Draw   ")
action <- toupper(action)

return(action)
}

load_player<- function(active_player,all_player_cards){
  # assign cards to a player
  player_cards <- all_player_cards[active_player,2]
  player_cards <- as.data.frame(fread(gsub(",", "\n", player_cards, perl=TRUE), col.names = c("cards")))
  # player_cards <- as.data.frame(player_cards)
  colnames( player_cards) <- "cards"
  
  return(player_cards)
}

find_next_player<-function(active_player,players,direction,skip=FALSE){
  
  moves <-if(skip==TRUE){2}else{1}
  next_player <- active_player + (moves*direction)
  next_player <-if(next_player>players){next_player-players}else{next_player}
  next_player <-if(next_player<1){next_player+players}else{next_player}
  return(next_player)
}

find_next_player(1,6,-1,)


source("uno_setup.R")

player_cards <-load_player(1,all_player_cards)

# game loop

x<-1
while(x<3){
  
player_cards <-load_player(active_player,all_player_cards)


print(active_card)
print(player_cards)

action <-user_input()

turn_cards <- player_turn(player_cards,active_card,action,active_deck )

active_card <- turn_cards[[1]]
player_cards <- turn_cards[[2]]
active_deck <- turn_cards[[3]]

active_player <- active_player+1

x<-x+1
}
