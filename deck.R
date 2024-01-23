set.seed(123)
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


green <- deckmaker("green")
blue <- deckmaker("blue")
red <- deckmaker("red")
yellow <- deckmaker("yellow")

deck <- c(green,red,blue,yellow )
length(deck)


shuffle <- function(deck){
  
  
  shuffled_deck <- sample(deck)  
  
  
  return(shuffled_deck)

}


shuffled_deck <- shuffle(deck)
players <- 2

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
  
  length(shuffled_deck)
  
  shuffled_deck <- shuffled_deck[cards_req +1:(length(shuffled_deck)-cards_req)]
  
  # print(all_player_cards)
  
  return(c(all_player_cards, shuffled_deck))
  
}

list <- deal(5,shuffled_deck)


all_player_cards <- as.data.frame(list[1:2])
active_deck <- list[3:length(list)]
active_deck <- as.data.frame(do.call(rbind,active_deck))
colnames(active_deck) <-"cards"




  
draw_cards <- function(active_deck,cards_req){
  active_card <- as.data.frame( active_deck[c(1:cards_req),1])
  active_deck <- as.data.frame(active_deck[c(cards_req+1:nrow(active_deck)-nrow(active_card)),1])
  
  return(c(active_card,active_deck))
  
}

draw_cards(active_deck, 2)
draw_cards(active_deck, 3)
draw_cards(active_deck, 23)


active_player <- 1
next_player <- 2
direction <- 1

active_card <- draw_cards(active_deck, 1)[1]
active_deck <- draw_cards(active_deck, 1)[2]

active_deck <- unlist(active_deck)
active_deck <- data.frame(active_deck)


# assign cards to a player
player_cards <- all_player_cards[1,2]
player_cards <- as.data.frame(fread(gsub(",", "\n", player_cards, perl=TRUE), col.names = c("cards")))
# player_cards <- as.data.frame(player_cards)
colnames( player_cards) <- "cards"



find_playable_cards <- function(player_cards, active_card){
  
  
  
  # player_cards <- as.data.frame(fread(gsub(",", "\n", player_cards, perl=TRUE), col.names = c("cards")))
  
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


options <- find_playable_cards(player_cards, active_card)


play_card<- function(options,action){
  
  outcome<- if(action=="N"){2}else{""}
  outcome<- if(action=="C"){1}else{outcome}
  outcome<- if(action=="P"){"pick up"}else{outcome}
  outcome<- if(action=="W"){3}else{outcome}
  
  choice <- unlist(options[outcome])[1]

  return(choice)
}


active_card <- as.character(play_card(options,"C")[1])
# player_cards <-c(player_cards, unlist(play_card(options,"N")[2]))
# player_cards <- as.vector(unlist(player_cards))

# player_cards <- as.data.frame(fread(gsub(",", "\n", player_cards, perl=TRUE), col.names = c("cards")))

player_cards <- player_cards[-match(active_card, player_cards$cards),]
player_cards <- as.data.frame(player_cards)
colnames(player_cards) <- "cards"





options <- find_playable_cards(player_cards, active_card)
active_card <- as.character(play_card(options,"C")[1])

player_cards <- player_cards[-match(active_card, player_cards$cards),]
player_cards <- as.data.frame(player_cards)
colnames(player_cards) <- "cards"