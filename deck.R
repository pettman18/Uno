
deckmaker <- function(colour){
  initial <- substr(colour,1,1)
  colour <-c(paste0( initial,seq(1,9,1),sep=""))
  colour <- c(paste0(initial,"P2"),paste0(initial,"S"),paste0(initial,"R"),colour)
  colour <- rep(colour , each =2)
  colour <- c(paste0(initial,0),colour)
  colour <- c("W","P4",colour)
  
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


deal<- function(players,shuffled_deck){
  
  cards_req <- players * 7 
  shuffled_deck[1:cards_req]
  
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
  
  
  print(all_player_cards)
  
}

deal(5,shuffled_deck)
