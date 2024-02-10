setwd("~/GitHub/Uno")
# set.seed(123)
library(dplyr)
library(data.table)
# Load necessary library
library(tibble)
library(readr)


mround <- function(x,base){
  base*round(x/base)
}



deckmaker <- function(colour){
  initial <- substr(colour,1,1)
  colour <-c(paste0( initial,seq(1,9,1),sep=""))
  colour <- c(paste0(initial,"P"),paste0(initial,"S"),paste0(initial,"R"),colour)
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
  deck <- c(deck,deck)
  # deck <- c(deck,deck,deck,deck,deck,deck,deck,deck,deck,deck,deck,deck,deck,deck,deck) 
  length(deck)
  return(deck)
}


shuffle <- function(deck){
  shuffled_deck <- sample(deck) 
  
  return(shuffled_deck)
}


# Recursive function to check if an element or any of its nested elements is non-empty
is_non_empty_recursive <- function(x) {
  if (is.list(x)) {
    # If the element is a list, apply the function to each of its elements
    return(any(sapply(x, is_non_empty_recursive)))
  } else {
    # If the element is not a list, check if it's non-empty
    return(length(x) > 0)
  }
}



deal<- function(players,shuffled_deck){
  
  cards_req <- players * 7
  
  # cards_req <- players * 30
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

# draw_cards(active_deck, 1) 

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
  
  wild_cards <-sort( wild_cards,decreasing = TRUE)
  
  
  
  playable_cards <- list(colour_matches,number_matches,wild_cards)
  
  return(playable_cards)
}



play_card<- function(options,action,active_card){
  
  outcome<- if(action=="C"){1}else{""}
  outcome<- if(action=="N"){2}else{outcome}
  outcome<- if(action=="W"){3}else{outcome}
  outcome<- if(action=="D"){"pick up"}else{outcome}
  
  choice <- unlist(options[outcome])[1]
  choice <-if(length(outcome)==0){active_card}else{choice}
  
  return(choice)
}


# Function to check if an element is empty
is_empty <- function(x) {
  return(length(unlist(x)) == 0)
}

player <- 1

auto_play <- function(options, active_player, model, hand_state="") {
  # Initialize a dataframe to store the results
  result_df <- data.frame(Element = integer(0), Status = character(0), stringsAsFactors = FALSE)
  i <-1
  # Checking each of the first three elements
  for (i in 1:3) {
    status <- if (is_empty(options[[i]])) "FALSE" else "TRUE"
    result_df <- rbind(result_df, data.frame(Element = i, Status = status))
    
    
  }
  
  
  
  if(active_player==1){
    # print("ai")
  model_lookup <- paste0(result_df$Status,collapse = "")
  model_lookup <- paste0(model_lookup,hand_state,sep="")
  model_example <- model[2]
  model_example <-data.frame(model_example)
  colnames(model_example) <- c("1","2","3","4")
  model_example <- subset(model_example,row.names(model_example) %in% model_lookup )
  
  # if we dont have a modeled case then we need to chose a random valid value
  if(nrow(model_example)==0){    # print("random")
    result_df <- filter(result_df,result_df$Status=="TRUE")
    choice <- as.character(sample_n(result_df,1))[1]
    # print(choice)
    }else{
  # Find the maximum value in the dataframe
  eligible_ai_options <- result_df[result_df$Status==TRUE,][1]
  model_example <- model_example %>% select(unlist(eligible_ai_options$Element))
  
  max_value <- max(model_example, na.rm = TRUE)
  
  choice <-colnames(model_example)[apply(model_example,1,which.max)]
  
  # print(choice)
    }
  }else{
    # print("random")
  result_df <- filter(result_df,result_df$Status=="TRUE")
  choice <- as.character(sample_n(result_df,1))[1]
  # print(choice)
  
  }
  choice<- gsub("1","C",choice)
  choice<- gsub("2","N",choice)
  choice<- gsub("3","W",choice)
  
  return(choice)
}





player_turn <- function(player_cards, active_card,active_deck,draw_amount,w_colour=NULL,active_player,model){
  win <- FALSE
  cards_start_turn <- nrow(player_cards)
  options <- find_playable_cards(player_cards, active_card)
  
  w_pref <- names(which.max(table(substr(player_cards$cards,1,1))))
  
  # Adding extra states here
  
  active_colour<- substr(active_card,1,1)
  n_y <- min(sum(substr(player_cards$cards,1,1)=="y"),5)
  n_g <- min(sum(substr(player_cards$cards,1,1)=="g"),5)
  n_b <- min(sum(substr(player_cards$cards,1,1)=="b"),5)
  n_r <- min(sum(substr(player_cards$cards,1,1)=="r"),5)
  n_w <- min(sum(substr(player_cards$cards,1,1)=="W"),5)
  
  point_template <- data.frame(c("b","g","r","y","W"),c(0,0,0,0,0))
  colnames(point_template) <- c("colour","points")
  
  hand_colour_points <- left_join(point_template, find_hand_points(player_cards), by ="colour")
  hand_colour_points$points.x <- hand_colour_points$points.x +hand_colour_points$points.y
  hand_colour_points[is.na(hand_colour_points)] <- 0
  colnames(hand_colour_points) <- c("colour","points","reject")
  hand_colour_points <- hand_colour_points[,1:2]
  
  hand_colour_points$points <- mround(hand_colour_points$points,5)
  
  ready_hand_colour<-paste0(hand_colour_points$colour,hand_colour_points$points,sep="", collapse ="")
    
  hand_state <- paste(active_colour,n_b,n_g,n_r,n_y, n_w,ready_hand_colour, sep = "")
  
  
  # if the player only has one choice, lets make it.  
  # 
  # if (any(sapply(options, is_non_empty_recursive))) {
  #   action <-user_input()  
  # } else {
  #   action <- "D"
  # }
  
  
  
  # if the options are empty we must set action as D
  # 
  
  if(draw_amount==0){
    
    
    # Apply the function to each top-level element of the list and check if any are true
    if (any(sapply(options, is_non_empty_recursive))) {
      
      
      action <- auto_play(options, active_player, model,hand_state)
      
      
      # action <-user_input()  
    } else {
      action <- "D"
    }
    
    
    
    if(action=="D"){
      new_card <- as.data.frame(draw_cards(active_deck, 1)[[1]])
      colnames(new_card)<-"cards"
      # print(paste0("picked up a new cards: ",new_card))
      active_deck <- draw_cards(active_deck, 1)[[2]]
      player_cards <-rbind.data.frame(player_cards,new_card)
      # print(paste0("here are my cards ",player_cards))
      options <- find_playable_cards(player_cards, active_card)
      
      
      # Apply the function to each top-level element of the list and check if any are true
      if (any(sapply(options, is_non_empty_recursive))) {
        action <- auto_play(options, active_player, model,hand_state)
        
        # action <-user_picked_up()
      } else {
        action <- "S"
      }
      
      
    }
    
    
    # if we skip then active card remains the same, otherwise play the card
    if(action=="S"){active_card <- active_card}else{
      
      
      
      # I dont think we've set the action
      
      
      
      active_card <- as.character(play_card(options,action,active_card)[1])
    }
  }else{
    # Draw amount >0 stuff lives here
    # if(substr(active_card,2,2)=="P" && length(options[[2]]) > 0 && options[[2]] != ""){
    if(substr(active_card,2,2)=="P" && length(options[[2]]) > 0 ){
      # if the card is Plus two and we have plus 2 then we play.
      action <-"N"
      active_card <- as.character(play_card(options,action,active_card)[1])
      
    }else{
      # Draw some cards as punishment 
      new_card <- as.data.frame(draw_cards(active_deck, draw_amount)[[1]])
      colnames(new_card)<-"cards"
      # print(paste0("picked up a new cards: ",new_card))
      active_deck <- draw_cards(active_deck, draw_amount)[[2]]
      player_cards <-rbind.data.frame(player_cards,new_card)
      # print("all my new cards are now here")
      # print(player_cards)
      # make sure we dont play a card now
      action <- "S"
    }
    
  }
  
  
  
  
  
  # if we skip then active card remains the same, otherwise play the card
  if(action=="S"|action=="D"){player_cards <- player_cards}else{
    if(nrow(player_cards)>1){
      player_cards <- player_cards[-which(player_cards$cards==active_card)[1],]
      player_cards <- as.data.frame(player_cards)
      colnames(player_cards) <- "cards"
    }else{
      # print("Win")
      win <- TRUE
    }
  }
  
  
  cards_end_turn <- nrow(player_cards)
  
  card_played <- cards_end_turn <= cards_start_turn
  
  
  
  
  if(card_played==FALSE){
    
    # print("no card played")
    
    
  }else{
    
    
    # print(active_card)
    
    # if w4 then add 4 to pickup, colour behavior handled below
    if(active_card=="W4"){
      draw_amount<-4
      
    }else{draw_amount<-draw_amount}
    
    
    if(substr(active_card,1,1)=="W"){
      
      # w_colour <-pick_colour()
      w_colour <- w_pref
      
    }else{w_colour <-NULL}
    
    
    if(substr(active_card,2,2)=="R"){
      direction<- direction*-1
      # print(direction)
    }else{direction}
    
    if(substr(active_card,2,2)=="P"){
      draw_amount <- draw_amount +2
      # print(draw_amount)
    }else{draw_amount <-draw_amount}
    skip_state <- if(substr(active_card,2,2)=="S"){
      TRUE
    }else{FALSE}
    
    
  }
  
  # adding hand state to the output
  options[[4]]<- hand_state
  
  
  active_card<-if(length(w_colour)!=0){w_colour}else{active_card}
  
  output <- list(active_card,player_cards,active_deck,card_played,draw_amount,direction,skip_state,w_colour,options,action,win)
  
  return(output)
  
}





user_input<- function(){
  action <- readline("Enter: C for the same colour, N for same number, W for Wild, D for Draw   ")
  action <- toupper(action)
  
  return(action)
}





user_picked_up<- function(){
  action <- readline("Enter: C for the same colour, N for same number, W for Wild, S for skip   ")
  action <- toupper(action)
  
  return(action)
}






pick_colour<- function(){
  w_colour <- readline("Pick Colour: R, B, Y, G   ")
  w_colour <- tolower(w_colour)
  
  return(w_colour)
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



# find_next_player(3,6,-1,F)

# uno_setup

# setup
# set.seed(126)

deck <-full_deck()
shuffled_deck <- shuffle(deck)
players <- 3

list <- deal(players,shuffled_deck)


all_player_cards <- as.data.frame(list[1:2])
active_deck <- list[3:length(list)] 
active_deck <- as.data.frame(do.call(rbind,active_deck))
colnames(active_deck) <-"cards"

active_player <- 1
next_player <- 2
direction <- 1

active_card <- draw_cards(active_deck, 1)[[1]]
active_deck <- draw_cards(active_deck, 1)[[2]]

draw_amount <- 0
direction <- 1
skip_state <- FALSE
w_colour <- NULL


game_players<-0


game_players <-load_player(active_player,all_player_cards)


find_hand_points <- function(player_cards){
  
  card_points <-player_cards
  
  card_points$cards <- gsub("R","25",card_points$cards)
  card_points$cards <- gsub("S","25",card_points$cards)
  card_points$cards <- gsub("P","25",card_points$cards)
  
  card_points$cards <- gsub("W4","50",card_points$cards)
  card_points$cards <- gsub("w","50",card_points$cards)
  
  card_points$colour <-substr(card_points$cards,1,1)
  card_points$points<-as.numeric(substr(card_points$cards,2,3))
  
  
  card_points_agg <-aggregate(card_points$points, by=list(colour=card_points$colour), FUN=sum)
  
  colnames(card_points_agg) <- c( "colour","points")
  
  return(card_points_agg)
  
}



find_points <- function(game_cards=game_cards,player){
  
card_points <- gsub("b","",game_cards$cards)
card_points <- gsub("y","",card_points)
card_points <- gsub("g","",card_points)
card_points <- gsub("r","",card_points)

card_points <- gsub("R","25",card_points)
card_points <- gsub("S","25",card_points)
card_points <- gsub("P2","25",card_points)
card_points <- gsub("P","25",card_points)

card_points <- gsub("W4","50",card_points)
card_points <- gsub("w","50",card_points)

game_points <-cbind.data.frame(game_cards,"card_points" = as.numeric(card_points)) 


game_points <-aggregate(game_points$card_points, by=list(player=game_points$player), FUN=sum)

game_points <- data.frame(game_points)
colnames(game_points)[2]<- "points"

game_points <- game_points[ game_points$player == player,]

return(game_points)

}


player <- 1




ai_turn_learn <- function(turn_action, turn_options, player_cards,active_player,x,feedback) {
  
  turn_action <- gsub("C",1,turn_action)
  turn_action <- gsub("N",2,turn_action)
  turn_action <- gsub("W",3,turn_action)
  turn_action <- gsub("S",4,turn_action)
  
  state_colour <- is_non_empty_recursive(turn_options[[1]])
  state_number <- is_non_empty_recursive(turn_options[[2]])
  state_wild <- is_non_empty_recursive(turn_options[[3]])
  hand_state <- turn_options[[4]]
  state_win <-length(player_cards)<1
  
  training_data <- tibble(
    active_player,
    x,
    state_colour,
    state_number,
    state_wild,
    state_win,
    hand_state,
    turn_action,
    feedback
  )
  
  # Write the data frame to a CSV file
  write.csv(training_data, "data.csv", row.names = FALSE)
  
  return(training_data)
  
}
