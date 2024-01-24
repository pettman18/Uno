# uno_setup

# setup
set.seed(123)

deck <-full_deck()
shuffled_deck <- shuffle(deck)
players <- 2

list <- deal(2,shuffled_deck)


all_player_cards <- as.data.frame(list[1:2])
active_deck <- list[3:length(list)]
active_deck <- as.data.frame(do.call(rbind,active_deck))
colnames(active_deck) <-"cards"

active_player <- 1
next_player <- 2
direction <- 1

active_card <- draw_cards(active_deck, 1)[[1]]
active_deck <- draw_cards(active_deck, 1)[[2]]

