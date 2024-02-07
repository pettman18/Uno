# graphing the progress
library(dplyr)
library(ggplot2)


graph_it<- function(){
winners <- read_csv("winners.csv",show_col_types = FALSE)
games <- nrow(winners)

winners<- data.frame(winners)
winners$winner <- as.character(winners$winner) 
winners$game <- seq(1,games,1) 

winners<- winners %>%
  group_by(winner) %>%
  mutate(count=row_number())

ggplot(winners,aes(winners$game,winners$count,group= winner,color=winner))+
  geom_line()
}
