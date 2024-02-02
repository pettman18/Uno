library("synthpop")


my_data <- data.frame(model[2])
 

mysyn <- syn(my_data)

summary(mysyn)

compare(mysyn, my_data, stat = "counts")
