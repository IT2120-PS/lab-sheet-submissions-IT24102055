getwd()
setwd("C:\\Users\\Lenovo\\Desktop\\Lab06")
getwd()


n <- 50
p <- 0.85

prob_geq_47 <- 1 - pbinom(46, size = 50, prob = 0.85)
prob_geq_47

prob_15_calls <- dpois(15, lambda = 12)
prob_15_calls
