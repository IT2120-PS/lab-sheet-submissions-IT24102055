setwd("C:\\Users\\DELL\\Desktop\\IT24102055")
getwd()

#Question 1
punif(25,min = 0,max = 40,lower.tail = TRUE)
punif(10,min = 0,max = 40,lower.tail = TRUE)

#Question 2
pexp(2,rate = 1/3,lower.tail = TRUE)

#Question 3.i
pnorm(130, mean = 100, sd = 15, lower.tail = FALSE)

# Question 3.ii
qnorm(0.95, mean = 100, sd = 15, lower.tail = TRUE)