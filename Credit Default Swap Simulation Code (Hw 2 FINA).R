library(tidyverse)
library(mcsm)
#Start off by defining variables
#For Z, I have decided to generate 100000 random normally distributed numbers. This will go into our factor model
rho_1 <- .1
rho_2 <- .2
rho_3 <- .3
rho_4 <- .4
rho_5 <- .5
Z <- rnorm(100000)
C1 <- -1.8
C2 <- -1.45

#For e1 and e2 we assign rv with 0 mean and unit variance
e1 <- rnorm(100000)
e2 <- rnorm(100000)

#QUESTION 1
#x1_1 through x1_5 are the X1 values as rho changes from .1 to .5
#This equations takes our 100000 simulations into account Through Z and e1. 
#Therefore we have 100000 values each of X1_1 through x1_5
x1_1 <- rho_1*Z + (sqrt(1-rho_1^2)*e1)
x1_2 <- rho_2*Z + (sqrt(1-rho_2^2)*e1)
x1_3 <- rho_3*Z + (sqrt(1-rho_3^2)*e1)
x1_4 <- rho_4*Z + (sqrt(1-rho_4^2)*e1)
x1_5 <- rho_5*Z + (sqrt(1-rho_5^2)*e1)


# To figure out the Probability of default, we count how many times X1<C1 for each value of rho
# After we count how many defaults, we divide by the total to get probability

P1_def1 <- (length(x1_1[ x1_1 <= C1 ]))/100000
P1_def2 <- (length(x1_2[ x1_2 <= C1 ]))/100000
P1_def3 <- (length(x1_3[ x1_3 <= C1 ]))/100000
P1_def4 <- (length(x1_4[ x1_4 <= C1 ]))/100000
P1_def5 <- (length(x1_5[ x1_5 <= C1 ]))/100000

#Standard error calculation
se1_1 <- 1/sqrt(100000)*sd(x1_1)
se1_2 <- 1/sqrt(100000)*sd(x1_2)
se1_3 <- 1/sqrt(100000)*sd(x1_3)
se1_4 <- 1/sqrt(100000)*sd(x1_4)
se1_5 <- 1/sqrt(100000)*sd(x1_5)

#QUESTION 2
#In order to figure out the probability of both defaulting,
#We need to run a simulation for obligor 2
x2_1 <- rho_1*Z + (sqrt(1-rho_1^2)*e2)
x2_2 <- rho_2*Z + (sqrt(1-rho_2^2)*e2)
x2_3 <- rho_3*Z + (sqrt(1-rho_3^2)*e2)
x2_4 <- rho_4*Z + (sqrt(1-rho_4^2)*e2)
x2_5 <- rho_5*Z + (sqrt(1-rho_5^2)*e2)


#Lets duplicate x1_1 through x1_5
#The goal is to create a matrix with 0s for no default and 1s for default

dup_x1_1 <- x1_1
dup_x1_2 <- x1_2
dup_x1_3 <- x1_3
dup_x1_4 <- x1_4
dup_x1_5 <- x1_5

#In the next few lines we assign 0s and 1s
dup_x1_1[ dup_x1_1 > C1 ] <- 0
dup_x1_2[ dup_x1_2 > C1 ] <- 0
dup_x1_3[ dup_x1_3 > C1 ] <- 0
dup_x1_4[ dup_x1_4 > C1 ] <- 0
dup_x1_5[ dup_x1_5 > C1 ] <- 0

dup_x1_1[ dup_x1_1 <= C1 ] <- 1
dup_x1_2[ dup_x1_2 <= C1 ] <- 1
dup_x1_3[ dup_x1_3 <= C1 ] <- 1
dup_x1_4[ dup_x1_4 <= C1 ] <- 1
dup_x1_5[ dup_x1_5 <= C1 ] <- 1


#Same for X2
dup_x2_1 <- x2_1
dup_x2_2 <- x2_2
dup_x2_3 <- x2_3
dup_x2_4 <- x2_4
dup_x2_5 <- x2_5

dup_x2_1[ dup_x2_1 > C2 ] <- 0
dup_x2_2[ dup_x2_2 > C2 ] <- 0
dup_x2_3[ dup_x2_3 > C2 ] <- 0
dup_x2_4[ dup_x2_4 > C2 ] <- 0
dup_x2_5[ dup_x2_5 > C2 ] <- 0

dup_x2_1[ dup_x2_1 <= C2 ] <- 1
dup_x2_2[ dup_x2_2 <= C2 ] <- 1
dup_x2_3[ dup_x2_3 <= C2 ] <- 1
dup_x2_4[ dup_x2_4 <= C2 ] <- 1
dup_x2_5[ dup_x2_5 <= C2 ] <- 1



#Add both matrices...call it "comb#" 
#For both to default, the sum of the matrices entries should give us 2, so we can count all the instances of this happening
#using the length function
comb1 <- dup_x1_1 + dup_x2_1 
comb2 <- dup_x1_2 + dup_x2_2
comb3 <- dup_x1_3 + dup_x2_3
comb4 <- dup_x1_4 + dup_x2_4
comb5 <- dup_x1_5 + dup_x2_5

#Here are our probabilities:
p_both_def1 <- length(comb1[ comb1 == 2 ])/100000
p_both_def2 <- length(comb2[ comb2 == 2 ])/100000
p_both_def3 <-length(comb3[ comb3 == 2 ])/100000
p_both_def4 <-length(comb4[ comb4 == 2 ])/100000
p_both_def5 <-length(comb5[ comb5 == 2 ])/100000

#Standard error
1/sqrt(100000)*sd(comb1)
1/sqrt(100000)*sd(comb2)
1/sqrt(100000)*sd(comb3)
1/sqrt(100000)*sd(comb4)
1/sqrt(100000)*sd(comb5)

#Question 3 
#In order to find the probability of either defaulting, we modify the last portion of code from question 2.
#In order to get the probability of either defaulting, we account for the 1s and 2s in the matrix. 0 means neither defaults
P_either_def1 <- length(comb1[ comb1 >= 1 ])/100000
P_either_def2 <- length(comb2[ comb2 >= 1 ])/100000
P_either_def3 <- length(comb3[ comb3 >= 1 ])/100000
P_either_def4 <- length(comb4[ comb4 >= 1 ])/100000
P_either_def5 <- length(comb5[ comb5 >= 1 ])/100000



#Question 1
#Probab
P1_def1
P1_def2
P1_def3
P1_def4
P1_def5

#Standard Error
se1_1
se1_2
se1_3
se1_4
se1_5

#Question 2
p_both_def1 
p_both_def2
p_both_def3
p_both_def4
p_both_def5 

#Question 3
P_either_def1 
P_either_def2 
P_either_def3 
P_either_def4
P_either_def5 

#Question 4
asset1 <- rbinom(100000,10,.04)
