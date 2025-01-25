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

#Replacement to convert matrix to 1s and 0s
comb1[ comb1 < 2 ] <- 0
comb2[ comb2 < 2 ] <- 0
comb3[ comb3 < 2 ] <- 0
comb4[ comb4 < 2 ] <- 0
comb5[ comb5 < 2 ] <- 0

comb1[ comb1 == 2 ] <- 1
comb2[ comb2 == 2 ] <- 1
comb3[ comb3 == 2 ] <- 1
comb4[ comb4 == 2 ] <- 1
comb5[ comb5 == 2 ] <- 1

#Here are our probabilities:
p_both_def1 <- length(comb1[ comb1 == 1 ])/100000
p_both_def2 <- length(comb2[ comb2 == 1 ])/100000
p_both_def3 <- length(comb3[ comb3 == 1 ])/100000
p_both_def4 <- length(comb4[ comb4 == 1 ])/100000
p_both_def5 <- length(comb5[ comb5 == 1 ])/100000

#Standard error
se_both1 <- 1/sqrt(100000)*sd(comb1)
se_both2 <- 1/sqrt(100000)*sd(comb2)
se_both3 <- 1/sqrt(100000)*sd(comb3)
se_both4 <- 1/sqrt(100000)*sd(comb4)
se_both5 <- 1/sqrt(100000)*sd(comb5)

#Question 3 
#Start off by creating new combination matrix for either defaulting
comb3_1 <- dup_x1_1 + dup_x2_1 
comb3_2 <- dup_x1_2 + dup_x2_2
comb3_3 <- dup_x1_3 + dup_x2_3
comb3_4 <- dup_x1_4 + dup_x2_4
comb3_5 <- dup_x1_5 + dup_x2_5

#Replacement to convert matrix to 1s and 0s...note: (we want either defaulting)
comb3_1[ comb3_1 < 1 ] <- 0
comb3_2[ comb3_2 < 1 ] <- 0
comb3_3[ comb3_3 < 1 ] <- 0
comb3_4[ comb3_4 < 1 ] <- 0
comb3_5[ comb3_5 < 1 ] <- 0

comb3_1[ comb3_1 >= 1 ] <- 1
comb3_2[ comb3_2 >= 1 ] <- 1
comb3_3[ comb3_3 >= 1 ] <- 1
comb3_4[ comb3_4 >= 1 ] <- 1
comb3_5[ comb3_5 >= 1 ] <- 1

#Here are our probabilities:
#In order to find the probability of either defaulting, we modify the last portion of code from question 2.
#In order to get the probability of either defaulting, we account for the 1s and 2s in the matrix. 0 means neither defaults

p_either_def1 <- length(comb3_1[ comb3_1 == 1 ])/100000
p_either_def2 <- length(comb3_2[ comb3_2 == 1 ])/100000
p_either_def3 <- length(comb3_3[ comb3_3 == 1 ])/100000
p_either_def4 <- length(comb3_4[ comb3_4 == 1 ])/100000
p_either_def5 <- length(comb3_5[ comb3_5 == 1 ])/100000

#Standard Errors
se_either1 <- 1/sqrt(100000)*sd(comb3_1)
se_either2 <- 1/sqrt(100000)*sd(comb3_2)
se_either3 <- 1/sqrt(100000)*sd(comb3_3)
se_either4 <- 1/sqrt(100000)*sd(comb3_4)
se_either5 <- 1/sqrt(100000)*sd(comb3_5)

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

#Standard error
se_both1 
se_both2 
se_both3
se_both4
se_both5

#Question 3
p_either_def1 
p_either_def2 
p_either_def3 
p_either_def4
p_either_def5 

#Standard error
se_either1 
se_either2 
se_either3 
se_either4 
se_either5 

#Question 4

no_def <- rbinom(10,0,.04)
count <- 0
for (val in no_def) {
  if(val == 1)  count = count+1
  repeat()
 
}
print(count)


two_def <- rbinom(10,2,.04)
iterations <- rep(two_def,100000)
count2 <- 0
for (rep in two_def) {
  if(sum(two_def) == 1)  count2 = count2+1
}
print(count2)






n_sim <- 5
p_def <- rbinom(10,2,.04)
result <- rep(p_def,n_sim)
count <- 0
for (i in 1:n_sim) {
  if(sum(p_def) == 2) count = count+1 
  result[i] <- i
}
result
count


n_sim <- 10
p_def <- rbinom(10,2,.04)
result <- replicate(n_sim,p_def)
count <- 0
for (i in 1:n_sim) {
  if(sum(p_def) == 2) count = count+1 
  result[i] <- i
}
result
count

rbinom(10,2,.04)
rep(rbinom(10,2,.04),4)

repeat {
  rbinom(10,2,.04)
  
  ()
replicate(5,rbinom(10,2,.04))

rbinom(50000, 10, 0.3)

hist(replicate(10,runif(10)))