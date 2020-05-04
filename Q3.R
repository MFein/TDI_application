library(tidyverse)

perm <- function(v) {
  n <- length(v)
  if (n == 1) v
  else {
    X <- NULL
    for (i in 1:n) X <- rbind(X, cbind(v[i], perm(v[-i])))
    X
  }
}

x <- c(1:10)

z <-as.data.frame(perm(x))
dim(z)
#dimensions are 3628800 x 10

z <- z %>% 
  mutate(payout = z[,1] + abs(z[,2]-z[,1]) + abs(z[,3]-z[,2]) + abs(z[,4]-z[,3]) + abs(z[,5]-z[,4]) + abs(z[,6]-z[,5]) + abs(z[,7]-z[,6]) +
        abs(z[,8]-z[,7]) + abs(z[,9]-z[,8]) + abs(z[,10]-z[,9])   
  )
mu <- mean(z$payout)
stdev <- sd(z$payout)
z

y <- c(1:20)

zz <- as.data.frame(perm(y))
dim(zz)
#dimensions are 

zz <- zz %>% 
  mutate(payout = z[,1] + abs(z[,2]-z[,1]) + abs(z[,3]-z[,2]) + abs(z[,4]-z[,3]) + abs(z[,5]-z[,4]) + abs(z[,6]-z[,5]) + abs(z[,7]-z[,6]) +
           abs(z[,8]-z[,7]) + abs(z[,9]-z[,8]) + abs(z[,10]-z[,9]) + abs(z[,11]-z[,10]) + abs(z[,12]-z[,11]) + abs(z[,13]-z[,12]) +
           abs(z[,14]-z[,3]) + abs(z[,15]-z[,14]) + abs(z[,16]-z[,15]) + abs(z[,17]-z[,16]) + abs(z[,18]-z[,17]) + abs(z[,19]-z[,18]) +
           abs(z[,20]-z[,19])    
  )