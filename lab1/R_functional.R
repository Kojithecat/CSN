##Argimiro@2016,2018
##R Functional, declarative
##Functional perspective: express WHAT (result do you want)
##Procedural: express HOW (to get the wanted result)

##Even numbers from a vector
m<- 25
x<- 1:m
even <-x[x%%2==0]
even
odd <-x[x%%2!=0]


## Checking a number is prime:
isPrime <- function(x){
  div <- 2:ceiling(sqrt(x))
  all(x %% div != 0)
}

##Functionals: sapply
##We can write a function to get list of primes between n < m:
PrimeList <- function(n,m){
  x <- n:m
  x[sapply(x,isPrime)]
}

PrimeList(2,25)

##Same using llply from plyr
library(plyr)
plyPrimeList <- function(n,m){
  x <- n:m 
  x[unlist(llply(x,isPrime))] ##unlist to get atomic vector of values (remove indices of list)
}
##Note: a list is a recursive vector: a list can have lists as elements, 
##hence to convert a list to vector we have to unwind the nested listing by unlist

n=2; m=1e5
system.time(L<- PrimeList(n,m))
system.time(L<- plyPrimeList(n,m))

##Example: Comparing Functional vs Procedural:
##The VERY basic procedural solution:
isPrimeProc <- function(i){
    j=2; Prime=TRUE;
    while (j <= ceiling(sqrt(i)) && Prime) {
      if(i %% j == 0){Prime=F} else j=j+1}
    return(Prime)
}
PrimeListProc <- function(n,m){
  x<-n:m
  y<-NULL
  for(i in x){if(isPrimeProc(i)){y=append(y,i)}}
  return(y)
}

n=2; m=1e5;
system.time(L <- PrimeList(n,m))
system.time(L <- PrimeListProc(n,m))

##Results for n=2, m=1e5: (orientative. depends on machine arquitecture) 
#           PrimeList        PrimeListProc
# user  system elapsed      user  system elapsed
# 0.385   0.063   0.451     0.909   0.036   0.952

##Results for n=2; m=1e6:   
#           PrimeList        PrimeListProc
# user  system  elapsed      user  system elapsed
# 6.811   0.563   7.385    25.037   3.178  28.254 

