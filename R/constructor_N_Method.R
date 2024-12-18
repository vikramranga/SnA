# define class of the dataset
# I am going to implement this in S3 class


# This is proto-type and I will keep on adding things to it
as.sna <- function(tab, sVar = "s", aVar = "a"){
  require(tidyverse)
  if(!is.data.frame(tab) & !is_tibble(tab)) stop('Please provide the dataset in data frame or tibble format')
  tab <- rename(tab, S = sVar, A = aVar)
  class(tab) <- "sna"
  return(tab)
}


#Define the generics: print and summary

print.sna <- function(sna){
  cat("Summary of S: ", "\n")
  print(summary(sna$S))
  cat("Summary of A: ", "\n")
  print(summary(sna$A))
}



#Example
a <- data.frame(s = runif(10), a = runif(10), b = runif(10))
a1 <- as.sna(tab = a)
