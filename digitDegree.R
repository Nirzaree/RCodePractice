# Let's define digit degree of some positive integer as the number of times we need to replace this number with the sum of its digits until we get to a one digit number.
# 
# Given an integer, find its digit degree.
# 
# Example
# 
#     For n = 5, the output should be
#     digitDegree(n) = 0;
#     For n = 100, the output should be
#     digitDegree(n) = 1.
#     1 + 0 + 0 = 1.
#     For n = 91, the output should be
#     digitDegree(n) = 2.
#     9 + 1 = 10 -> 1 + 0 = 1.

digitDegree <- function(n) {
  sumofdigits <- function(digit) {
    digitarray <- strsplit(digit,"")[[1]]
    arraysum <- sum(as.numeric(digitarray))
    return(arraysum)
  }
  if (n <= 9) {
    return(0)
  }
  currentsum = sumofdigits(as.character(n))
  degree <- 1
  while (currentsum > 9) {
    currentsum = sumofdigits(as.character(currentsum))
    degree <- degree + 1
  }
  return(degree)
}
