# Given some integer, find the maximal number you can obtain by deleting exactly one digit of the given number.
# 
# Example
# 
# For n = 152, the output should be
# deleteDigit(n) = 52;
# For n = 1001, the output should be
# deleteDigit(n) = 101.
# 
# Input/Output
# 
# [execution time limit] 5 seconds (r)
# 
# [input] integer n
# 
# Guaranteed constraints:
#   10 ≤ n ≤ 106.
# 
# [output] integer
n = 44435
deleteDigit <- function(n) {
  n = as.numeric(strsplit(as.character(n),"")[[1]])
  alldigits <- lapply(seq(1:length(n)),function(x) {
    return(as.numeric(paste(n[-x],collapse = "")))
  })
  
  return(max(unlist(alldigits)))
}

