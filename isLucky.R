# Ticket numbers usually consist of an even number of digits. A ticket number is considered lucky if the sum of the first half of the digits is equal to the sum of the second half.
# 
# Given a ticket number n, determine if it's lucky or not.
# 
# Example
# 
#     For n = 1230, the output should be
#     isLucky(n) = true;
#     For n = 239017, the output should be
#     isLucky(n) = false.
# 
# Input/Output
# 
#     [execution time limit] 5 seconds (r)
# 
#     [input] integer n
# 
#     A ticket number represented as a positive integer with an even number of digits.
# 
#     Guaranteed constraints:
#     10 ≤ n < 106.
# 
#     [output] boolean
#         true if n is a lucky ticket number, false otherwise.

isLucky <- function(n) {
 n <-  as.character(n)
 n <- as.list(unlist(strsplit(n,"")))
 firstHalfofn <- n[c(seq(from = 1, to = length(n)/2))]  
 secondHalfofn <- n[c(seq(from = (length(n)/2) + 1, to = length(n)))]  
 if (sum(as.numeric(firstHalfofn)) == sum(as.numeric(secondHalfofn))) {
   return(TRUE)
 } else {
   return(FALSE)
 }
}

