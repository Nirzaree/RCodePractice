# Given a string, find out if its characters can be rearranged to form a palindrome.

# Example
# 
# For inputString = "aabb", the output should be
# palindromeRearranging(inputString) = true.
# 
# We can rearrange "aabb" to make "abba", which is a palindrome.
# 
# Input/Output
# 
# [execution time limit] 5 seconds (r)
# 
# [input] string inputString
# 
# A string consisting of lowercase English letters.
# 
# Guaranteed constraints:
#   1 ≤ inputString.length ≤ 50.
# 
# [output] boolean
# true if the characters of the inputString can be rearranged to form a palindrome, false otherwise.
# inputString = "aabb"

# what is th requirement for a palindrome :
#get freq. plot of char from string 

library(data.table)
palindromeRearranging <- function(inputString) {
  #get freq table for string
  initial <- inputString
  inputString <- strsplit(initial,"")[[1]]
  freqChar <- unlist(lapply(unique(inputString),function(x) {return(sum(inputString == x))}))
  if (sum(freqChar %% 2) > 1) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#happy about this solution: quick, easy. :) 
