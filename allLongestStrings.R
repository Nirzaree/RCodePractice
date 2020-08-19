# # Given an array of strings, return another array containing all of its longest strings.
# 
# Example
# 
# For inputArray = ["aba", "aa", "ad", "vcd", "aba"], the output should be
# allLongestStrings(inputArray) = ["aba", "vcd", "aba"].
# 
# Input/Output
# 
# [execution time limit] 5 seconds (r)
# 
# [input] array.string inputArray
# 
# A non-empty array.
# 
# Guaranteed constraints:
#   1 ≤ inputArray.length ≤ 10,
# 1 ≤ inputArray[i].length ≤ 10.
# 
# [output] array.string
# Array of the longest strings, stored in the same order as in the inputArray.

# inputArray = list("aba", "aa", "ad", "vcd", "aba")
allLongestStrings <- function(inputArray) {
  getLengthofAllInputs <- lapply(inputArray,nchar)
  
  return(inputArray[c(which(unlist(getLengthofAllInputs) == max(unlist(getLengthofAllInputs))))])
}

