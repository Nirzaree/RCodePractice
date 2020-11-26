# Given array of integers, find the maximal possible sum of some of its k consecutive elements.
# 
# Example
# 
# For inputArray = [2, 3, 5, 1, 6] and k = 2, the output should be
# arrayMaxConsecutiveSum(inputArray, k) = 8.
# All possible sums of 2 consecutive elements are:
#   
#   2 + 3 = 5;
# 3 + 5 = 8;
# 5 + 1 = 6;
# 1 + 6 = 7.
# Thus, the answer is 8.
# 
# Input/Output
# 
# [execution time limit] 5 seconds (r)
# 
# [input] array.integer inputArray
# 
# Array of positive integers.
# 
# Guaranteed constraints:
#   3 ≤ inputArray.length ≤ 105,
# 1 ≤ inputArray[i] ≤ 1000.
# 
# [input] integer k
# 
# An integer (not greater than the length of inputArray).
# 
# Guaranteed constraints:
#   1 ≤ k ≤ inputArray.length.
# 
# [output] integer
# 
# The maximal possible sum.

inputArray = list(2, 3, 5, 1, 6)
k = 2

arrayMaxConsecutiveSum <- function(inputArray, k) {
  inputArray <- unlist(inputArray)
  maxsubarraysum = 0
  previtnsum = 0;
  for (ind in seq(1:(length(inputArray) - k + 1))) {
    if (ind > 1) {
      currentsum = previtnsum - inputArray[ind - 1] + inputArray[ind + k - 1]
    } else {
      currentsum = sum(inputArray[ind:(ind + k - 1)])
    }

    if (currentsum > maxsubarraysum) {
      maxsubarraysum = currentsum
    }
    previtnsum = currentsum
  }
  return(maxsubarraysum)
}

#failing in last test
# arrayMaxConsecutiveSum <- function(inputArray, k) {
#   inputArray <- unlist(inputArray)
#   maxsubarraysum = 0
#   for (ind in seq(1:(length(inputArray) - k + 1))) {
#     currentsum = sum(inputArray[ind:(ind + k - 1)])
#     if (currentsum > maxsubarraysum)
#       maxsubarraysum = currentsum
#   }
#   return(maxsubarraysum)
# }

