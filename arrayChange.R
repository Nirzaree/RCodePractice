# You are given an array of integers. On each move you are allowed to increase exactly one of its element by one. Find the minimal number of moves required to obtain a strictly increasing sequence from the input.
# 
# Example
# 
# For inputArray = [1, 1, 1], the output should be
# arrayChange(inputArray) = 3.
# 
# Input/Output
# 
# [execution time limit] 5 seconds (r)
# 
# [input] array.integer inputArray
# 
# Guaranteed constraints:
#   3 ≤ inputArray.length ≤ 105,
# -105 ≤ inputArray[i] ≤ 105.
# 
# [output] integer
# The minimal number of moves needed to obtain a strictly increasing sequence from inputArray.
# It's guaranteed that for the given test cases the answer always fits signed 32-bit integer type.
# inputArray <- list(1,1,1)
# inputArray <- list(-1000, 0, -2, 0)
# inputArray <- list(2, 1, 10, 1)

#this solution was still not fast enough for 1 test input. :\
arrayChange <- function(inputArray) {
    inputArray <- unlist(inputArray)
    ArrayDiff <- diff(inputArray)
    inputArrayModified <- inputArray
    if (sum(ArrayDiff > 0) == length(ArrayDiff)) {
      return(0)
    } else {
      #do it in 1 iteration
      #first first dec diff
      sumsteps = 0;
      for (ind in seq(from = min(which(ArrayDiff <= 0)), to = length(ArrayDiff))) {
        ArrayDiff <- diff(inputArray)
        if (sum(ArrayDiff > 0) == length(ArrayDiff)) {
          return(sumsteps)
        }
        if (ArrayDiff[ind] <= 0) {
          stepsTomakeitPos <- (1 - (ArrayDiff[ind]))
          sumsteps <- sumsteps + stepsTomakeitPos
          inputArray[ind + 1] <- inputArray[ind + 1] + stepsTomakeitPos
        }
      }
    }
  return(sumsteps)
}

#someone else's solution: pretty elegant
# arrayChange <- function(inputArray) {
#   inputArray <- unlist(inputArray)
#   initial <- inputArray
#   for (ind in 2:length(inputArray)) {
#     inputArray[ind] <- max(inputArray[ind-1] + 1,inputArray[ind])
#   }
#   return(sum(inputArray - initial))
# }