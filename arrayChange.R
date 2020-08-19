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

#come back to this later.
arrayChange <- function(inputArray) {
    inputArray <- unlist(inputArray)
    ArrayDiff <- diff(inputArray)

}

# arrayChange <- function(inputArray) {
#   inputArray <- unlist(inputArray)
#   ArrayDiff <- diff(inputArray)
#   inputArrayModified <- inputArray
#   totalStepsRequired <- 0
#   if (sum(ArrayDiff <= 0) == 0) {
#     return(0)
#   }
#   #switching for for while to check for better speed
#   initialEstimateOfLoopItn <- length(ArrayDiff) - min(which(ArrayDiff <= 0)) + 1;
#  for (ind in seq(1:initialEstimateOfLoopItn)) {
#    if (sum(ArrayDiff <= 0) > 0) {
#      stepsTomakeitpos <- 1 - ArrayDiff[min(which(ArrayDiff <= 0))]
#      #replace that and now do the same for remaining
#      inputArrayModified[min(which(ArrayDiff <= 0)) + 1] <- inputArrayModified[min(which(ArrayDiff <= 0)) + 1] + stepsTomakeitpos
#      ArrayDiff <- diff(inputArrayModified)
#      # print(ArrayDiff)
#      totalStepsRequired <- totalStepsRequired + stepsTomakeitpos 
#    }
#  } 
# 
#   return(totalStepsRequired)
# }
