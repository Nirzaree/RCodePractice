# You are given an array of integers representing coordinates of obstacles situated on a straight line.
# 
# Assume that you are jumping from the point with coordinate 0 to the right. You are allowed only to make jumps of the same length represented by some integer.
# 
# Find the minimal length of the jump enough to avoid all the obstacles.
# 
# Example
# 
# For inputArray = [5, 3, 6, 7, 9], the output should be
# avoidObstacles(inputArray) = 4.
# 
# Check out the image below for better understanding:
#   
#   Input/Output
# 
# [execution time limit] 5 seconds (r)
# 
# [input] array.integer inputArray
# 
# Non-empty array of positive integers.
# 
# Guaranteed constraints:
#   2 ≤ inputArray.length ≤ 1000,
# 1 ≤ inputArray[i] ≤ 1000.
# 
# [output] integer
# The desired length.
# inputArray <- list(1, 4, 10, 6, 2)
avoidObstacles <- function(inputArray) {
  inputArray <- unlist(inputArray)
  inputArray <- inputArray[order(inputArray)]
  for (ind in 2:(max(inputArray) + 1)) {
    if (sum(inputArray %% ind == 0) == 0) {
      return(ind)
    }
  }
}
#to think through: maxindex = max + 1 ? never more? ya makes sense. hmm.