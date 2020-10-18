# Given an array of integers, find the maximum possible sum you can get from one of its contiguous subarrays. The subarray from which this sum comes must contain at least 1 element.
# 
# Example
# 
# For inputArray = [-2, 2, 5, -11, 6], the output should be
# arrayMaxConsecutiveSum2(inputArray) = 7.
# 
# The contiguous subarray that gives the maximum possible sum is [2, 5], with a sum of 7.
# 
# Input/Output
# 
# [execution time limit] 5 seconds (r)
# 
# [input] array.integer inputArray
# 
# An array of integers.
# 
# Guaranteed constraints:
#   3 ≤ inputArray.length ≤ 105,
# -1000 ≤ inputArray[i] ≤ 1000.
# 
# [output] integer
# 
# The maximum possible sum of a subarray within inputArray.

#rules
# if all positive, then return(sum(inputArray))
#if any elements negative, then find the subarray with max sum 
#n! subarrays. Find each, do sum and return. 
#Lets try
# inputArray = list(-2, 2, 5, -11, 6)
# inputArray = list(-3, -2, -1, -4)

#kadane's algo: beautiful algo <3
arrayMaxConsecutiveSum2 <- function(inputArray) {
  inputArray <- as.vector(unlist(inputArray))
  
    if (all(inputArray > 0)) {
      return(sum(inputArray))
    }

    if (all(inputArray < 0)) {
      return(max(inputArray))
    }
  
  globalmax = localmax = inputArray[1];
  for (index in 2:length(inputArray)) {
    localmax <- max(inputArray[index],localmax + inputArray[index])
    if (localmax > globalmax) {
      globalmax <- localmax
    }
  }
  return(globalmax)
}

#logic2-3: cumsum function. what a boon. still not fast enough.
# arrayMaxConsecutiveSum2 <- function(inputArray) {
#   inputArray <- as.vector(unlist(inputArray)) 
#   
#   if (all(inputArray > 0)) {
#     return(sum(inputArray))
#   }
#   
#   if (all(inputArray < 0)) {
#     return(max(inputArray))
#   }
#   
#   if (length(which(inputArray == min(inputArray)))) {
#     #data before min
#     subarraysum <- c()
#     # subarraysum = vector(mode = "integer",length = (length(inputArray) * (length(inputArray) + 1))/2)
#     startindex = 1;
#     stopindex = max(which(inputArray == min(inputArray)) - 1,1);
#     
#     for (startindex in seq(from = startindex, to = stopindex, by = 1)) {
#       # inputArray2 = inputArray[startindex:length(inputArray)]
#       subarraysum <- append(subarraysum,cumsum(inputArray[startindex:length(inputArray)]))
#     }
#     #data after min
#     startindex = min(which(inputArray == min(inputArray)) + 1,length(inputArray));
#     stopindex = length(inputArray);
#     
#     for (startindex in seq(from = startindex, to = stopindex, by = 1)) {
#       # inputArray2 = inputArray[startindex:length(inputArray)]
#       subarraysum <- append(subarraysum,cumsum(inputArray[startindex:length(inputArray)]))
#     }
#     return(max(subarraysum))
#   }
#   
#   subarraysum <- c()
#   # subarraysum = vector(mode = "integer",length = (length(inputArray) * (length(inputArray) + 1))/2)
#   for (startindex in seq(1:length(inputArray))) {
#     # inputArray2 = inputArray[startindex:length(inputArray)]
#     subarraysum <- append(subarraysum,max(cumsum(inputArray[startindex:length(inputArray)])))
#   }
#   return(max(subarraysum))
# }

#logic1: worked but failed in hidden test for time
# arrayMaxConsecutiveSum2 <- function(inputArray) {
#  inputArray <- as.vector(unlist(inputArray)) 
#  
#  if (all(inputArray > 0)) {
#    return(sum(inputArray))
#  }
#  
#  subarraysum = vector(mode = "integer",length = (length(inputArray) * (length(inputArray) + 1))/2)
#  index = 1;
# for (ind in seq(1:length(inputArray))) {
#    startindex = ind;
#    # endindex = length(inputArray)
#    for (endindex in seq(from = startindex,to = length(inputArray),by = 1)) {
#      subarraysum[index] = sum(inputArray[startindex:endindex])
#      index <- index + 1
#    }
#  }
#  return(max(subarraysum))
# }

