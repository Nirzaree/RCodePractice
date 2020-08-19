# Given a sequence of integers as an array, determine whether it is possible to obtain a strictly increasing sequence by removing no more than one element from the array.
# 
# Note: sequence a0, a1, ..., an is considered to be a strictly increasing if a0 < a1 < ... < an. Sequence containing only one element is also considered to be strictly increasing.
# 
# Example
# 
# For sequence = [1, 3, 2, 1], the output should be
# almostIncreasingSequence(sequence) = false.
# 
# There is no one element in this array that can be removed in order to get a strictly increasing sequence.
# 
# For sequence = [1, 3, 2], the output should be
# almostIncreasingSequence(sequence) = true.
# 
# You can remove 3 from the array to get the strictly increasing sequence [1, 2]. Alternately, you can remove 2 to get the strictly increasing sequence [1, 3].
# 
# Input/Output
# 
# [execution time limit] 5 seconds (r)
# 
# [input] array.integer sequence
# 
# Guaranteed constraints:
#   2 ≤ sequence.length ≤ 105,
# -105 ≤ sequence[i] ≤ 105.
# 
# [output] boolean
# Return true if it is possible to remove one element from the array in order to get a strictly increasing sequence, otherwise return false.

# sequence = list(1,5,3,4)
# sequence = list(1, 1, 2, 3, 4, 4) 
# sequence = list(1, 2, 3, 4, 5, 3, 5, 6)
# sequence = list(1,1)
almostIncreasingSequence <- function(sequence) {
  sequence <- as.vector(unlist(sequence))
  getEdges <- diff(sequence)
  if (length(getEdges) == 1 && getEdges == 0) {
    return (TRUE)
  } else if (sum(getEdges > 0) == length(getEdges)) {
    return (TRUE)
  } else if (length(getEdges[getEdges <= 0]) > 1) {
    return(FALSE)
  } else if (which(getEdges < 0) == length(getEdges)) {
    return(TRUE)
  } else {
    #try removing the 2 options and see if it is a strictly increasing sequence
    newSequence = sequence
    newSequence[which(getEdges < 0)] <- NA
    newSequence <- newSequence[!is.na(newSequence)]
    getNewEdges <- diff(newSequence)
    if (sum(getNewEdges > 0) == length(getNewEdges)) {
      return (TRUE)
    } else {
      newSequence = sequence
      newSequence[which(getEdges < 0) + 1] <- NA
      newSequence <- newSequence[!is.na(newSequence)]
      getNewEdges <- diff(newSequence)
      if (sum(getNewEdges > 0) == length(getNewEdges)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  } 
}
