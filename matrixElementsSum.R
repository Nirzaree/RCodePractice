# After becoming famous, the CodeBots decided to move into a new building together. Each of the rooms has a different cost, and some of them are free, but there's a rumour that all the free rooms are haunted! Since the CodeBots are quite superstitious, they refuse to stay in any of the free rooms, or any of the rooms below any of the free rooms.
# 
# Given matrix, a rectangular matrix of integers, where each value represents the cost of the room, your task is to return the total sum of all rooms that are suitable for the CodeBots (ie: add up all the values that don't appear below a 0).

# For
# 
# matrix = [[0, 1, 1, 2], 
#           [0, 5, 0, 0], 
#           [2, 0, 3, 3]]
# 
# the output should be
# matrixElementsSum(matrix) = 9.
# matrix = (list(list(0,1,1,2),list(0,5,0,0),list(2,0,3,3)))

# [[1,1,1,0], 
# [0,5,0,1], 
# [2,1,3,10]]
# matrix = (list(list(1,1,1,0),list(0,5,0,1),list(2,1,3,10)))
matrixElementsSum <- function(matrix) {
  matrix <- rbindlist(matrix)
  AllRelevantNumbers <- sapply(matrix,function(x) {
    if  (sum(x > 0) == length(x)) {
      return(x)
    } else if (min(which(x == 0)) > 1) {
      x[1:(min(which(x == 0)) - 1)]
    } else {
      return(0)
    }
    })
 return(sum(unlist(AllRelevantNumbers)))
}
