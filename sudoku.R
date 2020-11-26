# Sudoku is a number-placement puzzle. The objective is to fill a 9 × 9 grid
# with digits so that each column, each row, and each of the nine 3 × 3
# sub-grids that compose the grid contains all of the digits from 1 to 9.
#
# This algorithm should check if the given grid of numbers represents a correct
# solution to Sudoku.
#
# Example
#
# For
#
# grid = [[1, 3, 2, 5, 4, 6, 9, 8, 7], [4, 6, 5, 8, 7, 9, 3, 2, 1], [7, 9, 8, 2,
# 1, 3, 6, 5, 4], [9, 2, 1, 4, 3, 5, 8, 7, 6], [3, 5, 4, 7, 6, 8, 2, 1, 9], [6,
# 8, 7, 1, 9, 2, 5, 4, 3], [5, 7, 6, 9, 8, 1, 4, 3, 2], [2, 4, 3, 6, 5, 7, 1, 9,
# 8], [8, 1, 9, 3, 2, 4, 7, 6, 5]]
#
# the output should be sudoku(grid) = true;
#
# For
#
# grid = [[1, 3, 2, 5, 4, 6, 9, 2, 7], [4, 6, 5, 8, 7, 9, 3, 8, 1], [7, 9, 8, 2,
# 1, 3, 6, 5, 4], [9, 2, 1, 4, 3, 5, 8, 7, 6], [3, 5, 4, 7, 6, 8, 2, 1, 9], [6,
# 8, 7, 1, 9, 2, 5, 4, 3], [5, 7, 6, 9, 8, 1, 4, 3, 2], [2, 4, 3, 6, 5, 7, 1, 9,
# 8], [8, 1, 9, 3, 2, 4, 7, 6, 5]]
#
# the output should be sudoku(grid) = false.
#
# The output should be false: each of the nine 3 × 3 sub-grids should contain
# all of the digits from 1 to 9.

grid <- list(list(1,3,2,5,4,6,9,2,7), 
 list(4,6,5,8,7,9,3,8,1), 
 list(7,9,8,2,1,3,6,5,4), 
 list(9,2,1,4,3,5,8,7,6), 
 list(3,5,4,7,6,8,2,1,9), 
 list(6,8,7,1,9,2,5,4,3), 
 list(5,7,6,9,8,1,4,3,2), 
 list(2,4,3,6,5,7,1,9,8), 
 list(8,1,9,3,2,4,7,6,5))

library(data.table)
sudoku <- function(grid) {
  grid <- matrix(data = unlist(grid),nrow = 9,byrow = T)
  #check if any repeats in row
  for (x in seq(1:9)) {
    if ((uniqueN(grid[,x]) != 9) | (uniqueN(grid[x,]) != 9)) {
      return(FALSE)
      }
  }

  #subarrays
  for (ind1 in seq(from = 1, to = 7, by = 3)) {
    for (ind2 in seq(from = 1, to = 7, by = 3)) {
      submatrix <- grid[ind1:(ind1 + 2),ind2:(ind2+2)]
      if (uniqueN(submatrix) != 9) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}
