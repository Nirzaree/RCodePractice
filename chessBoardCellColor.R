# Given two cells on the standard chess board, determine whether they have the same color or not.
# 
# Example
# 
# For cell1 = "A1" and cell2 = "C3", the output should be
# chessBoardCellColor(cell1, cell2) = true.
# 
# For cell1 = "A1" and cell2 = "H3", the output should be
# chessBoardCellColor(cell1, cell2) = false.
# 
# Input/Output
# 
# [execution time limit] 5 seconds (r)
# 
# [input] string cell1
# 
# Guaranteed constraints:
#   cell1.length = 2,
# 'A' ≤ cell1[0] ≤ 'H',
# 1 ≤ cell1[1] ≤ 8.
# 
# [input] string cell2
# 
# Guaranteed constraints:
#   cell2.length = 2,
# 'A' ≤ cell2[0] ≤ 'H',
# 1 ≤ cell2[1] ≤ 8.
# 
# [output] boolean
# 
# true if both cells have the same color, false otherwise.

#if cell row and column are alternating from the other cell, it will 
#have the same color. 
#convert row numbers to also ascii & check for alternating
#columns are already numbers

cell1 = "A1"
cell2 = "H3"

chessBoardCellColor <- function(cell1, cell2) {
  cell1 <- as.vector(unlist(strsplit(cell1,"")))
  cell2 <- as.vector(unlist(strsplit(cell2,"")))  
  
  if (((abs(utf8ToInt(cell1[1]) - utf8ToInt(cell2[1])) %% 2  == 0) & (abs(utf8ToInt(cell1[2]) - utf8ToInt(cell2[2])) %% 2  == 0)) || (
    (abs(utf8ToInt(cell1[1]) - utf8ToInt(cell2[1])) %% 2  == 1) & (abs(utf8ToInt(cell1[2]) - utf8ToInt(cell2[2])) %% 2  == 1)
  )) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
