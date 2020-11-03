# Given a position of a knight on the standard chessboard, find the number of different moves the knight can perform.
# 
# The knight can move to a square that is two squares horizontally and one square vertically, or two squares vertically and one square horizontally away from it. The complete move therefore looks like the letter L. Check out the image below to see all valid moves for a knight piece that is placed on one of the central squares.
# 
# Example
# 
# For cell = "a1", the output should be
# chessKnight(cell) = 2.
# 
# For cell = "c2", the output should be
# chessKnight(cell) = 6.
# 
# Input/Output
# 
# [execution time limit] 5 seconds (r)
# 
# [input] string cell
# 
# String consisting of 2 letters - coordinates of the knight on an 8 × 8 chessboard in chess notation.
# 
# Guaranteed constraints:
#   cell.length = 2,
# 'a' ≤ cell[0] ≤ 'h',
# 1 ≤ cell[1] ≤ 8.
# 
# [output] integer
# 
# [R] Syntax Tips

# cell = "a1"

#Logic: the possible moves make a T shape on 4 sides. 
#Check if each such position is within the square or not. 
#Easiest solution that I could think of. 
#todo: more elegant solution
#2d validation check as done in one of the solutions would be a better solution. 

chessKnight <- function(cell) {
  dtAlphatoInt <- data.table(alphabets = c("a","b","c","d","e","f","g","h"),
                             int = c(1,2,3,4,5,6,7,8))
  
  cell <- strsplit(cell,"")[[1]]
  cell[1] <- dtAlphatoInt[alphabets == cell[1],int]
  cell <- as.numeric(cell)
  #find all 8 positions. 
  #if their indices exist on the board, they are counted else not. 
  positions = 0;
  #position 1: 
  if (between(cell[1] + 2,1,8)) {
    if (between(cell[2] + 1,1,8)) {
      positions = positions + 1;
    }
    if (between(cell[2] - 1,1,8)) {
      positions = positions + 1;
    }
  }
  if (between(cell[1] - 2,1,8)) {
    if (between(cell[2] + 1,1,8)) {
      positions = positions + 1;
    }
    if (between(cell[2] - 1,1,8)) {
      positions = positions + 1;
    }
  }
  if (between(cell[2] - 2,1,8)) {
    if (between(cell[1] + 1,1,8)) {
      positions = positions + 1;
    }
    if (between(cell[1] - 1,1,8)) {
      positions = positions + 1;
    }
  }
  if (between(cell[2] + 2,1,8)) {
    if (between(cell[1] + 1,1,8)) {
      positions = positions + 1;
    }
    if (between(cell[1] - 1,1,8)) {
      positions = positions + 1;
    }
  }
  return(positions)
}
