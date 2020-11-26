# Given a rectangular matrix containing only digits, calculate the number of
# different 2 × 2 squares in it.
#
# Example
#
# For
#
# matrix = [[1, 2, 1], [2, 2, 2], [2, 2, 2], [1, 2, 3], [2, 2, 1]]
#
# the output should be differentSquares(matrix) = 6.

#put all submatrices in list.
#do unique(submatrixlist)

library(data.table)
inputmatrix = list(list(1, 2, 1), list(2, 2, 2), list(2, 2, 2), list(1, 2, 3), list(2, 2, 1))
inputmatrix = list(list(3))
inputmatrix = list(list(3,4,5,6,7,8,9))
differentSquares <- function(inputmatrix) {
  inputmatrix <- matrix(unlist(inputmatrix),nrow = length(inputmatrix),byrow = T)
  lsubmatrix <- vector("list",((nrow(inputmatrix) - 1) * (ncol(inputmatrix)-1)))
  if ((nrow(inputmatrix) < 2) || (ncol(inputmatrix) < 2)) {
    return(0)
  }
  index = 1;
  for (rowindex in 1:(nrow(inputmatrix)-1)) {
    for (colindex in 1:(ncol(inputmatrix)-1)) {
      lsubmatrix[[index]] <- inputmatrix[(rowindex):(rowindex+1),(colindex):(colindex+1)]
      index <- index + 1;
    }
  }
  return(length(unique(lsubmatrix)))
}
