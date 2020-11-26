# Construct a square matrix with a size N × N containing integers from 1 to N *
# N in a spiral order, starting from top-left and in clockwise direction.
#
# Example
#
# For n = 3, the output should be
#
# spiralNumbers(n) = [[1, 2, 3], [8, 9, 4], [7, 6, 5]]
#
# Input/Output
#
# [execution time limit] 5 seconds (r)
#
# [input] integer n
#
# Matrix size, a positive integer.
#
# Guaranteed constraints: 3 ≤ n ≤ 100.
#
# [output] array.array.integer
#
#

#spiral instead of inverted spiral
spiralNumbers <- function(n) {
  outputmatrix <- matrix(data = 0,
                         nrow = n,
                         ncol = n)
  rowind = 1;
  colind = 1;
  value = 1;
  outputmatrix[rowind,colind] = value;
  value = value + 1;
  for (ind in 1:(ceiling(n/2))) {
    while(colind < (n - ind + 1)) {
      colind = colind + 1;
      outputmatrix[rowind,colind] = value;
      value = value + 1;
    }
    while(rowind < (n - ind + 1)) {
      rowind = rowind + 1;
      outputmatrix[rowind,colind] = value;
      value = value + 1;
    }
    while(colind > (ind)) {
      colind = colind - 1;
      outputmatrix[rowind,colind] = value;
      value = value + 1;
    }
    while(rowind > (ind +1)) {
      rowind = rowind - 1;
      outputmatrix[rowind,colind] = value;
      value = value + 1;
    }
  }
  return(outputmatrix)
}

#hehe I wrote for inverted spiral. Question expects spiral. 
# spiralNumbers <- function(n) {
#   outputmatrix <- matrix(data = 0,
#                          nrow = n,
#                          ncol = n)
#   rowind = n;
#   colind = n;
#   value = 1;
#   for (ind in 1:(ceiling(n/2))) {
#     outputmatrix[rowind,colind] = value;
#     value = value + 1;
#     while(rowind > (ind)) {
#       rowind = rowind - 1;
#       outputmatrix[rowind,colind] = value;
#       value = value + 1;
#     }
#     while(colind > (ind)) {
#       colind = colind - 1;
#       outputmatrix[rowind,colind] = value;
#       value = value + 1;
#     }
#     while(rowind < (n - ind + 1)) {
#       rowind = rowind + 1;
#       outputmatrix[rowind,colind] = value;
#       value = value + 1;
#     }
#     while(colind < (n - ind)) {
#       colind = colind + 1;
#       outputmatrix[rowind,colind] = value;
#       value = value + 1;
#     }
#   }
#   return(outputmatrix)
# }