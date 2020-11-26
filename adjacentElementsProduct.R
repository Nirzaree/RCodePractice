# Given an array of integers,
# find the pair of adjacent elements 
# that has the largest product and return that product.

# For inputArray = [3, 6, -2, -5, 7, 3], the output should be
# adjacentElementsProduct(inputArray) = 21.
# 7 and 3 produce the largest product.

#method 1: get all adjacent products. get max.
# inputArray = list(3, 6, -2, -5, 7, 3)
adjacentElementsProduct <- function(inputArray) {
  adjacentProducts = lapply(seq(1:(length(inputArray)-1)), function(x) {
    return(inputArray[[x]] * inputArray[[x + 1]])
  })
  return(max(unlist(adjacentProducts)))
}
