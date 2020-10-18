# In the popular Minesweeper game you have a board with some mines and those cells that don't contain a mine have a number in it that indicates the total number of mines in the neighboring cells. Starting off with some arrangement of mines we want to create a Minesweeper game setup.
# 
# Example
# 
# For
# 
# matrix = [[true, false, false],
#           [false, true, false],
#           [false, false, false]]
# 
# the output should be
# 
# minesweeper(matrix) = [[1, 2, 1],
#                        [2, 1, 1],
#                        [1, 1, 1]]
# 
# Check out the image below for better understanding:
# 
# Input/Output
# 
#     [execution time limit] 5 seconds (r)
# 
#     [input] array.array.boolean matrix
# 
#     A non-empty rectangular matrix consisting of boolean values - true if the corresponding cell contains a mine, false otherwise.
# 
#     Guaranteed constraints:
#     2 ≤ matrix.length ≤ 100,
#     2 ≤ matrix[0].length ≤ 100.
# 
#     [output] array.array.integer
#         Rectangular matrix of the same size as matrix each cell of which contains an integer equal to the number of mines in the neighboring cells. Two cells are called neighboring if they share at least one corner.

# steps:
# for each cell, get relevant neighboring cells, get the output number, return. 

#happy happy happy B-)
library(data.table)
library(raster)

matrix2 = list(list(TRUE,FALSE,FALSE,TRUE), 
          list(FALSE,FALSE,TRUE,FALSE), 
          list(TRUE,TRUE,FALSE,TRUE))

  minesweeper <- function(matrix2) {
    # matrix2 <- do.call(cbind,matrix2)
    # matrix2 <- t(matrix2)
    matrix2 <- matrix(unlist(matrix2),nrow = length(matrix2),byrow=T)
    
    euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
    newmat <- matrix(nrow = nrow(matrix2),ncol = ncol(matrix2))
    for (rowindex in 1:nrow(matrix2)) {
      for (colindex in 1:ncol(matrix2)) {
        #get relevant submatrix
        neighboringMatrix <- matrix2[max(rowindex-1,1):min(rowindex+1,nrow(matrix2)),max(colindex-1,1):min(colindex+1,ncol(matrix2))]
        #get distance of each cell in the neighboring matrix from the given cell
        # trueNeighbor <- data.table(rowindex = numeric(),
        #                            colindex = numeric())
        
        # trueneighborlistindex <- 1
        trueneighborsum = 0
        for (rowindex2 in max(rowindex-1,1):min(rowindex+1,nrow(matrix2))) {
          for (colindex2 in max(colindex-1,1):min(colindex+1,ncol(matrix2))) {
            dist <- (euc.dist(c(rowindex2,colindex2),c(rowindex,colindex)))
            if (dist < 2 & dist > 0) {
              # trueNeighbor[[trueneighborlistindex]] <- c(rowindex2,colindex2)
              # trueneighborlistindex <- trueneighborlistindex + 1
              # trueNeighbor <- rbind(trueNeighbor,
              #                       list(rowindex2,colindex2))
              trueneighborsum = as.numeric(matrix2[rowindex2,colindex2]) + trueneighborsum;
             }
          }
        }
        newmat[rowindex,colindex] = trueneighborsum;
      }
    }
    return(newmat)
  }
