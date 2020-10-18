# Sudoku is a number-placement puzzle. The objective is to fill a 9 × 9 grid with numbers in such a way that each column, each row, and each of the nine 3 × 3 sub-grids that compose the grid all contain all of the numbers from 1 to 9 one time.
# 
# Implement an algorithm that will check whether the given grid of numbers represents a valid Sudoku puzzle according to the layout rules described above. Note that the puzzle represented by grid does not have to be solvable.
# 
# Example
# 
# For
# 
# grid = [['.', '.', '.', '1', '4', '.', '.', '2', '.'],
#         ['.', '.', '6', '.', '.', '.', '.', '.', '.'],
#         ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
#         ['.', '.', '1', '.', '.', '.', '.', '.', '.'],
#         ['.', '6', '7', '.', '.', '.', '.', '.', '9'],
#         ['.', '.', '.', '.', '.', '.', '8', '1', '.'],
#         ['.', '3', '.', '.', '.', '.', '.', '.', '6'],
#         ['.', '.', '.', '.', '.', '7', '.', '.', '.'],
#         ['.', '.', '.', '5', '.', '.', '.', '7', '.']]
# 
# the output should be
# sudoku2(grid) = true;
# 
# For
# 
# grid = [['.', '.', '.', '.', '2', '.', '.', '9', '.'],
#         ['.', '.', '.', '.', '6', '.', '.', '.', '.'],
#         ['7', '1', '.', '.', '7', '5', '.', '.', '.'],
#         ['.', '7', '.', '.', '.', '.', '.', '.', '.'],
#         ['.', '.', '.', '.', '8', '3', '.', '.', '.'],
#         ['.', '.', '8', '.', '.', '7', '.', '6', '.'],
#         ['.', '.', '.', '.', '.', '2', '.', '.', '.'],
#         ['.', '1', '.', '2', '.', '.', '.', '.', '.'],
#         ['.', '2', '.', '.', '3', '.', '.', '.', '.']]
# 
# the output should be
# sudoku2(grid) = false.
# 
# The given grid is not correct because there are two 1s in the second column. Each column, each row, and each 3 × 3 subgrid can only contain the numbers 1 through 9 one time.
# 
# Input/Output
# 
# [execution time limit] 5 seconds (r)
# 
# [input] array.array.char grid
# 
# A 9 × 9 array of characters, in which each character is either a digit from '1' to '9' or a period '.'.
# 
# [output] boolean
# 
# Return true if grid represents a valid Sudoku puzzle, otherwise return false.

#no repeating numbers in a row or a column or in a 3x3 grid 
#if they are then false

grid = list(list('.', '.', '.', '.', '2', '.', '.', '9', '.'),
            list('.', '.', '.', '.', '6', '.', '.', '.', '.'),
            list('7', '1', '.', '.', '7', '5', '.', '.', '.'),
            list('.', '7', '.', '.', '.', '.', '.', '.', '.'),
            list('.', '.', '.', '.', '8', '3', '.', '.', '.'),
            list('.', '.', '8', '.', '.', '7', '.', '6', '.'),
            list('.', '.', '.', '.', '.', '2', '.', '.', '.'),
            list('.', '1', '.', '2', '.', '.', '.', '.', '.'),
            list('.', '2', '.', '.', '3', '.', '.', '.', '.'))

# grid = list(list('.','.','.'),
#             list('.','.','.'),
#             list('.','.','.'))

# logic1: vanilla: solved 17/20 tests.

#logic2: additional to vanilla, solve the failing cases
# failinginput in logic1 : issue: silliest of all. lapply return is not global. 
#had to check for any false in that iteration and then return. 

grid = list(list(".",".","4",".",".",".","6","3","."),
            list(".",".",".",".",".",".",".",".","."),
            list("5",".",".",".",".",".",".","9","."),
            list(".",".",".","5","6",".",".",".","."),
            list("4",".","3",".",".",".",".",".","1"),
            list(".",".",".","7",".",".",".",".","."),
            list(".",".",".","5",".",".",".",".","."),
            list(".",".",".",".",".",".",".",".","."),
            list(".",".",".",".",".",".",".",".","."))

##issues: number of passing tests varyign for the same solution even when all hidden tests are revealed :|
#logic5: hopefully the last one
#do.call(rbind,grid) was slowing it all down 
#all that was needed was matrix() function. 
#since the numeric thing was not done, instead of NA checks, checks for dots are being done. 
sudoku2 <- function(grid) {
        # grid <- lapply(grid,as.numeric)
        # grid <- do.call(rbind,grid)
        grid <- matrix(unlist(grid),nrow = 9,ncol = 9,byrow = T)
        validsudoku <- TRUE
        
        for (rowindex in 1:3) {
                for (colindex in 1:3) {
                        rowdata = grid[3*(rowindex-1) + colindex,]
                        if (length(rowdata[rowdata != "."]) != length(unique(rowdata[rowdata != "."]))) {
                                validsudoku <- FALSE
                                break
                        }
                        coldata = grid[,3*(rowindex-1) + colindex]
                        if (length(coldata[coldata != "."]) != length(unique(coldata[coldata != "."]))) {
                                validsudoku <- FALSE
                                break
                        }
                        temp = grid[(3*(rowindex-1) + 1):(3*(rowindex-1) + 3),(3*(colindex-1) + 1):(3*(colindex-1) + 3)]
                        if (length(temp[temp != "."]) != length(unique(temp[temp != "."]))) {
                                validsudoku <- FALSE
                                break
                        }
                }
                if (validsudoku == FALSE) {
                        break
                }
        }
        if (validsudoku == FALSE) {
                return(FALSE)
        } else {
                return(TRUE)
        }
}

#logic4: with some wisdom after breaking head for days : 29 tests passing. time up on 30th.
sudoku2 <- function(grid) {
        grid <- lapply(grid,as.numeric)
        grid <- do.call(rbind,grid)

        for (rowindex in 1:3) {
                for (colindex in 1:3) {
                        rowdata = grid[3*(rowindex-1) + colindex,]
                        rowdata = rowdata[!is.na(rowdata)]
                        if (length(rowdata) != length(unique(rowdata))) {
                                return(FALSE)
                        }
                        coldata = grid[,3*(rowindex-1) + colindex]
                        coldata = coldata[!is.na(coldata)]
                        if (length(coldata) != length(unique(coldata))) {
                                return(FALSE)
                        }
                        temp = grid[(3*(rowindex-1) + 1):(3*(rowindex-1) + 3),(3*(colindex-1) + 1):(3*(colindex-1) + 3)]
                        temp = temp[!is.na(temp)]
                        if (length(temp) != length(unique(temp))) {
                                return(FALSE)
                        }
                }
        }
        return(TRUE)
}
#logic3: slightly faster

sudoku2 <- function(grid) {
        # start.time <- Sys.time()
        grid <- lapply(grid,as.numeric)
        grid <- do.call(rbind,grid)
        # grid <- matrix(unlist(grid), ncol = length(grid), byrow = T)
        # grid <- matrix(unlist(grid),nrow = 9,byrow = T)
        # grid <- rbindlist(grid)
        # grid <- grid[, lapply(.SD, as.numeric)]
        # validsudoku <- TRUE

        for (rowindex in 1:3) {
                # print(:rowindex)
                for (colindex in 1:3) {
                        # print(colindex)
                        rowdata = grid[3*(rowindex-1) + colindex,]
                        rowdata = rowdata[!is.na(rowdata)]
                        if (length(rowdata) > 0 && sum(duplicated(rowdata)) > 0) {
                                return(FALSE)
                        }
                        coldata = grid[,3*(rowindex-1) + colindex]
                        if (length(coldata) > 0 && sum(duplicated(coldata)) > 0) {
                                return(FALSE)
                        }
                        temp = grid[(3*(rowindex-1) + 1):(3*(rowindex-1) + 3),(3*(colindex-1) + 1):(3*(colindex-1) + 3)]
                        if (length(temp) > 0 && sum(duplicated(temp)) > 0) {
                                return(FALSE)
                        }
                }
        }
return(TRUE)
}

# sudoku2 <- function(grid) {
#         grid <- lapply(grid,as.numeric)
#         grid <- do.call(rbind,grid)
#         validsudoku <- TRUE
#         
#         for (rowindex in 1:3) {
#                 for (colindex in 1:3) {
#                         rowdata = grid[3*(rowindex-1) + colindex,]
#                         if (length(rowdata[!is.na(rowdata)]) != length(unique(rowdata[!is.na(rowdata)]))) {
#                                 validsudoku <- FALSE
#                                 break                                
#                         }
#                         coldata = grid[,3*(rowindex-1) + colindex]
#                         if (length(coldata[!is.na(coldata)]) != length(unique(coldata[!is.na(coldata)]))) {
#                                 validsudoku <- FALSE
#                                 break                                
#                         }
#                         temp = grid[(3*(rowindex-1) + 1):(3*(rowindex-1) + 3),(3*(colindex-1) + 1):(3*(colindex-1) + 3)]
#                         if (length(temp[!is.na(temp)]) != length(unique(temp[!is.na(temp)]))) {
#                                 validsudoku <- FALSE
#                                 break
#                         }
#                 }
#                 if (validsudoku == FALSE) {
#                         break
#                 }
#         }
#         if (validsudoku == FALSE) {
#                 return(FALSE)
#         } else {
#                 return(TRUE)
#         }
# }

# sudoku2 <- function(grid) {
#   grid <- lapply(grid,as.numeric)        
#   grid <- do.call(rbind,grid)
#   
#   #empty is true
#   # if (all(is.na(grid))) {
#   #         return(TRUE)
#   # }
#   # grid <- as.data.table(grid)
# 
#   #apply on each row
# response <-  apply(grid,1,function(x) {
#     # x = x[!is.na(x)]
#     if (length(x[!is.na(x)]) != length(unique(x[!is.na(x)]))) {
#       return(FALSE)
#     }
#   })
# 
# if(any(unlist(response) == FALSE)) {
#         return(FALSE)
# }
# 
#   #apply on each column
# response <-  apply(grid,2,function(x) {
#         # x = x[!is.na(x)]
#         if (length(x[!is.na(x)]) != length(unique(x[!is.na(x)]))) {
#                 return(FALSE)
#         }
# })
# 
# if(any(unlist(response) == FALSE)) {
#         return(FALSE)
# }
# 
#   #apply on each 3x3 matrix
#   #there are 9 such grids
#   for (rowindex in 1:3) {
#     for (colindex in 1:3) {
#         temp = grid[(3*(rowindex-1) + 1):(3*(rowindex-1) + 3),(3*(colindex-1) + 1):(3*(colindex-1) + 3)]
#         if (length(temp[!is.na(temp)]) != length(unique(temp[!is.na(temp)]))) {
#           return(FALSE)
#         }
#     }
#   }
#   return(TRUE)
# }
# 
