# Note: Try to solve this task in-place (with O(1) additional memory), since this is what you'll be asked to do during an interview.
# 
# You are given an n x n 2D matrix that represents an image. Rotate the image by 90 degrees (clockwise).
# 
# Example
# 
# For
# 
# a = [[1, 2, 3],
#      [4, 5, 6],
#      [7, 8, 9]]
# 
# the output should be
# 
# rotateImage(a) =
#     [[7, 4, 1],
#      [8, 5, 2],
#      [9, 6, 3]]
# 
# Input/Output
# 
#     [execution time limit] 5 seconds (r)
# 
#     [input] array.array.integer a
# 
#     Guaranteed constraints:
#     1 ≤ a.length ≤ 100,
#     a[i].length = a.length,
#     1 ≤ a[i][j] ≤ 104.
# 
#     [output] array.array.integer

b = list(list(1, 2, 3,4),
     list(5,6,7,8),
     list(9,10,11,12),
     list(13,14,15,16))

b = list(list(1, 2, 3),
         list(4,5,6),
         list(7,8,9))

b = list(list(1,2),
         list(3,4))

b = list(list(1, 2, 3,4,5),
         list(6,7,8,9,10),
         list(11,12,13,14,15),
         list(16,17,18,19,20),
         list(21,22,23,24,25))

b = list(list(1))
b = do.call(rbind,b)

#speed up logic 3 : logic 3 just worked. whaatt!
rotateImage <- function(a) {
  if (length(a) == 1 & length(a[[1]]) == 1) {
    return(a)
  }
  a = do.call(rbind,a)
  nframes = floor(nrow(a)/2)

  for (frameitn in seq(1:nframes)) {
    #rotate elements of the frame
    colIndexMin = frameitn;
    colIndexMax = nrow(a) - frameitn;
    rowindex = frameitn

    for (colindex in seq(from = colIndexMin, to = colIndexMax)) {
      temp = a[colindex,nrow(a) - rowindex + 1]
      #move
      a[colindex,nrow(a) - rowindex + 1] = a[rowindex,colindex]

      #right to bottom
      #find target of the value in temp
      temp2 = a[nrow(a) - rowindex + 1,nrow(a) - colindex + 1]
      a[nrow(a) - rowindex + 1,nrow(a) - colindex + 1] = temp

      #bottom to left
      temp = a[nrow(a) - colindex + 1,nrow(a) - (nrow(a) - rowindex + 1) + 1]
      a[nrow(a) - colindex + 1,nrow(a) - (nrow(a) - rowindex + 1) + 1] = temp2

      #left to top
      a[nrow(a) - (nrow(a) - rowindex + 1) + 1,nrow(a) - (nrow(a) - colindex + 1) + 1] = temp
    }
  }
  # return(a)
  return(lapply(seq(1:nrow(a)),function(x) as.list(a[x,]))) #return as list of list duhhh..
}


#logic3: find number of frames. loop within them. Working. failing for speed in last 2 tests. 
# rotateImage <- function(a) {
#   if (length(a) == 1 & length(a[[1]]) == 1) {
#     return(a)
#   }
#   a = do.call(rbind,a)
#   nframes = floor(nrow(a)/2)
#   
#   for (frameitn in seq(1:nframes)) {
#     #rotate elements of the frame
#     colIndexMin = frameitn;
#     colIndexMax = nrow(a) - frameitn;
#     rowindex = frameitn
#     
#     for (colindex in seq(from = colIndexMin, to = colIndexMax)) {
#       temp = a[colindex,nrow(a) - rowindex + 1]
#       #move
#       a[colindex,nrow(a) - rowindex + 1] = a[rowindex,colindex]
#       
#       #right to bottom
#       #find target of the value in temp
#       temp2 = a[nrow(a) - rowindex + 1,nrow(a) - colindex + 1]
#       a[nrow(a) - rowindex + 1,nrow(a) - colindex + 1] = temp
#       
#       #bottom to left
#       temp = a[nrow(a) - colindex + 1,nrow(a) - (nrow(a) - rowindex + 1) + 1]
#       a[nrow(a) - colindex + 1,nrow(a) - (nrow(a) - rowindex + 1) + 1] = temp2
#       
#       #left to top
#       a[nrow(a) - (nrow(a) - rowindex + 1) + 1,nrow(a) - (nrow(a) - colindex + 1) + 1] = temp
#     }
#   }
#   # return(a)
#   return(lapply(seq(1:nrow(a)),function(x) as.list(a[x,]))) #return as list of list duhhh..
# }


#1. trying to solve it someway. 
#logic: new indices: newrowindex = prevcolumnindex ; newcolumnindex = nrow - givenrow + 1
# rotateImage <- function(a) {
#   a = do.call(rbind,a)
#   #number of swap cycles = nrow - 1
#   #outermost circle swap
#   #1.
#   rowindex = 1
#   colindex = 1
# 
#   #next itn:
#   rowindex = 1
#   colindex = 2
#   
#   #next 
#   rowindex = 1
#   colindex = 3
#   
#   #next
#   rowindex = 2
#   colindex = 2
#   
#   #done
#   
#   #target position of this cell 
#   #top to right
#   for (rowindex in seq(1:(nrow(a) - 2))) {
#     for (colindex in seq(rowindex:(ncol(a) - 1))) {
#       temp = a[colindex,nrow(a) - rowindex + 1]
#       #move
#       a[colindex,nrow(a) - rowindex + 1] = a[rowindex,colindex]
#       
#       #right to bottom
#       #find target of the value in temp
#       temp2 = a[nrow(a) - rowindex + 1,nrow(a) - colindex + 1]
#       a[nrow(a) - rowindex + 1,nrow(a) - colindex + 1] = temp
#       
#       #bottom to left
#       temp = a[nrow(a) - colindex + 1,nrow(a) - (nrow(a) - rowindex + 1) + 1]
#       a[nrow(a) - colindex + 1,nrow(a) - (nrow(a) - rowindex + 1) + 1] = temp2
#       
#       #left to top
#       a[nrow(a) - (nrow(a) - rowindex + 1) + 1,nrow(a) - (nrow(a) - colindex + 1) + 1] = temp
#     }
#   }
# 
# }
# for (rowindex in seq(1:nrow(a))) {
#   for (colindex in seq(1:ncol(a))) {
#     #get the element #put it in a buffer
#     temp = a[colindex,nrow(a) - rowindex + 1]
#     #swap
#     a[colindex,nrow(a) - rowindex + 1] = a[rowindex,colindex]
#   }
#   
# }
