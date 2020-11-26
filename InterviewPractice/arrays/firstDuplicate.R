# Given an array a that contains only numbers in the range from 1 to a.length, find the first duplicate number for which the second occurrence has the minimal index. In other words, if there are more than 1 duplicated numbers, return the number for which the second occurrence has a smaller index than the second occurrence of the other number does. If there are no such elements, return -1.
# 
# Example
# 
# For a = [2, 1, 3, 5, 3, 2], the output should be firstDuplicate(a) = 3.
# 
# There are 2 duplicates: numbers 2 and 3. The second occurrence of 3 has a smaller index than the second occurrence of 2 does, so the answer is 3.
# 
# For a = [2, 2], the output should be firstDuplicate(a) = 2;
# 
# For a = [2, 4, 3, 5, 1], the output should be firstDuplicate(a) = -1.
# 
# Input/Output
# 
# [execution time limit] 5 seconds (r)
# 
# [input] array.integer a
# 
# Guaranteed constraints:
#   1 ≤ a.length ≤ 105,
# 1 ≤ a[i] ≤ a.length.
# 
# [output] integer
# 
# The element in a that occurs in the array more than once and has the minimal index for its second occurrence. If there are no such elements, return -1.

#Vectors faster than lists. 
#But still not fast enough to pass all tests.
#Trying datatable again. 
library(data.table)
# a = list(2, 1, 3, 5, 3, 2)

#final solution : not very happy as I wanted to write code without using any functino
#which is fast enough. todo: try again later.
firstDuplicate <- function(a) {
  if (sum(duplicated(a) > 0)) {
    return(a[duplicated(a)][[1]])
  } else {
    return(-1)
  } 
}

# #datatable initialization causing issues in codesignal :( 
# firstDuplicate <- function(a) {
#   # a = as.vector(unlist(a)) #removing this step. no improvement from results side
#   if (length(unique(a)) == length(a)) {
#     return(-1)
#   } else {
#     #traverse the string upto first duplication position
#     charAlreadyOccured <- vector()
#     for (ind in 1:length(a)) {
#       # print(ind)
#       if (a[[ind]] %in% charAlreadyOccured) { #no change in speed
#       # if (any(charAlreadyOccured == a[[ind]])) { #changing to %in% 
#         return(a[[ind]])
#       }
#       charAlreadyOccured <- c(charAlreadyOccured,a[[ind]]) #c against rbind
#     }
#   }
# }

#list: not as fast. Vector is faster but not fast enough to crack the last 2 tests.
# firstDuplicate <- function(a) {
#   a = as.vector(unlist(a))
#   if (length(unique(a)) == length(a)) {
#     return(-1)
#   } else {
#     #traverse the string upto first duplication position
#     charAlreadyOccured <- list()
#     for (ind in 1:length(a)) {
#       # print(ind)
#       if (a[ind] %in% charAlreadyOccured) {
#         return(a[ind])
#       }
#       charAlreadyOccured <- rbind(charAlreadyOccured,a[ind])
#     }
#   }
# }

#traversing whole list which is not required. 
# firstDuplicate <- function(a) {
#   a = as.vector(unlist(a))
#   uniqueVal = unique(a)
#   if (length(uniqueVal) == length(a)) {
#     return(-1)
#   } else {
#     pos = data.table(value = numeric(1),pos2 = numeric(1))
#     #only run loop for more than 1 occurence objects
#     for (ind in seq(1:uniqueN(a))) {
#       if (length(a[a == uniqueVal[ind]]) > 1) {
#         if (pos[,pos2] == 0 || (which(a == uniqueVal[ind])[2] < pos[1,pos2])) {
#           pos[,value := uniqueVal[ind]]
#           pos[,pos2 := which(a == uniqueVal[ind])[2]]
#         }
#       }
#     }
#     #remove 0 rows
#     # pos = pos[rowSums(pos) > 0] 
#     return(pos[,pos2])
#   }
# }
