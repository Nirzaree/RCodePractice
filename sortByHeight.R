# Some people are standing in a row in a park. There are trees between them which cannot be moved. Your task is to rearrange the people by their heights in a non-descending order without moving the trees. People can be very tall!
#   
#   Example
# 
# For a = [-1, 150, 190, 170, -1, -1, 160, 180], the output should be
# sortByHeight(a) = [-1, 150, 160, 170, -1, -1, 180, 190].
# 
# Input/Output
# 
# [execution time limit] 5 seconds (r)
# 
# [input] array.integer a
# 
# If a[i] = -1, then the ith position is occupied by a tree. Otherwise a[i] is the height of a person standing in the ith position.
# 
# Guaranteed constraints:
#   1 ≤ a.length ≤ 1000,
# -1 ≤ a[i] ≤ 1000.
# 
# [output] array.integer
# Sorted array a with all the trees untouched.
# a = list(-1, 150, 190, 170, -1, -1, 160, 180)

#comments: max pain because of the output format required. R does not have array,
#hence conversion from list etc. 

library(data.table)

sortByHeight <- function(a) {
  if (length(a) == 1) {
    return(a)
  } else {
    a = as.vector(unlist(a))
    a = as.data.table(a);
    a[,"pos"] <- seq(1:nrow(a))
    treespos = a[a == -1]
    getallpeopleheights <- a[a > -1]
    if (length(getallpeopleheights) > 0) {
      orderpeopleinheight <- getallpeopleheights[order(a)]
      newpos <- (orderpeopleinheight[,sort(pos)])
      orderpeopleinheight[,pos := newpos]
      a <- rbind(orderpeopleinheight,treespos)
      a <- a[order(pos)]
      a[,pos := NULL]
    } else {
      a[,pos := NULL]
    }
    a <- unname(a)
    return(unlist(as.list(a)))
  }
}

#   #Solution a: using R.utils 
# a = as.vector(unlist(a))
# getallpeopleheights <- a[a != -1]
# if (length(getallpeopleheights) > 0) {
#   orderpeopleinheight <- getallpeopleheights[order(getallpeopleheights)]
#   finalOutput <- insert(orderpeopleinheight,which(a == -1),-1)
#   return(finalOutput)
# } else {
#   return(a)
# }

#SolutionB: without R.utils
# a = as.vector(unlist(a))
# b =  data.table(a)
# b = b[ifelse(b == -1,-1,NA) ]
# getallpeopleheights <- a[a != -1]
# if (length(getallpeopleheights) > 0) {
#   orderpeopleinheight <- getallpeopleheights[order(getallpeopleheights)]
#   finalOutput <- insert(orderpeopleinheight,which(a == -1),-1)
#   return(finalOutput)
# } else {
#   return(a)
# }
# }
# count = 1;
# check <- lapply(seq(1:length(a)), function(x) {
#   if (a[x] == -1) {
#     return (a[x]) } else {
#       finalValue = getallpeopleheights[count]
#       count = count + 1;
#       return(finalValue)
#     }
