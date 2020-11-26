# Given an array of equal-length strings, 
# you'd like to know if it's possible to
# rearrange the order of the elements in 
# such a way that each consecutive pair 
# of strings differ by exactly one character. 
# Return true if it's possible, and false if not.
# 
# Note: You're only rearranging the order of the strings, not the order of the letters within the strings!
#   
#   Example
# 
# For inputArray = ["aba", "bbb", "bab"], the output should be
# stringsRearrangement(inputArray) = false.
# 
# There are 6 possible arrangements for these strings:
# ["aba", "bbb", "bab"]
# ["aba", "bab", "bbb"]
# ["bbb", "aba", "bab"]
# ["bbb", "bab", "aba"]
# ["bab", "bbb", "aba"]
# ["bab", "aba", "bbb"]
# 
# None of these satisfy the condition of consecutive strings differing by 1 character, so the answer is false.
# 
# For inputArray = ["ab", "bb", "aa"], the output should be
# stringsRearrangement(inputArray) = true.
# 
# It's possible to arrange these strings in a way that each consecutive pair of strings differ by 1 character (eg: "aa", "ab", "bb" or "bb", "ab", "aa"), so return true.
# 

# inputArray = list("aba", 
#               "bbb", 
#               "bab")
# 
# inputArray = list("ab", "bb", "aa")
# 
# inputArray = list("abc",
#                   "bef",
#                   "bcc",
#                   "bec",
#                   "bbc",
#                   "bdc")

# inputArray = list(
#   "ff", 
#    "gf", 
#    "af", 
#    "ar", 
#    "hf"
# )

#todo: write own permutation logic: 

#big mistake I was making till now, was not storing the permutation indices but computing
#them each time and extracting the specific row. :( Spent hours/days.. over this
stringsRearrangement <- function(inputArray) {
   inputArray <- unlist(inputArray)
   # https://stackoverflow.com/questions/11095992/generating-all-distinct-permutations-of-a-list-in-r

   permutations <- function(n){
      if(n==1){
         return(matrix(1))
      } else {
         sp <- permutations(n-1)
         p <- nrow(sp)
         A <- matrix(nrow=n*p,ncol=n)
         for(i in 1:n){
            A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i))
         }
         return(A)
      }
   }
   permutationindices <- permutations(length(inputArray))
   for (ind in 1:factorial(length(inputArray))) {
      #get the values in that order
      combinationitn <- inputArray[permutationindices[ind,]]
      diffarray = 0;
      for (ind2 in 1:(length(combinationitn)-1)) {
         if (sum(strsplit(combinationitn[ind2],"")[[1]] != strsplit(combinationitn[ind2 + 1],"")[[1]]) == 1) {
            diffarray = diffarray + 1
         } else {
            break
         }
      }
      if (diffarray == (length(inputArray) - 1)) {
         return(TRUE)
      }
   }
   return(FALSE)
}

#still not fast enough: die die
# stringsRearrangement <- function(inputArray) {
#    inputArray <- unlist(inputArray)
#    # https://stackoverflow.com/questions/11095992/generating-all-distinct-permutations-of-a-list-in-r
#    
#    permutations <- function(n){
#       if(n==1){
#          return(matrix(1))
#       } else {
#          sp <- permutations(n-1)
#          p <- nrow(sp)
#          A <- matrix(nrow=n*p,ncol=n)
#          for(i in 1:n){
#             A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i))
#          }
#          return(A)
#       }
#    }
#    
#    for (ind in 1:factorial(length(inputArray))) {
#       permutationindices <- permutations(length(inputArray))[ind,]
#       #get the values in that order
#       combinationitn <- inputArray[permutationindices]
#          
#       combinationitn <- lapply(combinationitn,utf8ToInt)
#       diffarray <- lapply(seq(1:(length(combinationitn) - 1)), function(x) {
#           return((combinationitn[[x]] - combinationitn[[x+1]]))
#       })
#       
#      nonzeroelements <- lapply(diffarray,function(x) {
#        return(sum(x != 0))
#      })
#     if (all(abs(unlist(nonzeroelements)) == 1)) {
#       return(TRUE)
#     }
#    }
#    return(FALSE)
# }

#combinat library is not there on codesignal. 
# library(combinat)
# stringsRearrangement <- function(inputArray) {
#    inputArray <- unlist(inputArray)
#    allcombinations <- permn(inputArray)
# 
#    output <- FALSE
#    for (x in seq(1:length(allcombinations))) {
#       speccombination <- allcombinations[[x]]
#       if (output == FALSE) {
#          alldiffwithinthecombination <- lapply(seq(1:(length(speccombination)-1)),function(y) {
#             sum(strsplit(speccombination[y],"")[[1]] != strsplit(speccombination[y+1],"")[[1]])
#          })
#          if (all(alldiffwithinthecombination == 1)) {
#             output <- TRUE
#             break
#          }
#       }
#    }
#    return(output)
# }


# stringsRearrangement <- function(inputArray) {
#   #convert string to int encoding
#   inputArray <- lapply(inputArray,utf8ToInt)
#   # inputArray <- as.vector(unlist(inputArray))
#   # inputArray <- strsplit(inputArray,"")
#   
#   #check number of different bits in the arrangement. 
#   diffarray <- lapply(seq(1:(length(inputArray) - 1)), function(x) {
#     return((inputArray[[x]] - inputArray[[x+1]]))
#   })
#   #rules: 
#   #for true
#   #1. length of diffarray = 1 element and it is 1
#   #2. either each row has only 1 nonzero element and it is abs() == 1
#   #3. do columnsum do  there's only 1 element which is abs() >= 1.
#   
#   #check for rule1
#   if (length(diffarray) == 1 && all(diffarray == 0)) {
#     return(FALSE)
#   }
#   #check for rule2
#   nonzeroelements <- lapply(diffarray,function(x) {
#     return(sum(x != 0))
#   })
#   if (all(abs(unlist(nonzeroelements)) == 1)) {
#     return(TRUE)
#   } 
#   #check for other rules
#   columnsums <- colSums(do.call(rbind,diffarray))
#   if (sum(columnsums) == 1) {
#     return(TRUE)
#   } else {
#     return(FALSE)
#   }
# }

##old
# stringsRearrangement <- function(inputArray) {
#   #convert string to int encoding
#   inputArray <- lapply(inputArray,utf8ToInt)
#   # inputArray <- as.vector(unlist(inputArray))
#   # inputArray <- strsplit(inputArray,"")
#   
#   #check number of different bits in the arrangement. 
#   diffarray <- lapply(seq(1:(length(inputArray) - 1)), function(x) {
#     return((inputArray[[x]] - inputArray[[x+1]]))
#   })
#   #rules: 
#   #for true
#   #1. length of diffarray = 1 element and it is 1
#   #2. either each row has only 1 nonzero element and it is abs() == 1
#   #3. do columnsum do  there's only 1 element which is abs() >= 1.
#   
#   #check for rule1
#   if (length(diffarray) == 1 && all(diffarray == 0)) {
#     return(FALSE)
#   }
#   #check for rule2
#   nonzeroelements <- lapply(diffarray,function(x) {
#     return(sum(x != 0))
#   })
#   if (all(abs(unlist(nonzeroelements)) == 1)) {
#     return(TRUE)
#   } 
#   #check for other rules
#   columnsums <- colSums(do.call(rbind,diffarray))
#   if ((sum(abs(columnsums) >= 1) == 1) || (any(columnsums == 0) && (sum(abs(columnsums) > 1) == 1))) {
#     return(TRUE)
#   } else {
#     return(FALSE)
#   }
# }