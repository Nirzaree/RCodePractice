# You have an unsorted array arr of non-negative integers and a number s. Find a longest contiguous subarray in arr that has a sum equal to s. Return two integers that represent its inclusive bounds. If there are several possible answers, return the one with the smallest left bound. If there are no answers, return [-1].
# 
# Your answer should be 1-based, meaning that the first position of the array is 1 instead of 0.
# 
# Example
# 
# For s = 12 and arr = [1, 2, 3, 7, 5], the output should be
# findLongestSubarrayBySum(s, arr) = [2, 4].
# 
# The sum of elements from the 2nd position to the 4th position (1-based) is equal to 12: 2 + 3 + 7.
# 
# For s = 15 and arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], the output should be
# findLongestSubarrayBySum(s, arr) = [1, 5].
# 
# The sum of elements from the 1st position to the 5th position (1-based) is equal to 15: 1 + 2 + 3 + 4 + 5.
# 
# For s = 15 and arr = [1, 2, 3, 4, 5, 0, 0, 0, 6, 7, 8, 9, 10], the output should be
# findLongestSubarrayBySum(s, arr) = [1, 8].
# 
# The sum of elements from the 1st position to the 8th position (1-based) is equal to 15: 1 + 2 + 3 + 4 + 5 + 0 + 0 + 0.
# 
# Input/Output
# 
# [execution time limit] 5 seconds (r)
# 
# [input] integer s
# 
# The sum of the subarray that you are searching for.
# 
# Guaranteed constraints:
#   0 ≤ s ≤ 109.
# 
# [input] array.integer arr
# 
# The given array.
# 
# Guaranteed constraints:
#   1 ≤ arr.length ≤ 105,
# 0 ≤ arr[i] ≤ 104.
# 
# [output] array.integer
# 
# An array that contains two elements that represent the left and right bounds of the subarray, respectively (1-based). If there is no such subarray, return [-1].
# 
# 
# arr = list(1, 2, 3, 4, 5, 0, 0, 0, 6, 7, 8, 9, 10)
# s = 15

# arr = list(1, 2, 3, 7, 5)
# s = 12


# arr = list(1, 0, 2)
# s = 0 
#really good reference: https://www.youtube.com/watch?v=HbbYPQc-Oo4
#entire operation done in 1 iteration.

#zero sum case is interesting. 
# arr = list(-2,2,-1,1,0,0)
# s = 0 

#todo: zero case: special stitching of adjacent groups. Mmmm dont think its required. 

findLongestSubarrayBySum <- function(s, arr) {
    arr <- as.vector(unlist(arr))
    cumulativesumarray = rep(NA,length(arr));
    maxlength = 0;
    startindex = rep(NA,length(arr))
    endindex = rep(NA,length(arr))
    index2 = 1;
    if (sum(arr) == s) {
      return(list(1,length(arr)))
    }
    
    for (index in 1:length(arr)) {
      if (index > 1) {
         cumulativesumarray[index] =  cumulativesumarray[index -1] + arr[index]        
      } else {
         cumulativesumarray[index] = arr[index]
      }

      if (cumulativesumarray[index] == s) {
        #check if first valid entry is not required coz we are doing cumsum. its
        #always from first index.
        # if 1st valid entry then startindex = 1 if
        #(length(!is.na(startindex)) == 0) {
        startindex[index2] = 1; 
        endindex[index2] =  index;
        index2 = index2 + 1;
        # }
      }
      
      if (arr[index] == s) {
        startindex[index2] = index; 
        endindex[index2] =  index;    
        index2 = index2 + 1;
      }
      
      if (((cumulativesumarray[index] - s) %in% cumulativesumarray) && (s != 0)) {
        startindexitn = which(cumulativesumarray == (cumulativesumarray[index] - s)) + 1
        startindex[index2] = startindexitn
        endindex[index2] = index
        index2 = index2 + 1;
      }
    }
      startindex <- startindex[!is.na(startindex)]
      endindex <- endindex[!is.na(endindex)]
    #special case for zero sum

    if (length(startindex) == 0) {
      return(list(-1))
    } else {
      vdiff = endindex - startindex
      allmaxlengths = which(vdiff == max(vdiff))
      if (length(allmaxlengths) > 1) {
        finalindex = which.min(startindex[allmaxlengths])
        return(list(startindex[finalindex],endindex[finalindex]))
      } else {
        return(list(startindex[allmaxlengths],endindex[allmaxlengths]))
      }
    }
}

#initilize vectors of specific size rather than appending iteratively to empty sized vectors
# findLongestSubarrayBySum <- function(s, arr) {
#   #find subarrays which have the given sum 
#   #find length of each. Return the one with max length. If multiple with same length then return the one with smallest start index (left bound)
#   arr <- as.vector(unlist(arr))
#   
#   #find subarrays with max length
#   startindex = rep(NA,length(arr))
#   endindex = rep(NA,length(arr))
#   index2 = 1;
#   for (index in 1:length(arr)) {
#     subarraysumforindex = cumsum(arr[index:length(arr)])
#     if (sum(subarraysumforindex == s) > 0) {
#       startindex[index2] = index;
#       currentendindex = (index + max(which(subarraysumforindex == s)) - 1) 
#       endindex[index2] = currentendindex;
#       index2 <- index2 + 1;
#     }
#   }
#   startindex <- startindex[!is.na(startindex)]
#   endindex <- endindex[!is.na(endindex)]  
#   if (length(startindex) == 0) {
#     return(list(-1))
#   } else {
#     vdiff = endindex - startindex
#     allmaxlengths = which(vdiff == max(vdiff))
#     if (length(allmaxlengths) > 1) {
#       finalindex = which.min(startindex[allmaxlengths])
#       return(list(startindex[finalindex],endindex[finalindex]))
#     } else {
#       return(list(startindex[allmaxlengths],endindex[allmaxlengths]))
#     }
#   }
# }


#faster: still failing the last test
# arr = list(1, 2, 3, 7, 5)
# s = 12
# findLongestSubarrayBySum <- function(s, arr) {
#   #find subarrays which have the given sum 
#   #find length of each. Return the one with max length. If multiple with same length then return the one with smallest start index (left bound)
#   arr <- as.vector(unlist(arr))
#   
#   if (sum(arr) == s) {
#     return(list(1,length(arr)))
#   }
#   
#   #find subarrays with max length
#   startindex = c()
#   endindex = c()
#   for (index in 1:length(arr)) {
#     subarraysumforindex = cumsum(arr[index:length(arr)])
#     if (sum(subarraysumforindex == s) > 0) {
#       startindex = c(startindex,index);
#       currentendindex = (index + max(which(subarraysumforindex == s)) - 1) 
#       
#       #we know this would be the longest subarray so we can safely return from here
#       if (currentendindex == (length(arr) - 1)) {
#         return(list(index,currentendindex))
#       }
#       endindex = c(endindex,currentendindex)
#     }
#   }
#   if (length(startindex) == 0) {
#     return(list(-1))
#   } else {
#     vdiff = endindex - startindex
#     allmaxlengths = which(vdiff == max(vdiff))
#     if (length(allmaxlengths) > 1) {
#       finalindex = which.min(startindex[allmaxlengths])
#       return(list(startindex[finalindex],endindex[finalindex]))
#     } else {
#       return(list(startindex[allmaxlengths],endindex[allmaxlengths]))
#     }
#   }
# }

#logic1: fails the last test for time :(
# findLongestSubarrayBySum <- function(s, arr) {
#   #find subarrays which have the given sum 
#   #find length of each. Return the one with max length. If multiple with same length then return the one with smallest start index (left bound)
#   arr <- as.vector(unlist(arr))
# 
#   #find subarrays with max length
#   startindex = c()
#   endindex = c()
#   for (index in 1:length(arr)) {
#     subarraysumforindex = cumsum(arr[index:length(arr)])
#     if (sum(subarraysumforindex == s) > 0) {
#       startindex = c(startindex,index);
#       currentendindex = (index + max(which(subarraysumforindex == s)) - 1) 
#       endindex = c(endindex,currentendindex)
#     }
#   }
#   if (length(startindex) == 0) {
#     return(list(-1))
#   } else {
#     vdiff = endindex - startindex
#     allmaxlengths = which(vdiff == max(vdiff))
#     if (length(allmaxlengths) > 1) {
#       finalindex = which.min(startindex[allmaxlengths])
#       return(list(startindex[finalindex],endindex[finalindex]))
#     } else {
#       return(list(startindex[allmaxlengths],endindex[allmaxlengths]))
#     }
#   }
# }
