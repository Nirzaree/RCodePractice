# You are given an array of desired filenames in the order of their creation.
# Since two files cannot have equal names, the one which comes later will have
# an addition to its name in a form of (k), where k is the smallest positive
# integer such that the obtained name is not used yet.
#
# Return an array of names that will be given to the files.
#
# Example
#
# For names = ["doc", "doc", "image", "doc(1)", "doc"], the output should be
# fileNaming(names) = ["doc", "doc(1)", "image", "doc(1)(1)", "doc(2)"].
#
# Input/Output
#
# [execution time limit] 5 seconds (r)
#
# [input] array.string names
#
# Guaranteed constraints: 5 ≤ names.length ≤ 1000, 1 ≤ names[i].length ≤ 15.
#
# [output] array.string
#
# 

# names = list("doc", "doc", "image", "doc(1)", "doc")
# names = list("dd","dd(1)","dd(2)","dd","dd(1)","dd(1)(2)","dd(1)(1)","dd","dd(1)")
# names = list("dd","dd(1)","dd(2)","dd","dd(1)","dd(1)(2)","dd(1)(1)","dd","dd(5)")

fileNaming <- function(names) {
  if (length(names) <= 1) {
    return(names)
  }
  namessofar <- names[1]
  for (ind in 2:length(names)) {
    if (names[[ind]] %in% namessofar) {
      suggestednewname <- paste0(names[[ind]],"(1)")
      while (suggestednewname %in% namessofar) {
        #get last braces and increment the int.
        allparantheses <- gregexpr("[(]",suggestednewname)[[1]]
        #find the last one
        reqstartindex <- max(allparantheses)
        reqendindex <- max(gregexpr("[)]",suggestednewname)[[1]])
        
        currentrepindex <- substr(suggestednewname,
                                  reqstartindex + 1,
                                  reqendindex - 1)
        suggestednewname <- paste0(substr(suggestednewname,1,reqstartindex),
                                   as.numeric(currentrepindex) + 1,
                                   ")")
      }
      namessofar[[ind]] <- suggestednewname
      next
    }
    namessofar[[ind]] <- names[[ind]]
  }
  return(namessofar)
}
