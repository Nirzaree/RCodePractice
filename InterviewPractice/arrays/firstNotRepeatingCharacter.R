# Given a string s consisting of small English letters, find and return the first instance of a non-repeating character in it. If there is no such character, return '_'.
# 
# Example
# 
# For s = "abacabad", the output should be
# firstNotRepeatingCharacter(s) = 'c'.
# 
# There are 2 non-repeating characters in the string: 'c' and 'd'. Return c since it appears in the string first.
# 
# For s = "abacabaabacaba", the output should be
# firstNotRepeatingCharacter(s) = '_'.
# 
# There are no characters in this string that do not repeat.
# 
# Input/Output
# 
# [execution time limit] 5 seconds (r)
# 
# [input] string s
# 
# A string that contains only lowercase English letters.
# 
# Guaranteed constraints:
#   1 ≤ s.length ≤ 105.
# 
# [output] char
# 
# The first non-repeating character in s, or '_' if there are no characters that do not repeat.

firstNotRepeatingCharacter <- function(s) {
  s = as.vector(unlist(strsplit(s,"")))
  s = strsplit(s,"")
  array1 = s[duplicated(s)]
  array2 = s[!duplicated(s)]
  nonrepeatingchar = array2[!(array2 %in% array1)]
  if (length(nonrepeatingchar) > 0) {
    positions =  lapply(nonrepeatingchar, function(x) {return(which(s == x))})
    return(unlist(s[min(unlist(positions))])) #top unlist to only match expected output type.
  } else {
    return("_")
  }
}

