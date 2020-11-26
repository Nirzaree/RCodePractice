# Given a string, return its encoding defined as follows:
#
# First, the string is divided into the least possible number of disjoint
# substrings consisting of identical characters for example, "aabbbc" is divided
# into ["aa", "bbb", "c"] Next, each substring with length greater than one is
# replaced with a concatenation of its length and the repeating character for
# example, substring "bbb" is replaced by "3b" Finally, all the new strings are
# concatenated together in the same order and a new string is returned.
#
# Example
#
# For s = "aabbbc", the output should be lineEncoding(s) = "2a3bc".
#
# Input/Output
#
# [execution time limit] 5 seconds (r)
#
# [input] string s
#
# String consisting of lowercase English letters.
#
# Guaranteed constraints: 4 ≤ s.length ≤ 15.
#
# [output] string
#
# Encoded version of s.

s = "bbjaadlkjdl"

s = "abbcabb"

lineEncoding <- function(s) {
  s2 = strsplit(s,"")[[1]]
  if (uniqueN(s2) == length(s2)) {
    return(s)
  }
  outputString = c()
  digit = 1;
  for (ind in 1:(length(s2) - 1)) {
    if (s2[ind+1] != s2[ind]) {
      if (digit > 1) {
        outputString <- c(outputString,digit,s2[ind])  
      } else {
        outputString <- c(outputString,s2[ind])          
      }
      if (ind == (length(s2) - 1)) {
        outputString <- c(outputString,s2[ind+1])  
      }
      digit = 1;
    } else {
      digit = digit + 1;
      if (ind == (length(s2) - 1)) {
          outputString <- c(outputString,digit,s2[ind])  
      }
    }
  }
  return(paste(outputString,collapse = ""))
}

