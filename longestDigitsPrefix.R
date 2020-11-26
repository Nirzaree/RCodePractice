# Given a string, output its longest prefix which contains only digits.
# 
# Example
# 
# For inputString = "123aa1", the output should be
# longestDigitsPrefix(inputString) = "123".
# 
# Input/Output
# 
# [execution time limit] 5 seconds (r)
# 
# [input] string inputString
# 
# Guaranteed constraints:
#   3 ≤ inputString.length ≤ 100.
# 
# [output] string

# inputString = "123aa1"
# inputString = "  3) always check for whitespaces"
# inputString = "12abc34"
# inputString = "0123456789"
# inputString = "1234a"
# inputString = "3t456tg7"
#todo: with regex. in 1 line. 
longestDigitsPrefix <- function(inputString) {
  digpos <- gregexpr("[0-9]",inputString)[[1]]
  
  if (length(digpos) == nchar(inputString)) {
    return(inputString)
  }
  if (1 %in% digpos) {
    diffarray = diff(digpos)
    if (all(diffarray == 1)) {
      return(substr(inputString,1,nchar(inputString)-1))
    }
    if (diffarray[1] != 1) {
      return(substr(inputString,1,1))
    }
    diffarray = diffarray[1:(min(which(diffarray != 1)) - 1)]
    endpos = (max(which(diffarray == 1))) + 1
    return(substr(inputString,1,endpos))
  } else {
    return("")
  }
}
