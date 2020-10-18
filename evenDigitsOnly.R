# Check if all digits of the given integer are even.
# 
# Example
# 
# For n = 248622, the output should be
# evenDigitsOnly(n) = true;
# For n = 642386, the output should be
# evenDigitsOnly(n) = false.
# 
# Input/Output
# 
# [execution time limit] 5 seconds (r)
# 
# [input] integer n
# 
# Guaranteed constraints:
#   1 ≤ n ≤ 109.
# 
# [output] boolean
# 
# true if all digits of n are even, false otherwise.
# n = 248622
evenDigitsOnly <- function(n) {
  n <- as.character(n)
  n <- strsplit(n,"")
  n <- as.numeric(as.vector(unlist(n)))
  if (all((n %% 2) == 0)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

