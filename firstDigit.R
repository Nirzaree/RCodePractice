# Find the leftmost digit that occurs in a given string.
# 
# Example
# 
# For inputString = "var_1__Int", the output should be
# firstDigit(inputString) = '1';
# For inputString = "q2q-q", the output should be
# firstDigit(inputString) = '2';
# For inputString = "0ss", the output should be
# firstDigit(inputString) = '0'.
# 
# Input/Output
# 
# [execution time limit] 5 seconds (r)
# 
# [input] string inputString
# 
# A string containing at least one digit.
# 
# Guaranteed constraints:
#   3 ≤ inputString.length ≤ 10.
# 
# [output] char

# inputString = "var_1__Int"

firstDigit <- function(inputString) {
  #todo: search till first digit is found (not the whole string). return that digit.
  posoffirstdigit = gregexpr("[0-9]",inputString)[[1]][1]
  return(substr(inputString,posoffirstdigit,posoffirstdigit))
}
