# Correct variable names consist only of English letters, digits and underscores and they can't start with a digit.
# 
# Check if the given string is a correct variable name.
# 
# Example
# 
#     For name = "var_1__Int", the output should be
#     variableName(name) = true;
#     For name = "qq-q", the output should be
#     variableName(name) = false;
#     For name = "2w2", the output should be
#     variableName(name) = false.
# 
# Input/Output
# 
#     [execution time limit] 5 seconds (r)
# 
#     [input] string name
# 
#     Guaranteed constraints:
#     1 ≤ name.length ≤ 10.
# 
#     [output] boolean
# 
#     true if name is a correct variable name, false otherwise.
# 

  variableName <- function(name) {
    # if first digit is not alphabet or an underscore, then false
    if (!(grepl("^[A-Za-z_]",substr(name,1,1)))) {
      return(FALSE)
    } else if (sum(grepl("^[A-Za-z0-9_]",strsplit(name,"")[[1]])) == nchar(name)) {
      return(TRUE) 
    } else {
      return(FALSE)
    }
  }
