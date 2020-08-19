library(stringr)
library(stringi)
checkPalindrome <- function(inputString) {
  if (str_sub(inputString,1,floor(nchar(inputString)/2)) == stringi::stri_reverse(str_sub(inputString,ceiling(nchar(inputString)/2)  + 1,nchar(inputString)))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
