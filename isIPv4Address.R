# An IP address is a numerical label assigned to each device (e.g., computer, printer) participating in a computer network that uses the Internet Protocol for communication. There are two versions of the Internet protocol, and thus two versions of addresses. One of them is the IPv4 address.
# 
# Given a string, find out if it satisfies the IPv4 address naming rules.
# 
# Example
# 
# For inputString = "172.16.254.1", the output should be
# isIPv4Address(inputString) = true;
# 
# For inputString = "172.316.254.1", the output should be
# isIPv4Address(inputString) = false.
# 
# 316 is not in range [0, 255].
# 
# For inputString = ".254.255.0", the output should be
# isIPv4Address(inputString) = false.
# 
# There is no first number.
# 
# Input/Output
# 
# [execution time limit] 5 seconds (r)
# 
# [input] string inputString
# 
# A string consisting of digits, full stops and lowercase English letters.
# 
# Guaranteed constraints:
#   1 ≤ inputString.length ≤ 30.
# 
# [output] boolean
# true if inputString satisfies the IPv4 address naming rules, false otherwise.

#special caution: double zeros not allowed. only single zero.
library(stringr)
library(data.table)
isIPv4Address <- function(inputString) {
  #get all data between dots
  dotpos <- as.data.table(str_locate_all(inputString,"\\.")[[1]])
  dotpos[,end := NULL]
  # initial <- inputString
  # inputString <- strsplit(inputString,"")[[1]]
  if  (nrow(dotpos) == 3) {
    firstdigit <- substr(inputString,1,dotpos[1,start-1])
    if ((is.na(as.numeric(firstdigit))) | (nchar(firstdigit) > nchar(as.numeric(firstdigit)))) {
      return(FALSE)
    }
    firstdigit <- as.numeric(firstdigit)
    if (is.na(as.numeric(firstdigit)) | (firstdigit < 0) | (firstdigit > 255)) {
      return(FALSE)
    } 
    seconddigit <- substr(inputString,dotpos[1,start+1],dotpos[2,start-1])
    if ((is.na(as.numeric(seconddigit))) | (nchar(seconddigit) > nchar(as.numeric(seconddigit)))) {
      return(FALSE)
    }
    seconddigit <- as.numeric(seconddigit)
    if (is.na(seconddigit) | (seconddigit < 0) | (seconddigit > 255)) {
      return(FALSE)
    }
    thirddigit <- substr(inputString,dotpos[2,start+1],dotpos[3,start-1])
    if ((is.na(as.numeric(thirddigit))) | (nchar(thirddigit) > nchar(as.numeric(thirddigit)))) {
      return(FALSE)
    }
    thirddigit <- as.numeric(thirddigit)
    if (is.na(thirddigit) | (thirddigit < 0) | (thirddigit > 255)) {
      return(FALSE)
    }
    
    fourthdigit <- substr(inputString,dotpos[3,start+1],nchar(inputString))
    if ((is.na(as.numeric(fourthdigit))) | (nchar(fourthdigit) > nchar(as.numeric(fourthdigit)))) {
      return(FALSE)
    }
    fourthdigit <- as.numeric(fourthdigit)
    if (is.na(fourthdigit) | (fourthdigit < 0) | (fourthdigit > 255)) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else {
    return(FALSE)
  }
}

#not the most elegant code.
