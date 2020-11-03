# A media access control address (MAC address) is a unique identifier assigned to network interfaces for communications on the physical network segment.
# 
# The standard (IEEE 802) format for printing MAC-48 addresses in human-friendly form is six groups of two hexadecimal digits (0 to 9 or A to F), separated by hyphens (e.g. 01-23-45-67-89-AB).
# 
# Your task is to check by given string inputString whether it corresponds to MAC-48 address or not.
# 
# Example
# 
# For inputString = "00-1B-63-84-45-E6", the output should be
# isMAC48Address(inputString) = true;
# For inputString = "Z1-1B-63-84-45-E6", the output should be
# isMAC48Address(inputString) = false;
# For inputString = "not a MAC-48 address", the output should be
# isMAC48Address(inputString) = false.
# 
# Input/Output
# 
# [execution time limit] 5 seconds (r)
# 
# [input] string inputString
# 
# Guaranteed constraints:
#   15 ≤ inputString.length ≤ 20.
# 
# [output] boolean
# 
# true if inputString corresponds to MAC-48 address naming rules, false otherwise.

# 1. Length
# 2. Hyphens
# 3. All hex digits

library(stringr)
inputString = "00-1B-63-84-45-E6"

#getting stuck in hidden tests:

#trying some sensible test cases that I can think of 
# inputString = "                 "
#helped resolve a bug. length function for hyphen count's end braces were incorrect. 

#still failing last test
#trying some test case 
# inputString = "00- B-63-84-45-E6" #worked
# inputString = "00-aB-63-84-45-E6" #worked
# inputString = "00-AB-63-84-40-E " #worked
# inputString = "00-1B-63-84-45-E6X" #worked
# inputString = "L0-1B-63-84-45-E6" #worked

#unlocked the hidden tests for 20k coins :'(
# last test which was failing was this : "AB-CD-EF-12-34_56"
inputString = "AB-CD-EF-12-34_56"
#figured the issue: 
# it was with :pos_hyphens[[1]][,1] == c(3,6,9,12,15))
#I thought A ==B would equate to true only if all elements are equal
#however all(A == B) has to be explicitly added for that. :'(
#lost 20k coins for this!!!
#Also I thought the && would make the hwole thing false if even one thing fails, looks like not :(

#The issue was slightly more detailed: 
# The code line was: 
  # ((pos_hyphens[[1]][,1] == pos_hyphens[[1]][,2])  && (pos_hyphens[[1]][,1] == c(3,6,9,12,15)))
# For this example: pos_hyphens was of length 4 and all values were true
# The second half of the equation was of length 5 with first 4 values being true and last one 
#being false the operation gave value true 
# A = c(T,T,T,T)
# B = c(T,T,T,T,F)
# A && B

#Solution: just make sure individual equations are giving out 1 true/false value so this 
#length issues dont occur. 
#Adding all infront of both of these equations. phew. 

isMAC48Address <- function(inputString) {
  if (nchar(inputString) != 17) {
    return(FALSE)
  }
 #positions of hyphens
  pos_hyphens = str_locate_all(inputString,"-")
  if (length(pos_hyphens[[1]]) > 0) {
    if (all(pos_hyphens[[1]][,1] == pos_hyphens[[1]][,2])  && all(pos_hyphens[[1]][,1] == c(3,6,9,12,15))) {
      inputString <- strsplit(inputString,"")[[1]]
      remainingString <- inputString[-c(3,6,9,12,15)]
      if (all(grepl("[0-9A-F]",remainingString) == TRUE)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }

  
}
