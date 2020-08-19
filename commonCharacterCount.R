# Given two strings, find the number of common characters between them.
# 
# Example
# 
# For s1 = "aabcc" and s2 = "adcaa", the output should be
# commonCharacterCount(s1, s2) = 3.
# 
# Strings have 3 common characters - 2 "a"s and 1 "c".
# 
# Input/Output
# 
# [execution time limit] 5 seconds (r)
# 
# [input] string s1
# 
# A string consisting of lowercase English letters.
# 
# Guaranteed constraints:
#   1 ≤ s1.length < 15.
# 
# [input] string s2
# 
# A string consisting of lowercase English letters.
# 
# Guaranteed constraints:
#   1 ≤ s2.length < 15.
# 
# [output] integer

# s1 = "aabcc" 
# s2 = "adcaa"

#todo: make a more elegant solution
library(data.table)
commonCharacterCount <- function(s1, s2) {
  
  AllChar_s1 <- as.list(unlist(strsplit(s1,split = "")))
  CharFreq_s1 <-  lapply(unique(AllChar_s1), function(x){return(sum(AllChar_s1 == x))} )
  CharFreq_s1 <- as.data.table(unlist(CharFreq_s1))
  names(CharFreq_s1) <- "freq_s1"
  CharFreq_s1[,Char := unlist(unique(AllChar_s1))]

  
  AllChar_s2 <- as.list(unlist(strsplit(s2,split = "")))
  CharFreq_s2 <-  lapply(unique(AllChar_s2), function(x){return(sum(AllChar_s2 == x))} )
  CharFreq_s2 <- as.data.table(unlist(CharFreq_s2))
  names(CharFreq_s2) <- "freq_s2"
  CharFreq_s2[,Char := unlist(unique(AllChar_s2))]
  
  #combine and take minima
  dtTemp <- merge(CharFreq_s1,CharFreq_s2,by.x = "Char",by.y = "Char")
  commonCharCount <- apply(dtTemp[,2:3], 1,min)
  return(sum(commonCharCount))
}
