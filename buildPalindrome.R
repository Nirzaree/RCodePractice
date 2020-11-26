# Given a string, find the shortest possible string which can be achieved by
# adding characters to the end of initial string to make it a palindrome.
#
# Example
#
# For st = "abcdc", the output should be buildPalindrome(st) = "abcdcba".
#
# Input/Output
#
# [execution time limit] 5 seconds (r)
#
# [input] string st
#
# A string consisting of lowercase English letters.
#
# Guaranteed constraints: 3 ≤ st.length ≤ 10.
#
# [output] string

# st = "abcdc" #soln: "abcdcba"
# st = "ababab" #soln: "abababa"
# st = "abba" #soln: "abba"
# st = "abaa" #soln: "abaaba"

#Tests: 
#1. st = "abba" #Already palindrome 
#2. st = "ababab" #Add 1 digit to right + string is even length 
#3. st = "bcba" #Add 1 to left + string is odd length 
#4. st = "baccdbdcc" #Add 2 to right + string is odd length 
# 5. st = "ccdbdccba" #Add 2 to left + string is odd length

#6. st = "abbccc"  #3 digits to left + string is even: had to add stringreversal
#for this case. good test

#7. st = "cccbba"  #3 digits to right + string is even : had to add string reversal 
#here as well. 

#now checking all fo the above if it is still fine. looks good. submitting.


#todo: paste vs paste0. Slight issue. 

library(stringi)

buildPalindrome <- function(st) {
  #check if palindrome
  checkPalindrome <- function(string) {
    if (nchar(string) == 1) {
      return(TRUE)
    }
    
    #if string is odd length, then remove mid char and then check
    if (nchar(string) %% 2 != 0) {
      string = paste0(substr(string,1,floor(nchar(string)/2)),
                      substr(string,ceiling(nchar(string)/2) + 1,nchar(string))) #paste with collapse "" not working
    }
    
    string2 = strsplit(string,"")[[1]]
    
    #now check 
    count = 0;
    for (i in seq(1:(nchar(string)/2))) {
      if (string2[i] == string2[nchar(string) - i + 1]) {
        count = count + 1;
      }
    }
    if (count == (nchar(string)/2)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  if (checkPalindrome(st)) {
    return(st)
  }  
  #if not a palindrome, we have to make one. 
  #find suitable midpoint starting from the midpoint of the string. 
  #i) either duplicating characters or 
  # ii) duplicate characters sandwiched between a character 
  
  #here string is even, hence midpoint is 2 char. but when we will add
  #1 char, it will be odd so then midpoint will be 1 charboth. 
  
  st2 = strsplit(st,"")[[1]]
  for (digitstoadd in 1:(nchar(st) - 1)) {
    #move from actual midpoint by digitstoadd amount and see if there is mirroring in 
    # remaining (n - digitstoadd) characters. if there is, then add digitstoadd bits on the
    #required side and then return
    
    #try digitstoadd on both sides of the midpoint. 
    #remove digitstoadd from both sides, one by one and see if its a palindrome. 
    #remove from head
    new_st = st2[(digitstoadd + 1): (length(st2))]
    if (checkPalindrome(paste(new_st,collapse = ""))) { #paste0 not working here. 
      #add required digits and return
      new_st2 = paste0(st,
                       stri_reverse(paste(st2[1:digitstoadd],collapse = "")))
      return(new_st2)
    } else {
      new_st = st2[1: (length(st2) - (digitstoadd))]
      if (checkPalindrome(paste(new_st,collapse = ""))) { #paste0 not working here. 
        #add required digits and return
        new_st2 = paste0(stri_reverse(paste(st2[((length(st2) - digitstoadd) + 1):length(st2)],
                                            collapse = "")),
                         st)
        return(new_st2)
      }
    }
  }
}

#Update: 
#Digits can only be added to the end according to the codesignal problem requirement, 
#so we update the code to only have partial logic 

library(stringi)

buildPalindrome <- function(st) {
  #check if palindrome
  checkPalindrome <- function(string) {
    if (nchar(string) == 1) {
      return(TRUE)
    }
    
    #if string is odd length, then remove mid char and then check
    if (nchar(string) %% 2 != 0) {
      string = paste0(substr(string,1,floor(nchar(string)/2)),
                      substr(string,ceiling(nchar(string)/2) + 1,nchar(string))) #paste with collapse "" not working
    }
    
    string2 = strsplit(string,"")[[1]]
    
    #now check 
    count = 0;
    for (i in seq(1:(nchar(string)/2))) {
      if (string2[i] == string2[nchar(string) - i + 1]) {
        count = count + 1;
      }
    }
    if (count == (nchar(string)/2)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  if (checkPalindrome(st)) {
    return(st)
  }  
  #if not a palindrome, we have to make one. 
  #find suitable midpoint starting from the midpoint of the string. 
  #i) either duplicating characters or 
  # ii) duplicate characters sandwiched between a character 
  
  #here string is even, hence midpoint is 2 char. but when we will add
  #1 char, it will be odd so then midpoint will be 1 charboth. 
  
  st2 = strsplit(st,"")[[1]]
  for (digitstoadd in 1:(nchar(st) - 1)) {
    #move from actual midpoint by digitstoadd amount and see if there is mirroring in 
    # remaining (n - digitstoadd) characters. if there is, then add digitstoadd bits on the
    #required side and then return
    
    #try digitstoadd on both sides of the midpoint. 
    #remove digitstoadd from both sides, one by one and see if its a palindrome. 
    #remove from head
    new_st = st2[(digitstoadd + 1): (length(st2))]
    if (checkPalindrome(paste(new_st,collapse = ""))) { #paste0 not working here. 
      #add required digits and return
      new_st2 = paste0(st,
                       stri_reverse(paste(st2[1:digitstoadd],collapse = "")))
      return(new_st2)
    } 
  }
}

