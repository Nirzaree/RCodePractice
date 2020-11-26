# Define a word as a sequence of consecutive English letters. Find the longest
# word from the given string.
#
# Example
#
# For text = "Ready, steady, go!", the output should be longestWord(text) =
# "steady".
#
# Input/Output
#
# [execution time limit] 5 seconds (r)
#
# [input] string text
#
# Guaranteed constraints: 4 ≤ text.length ≤ 50.
#
# [output] string
#
# The longest word from text. It's guaranteed that there is a unique output.
text = "You are the best!!!!!!!!!!!! CodeFighter ever!"
text = "ABCd"
text = "You are the best!!!!!!!!!!!! CodeFighterever!"
text = "small example!!"
text = "small-123-abs-gghdfgr$"
text = "smallabs-gg$hdfgr$"
text = "A!! AA[]z"

#need to learn  regex better: todo: 
#elegent solution
longestWord <- function(text) {
  allwords <- strsplit(text,"[^A-Za-z]")[[1]]
  alllengths <- lapply(allwords,nchar)
  allwords[which.max(unlist(alllengths))]
  
  return(allwords)
}

#steps:
#1. Get positions of all alphabets
#2. Find max contiguous block

longestWord <- function(text) {
  allalphabetspos = gregexpr("[A-Za-z]",text)[[1]]

  if (length(allalphabetspos) == nchar(text)) {
    return(text)
  }
  
  start_index = c()
  end_index = c()
  diffalphabets = c(1,diff(allalphabetspos))
  startpos = 1;
  endpos = 1;
  
  for (ind in 1:(length(diffalphabets))) {
    if ((diffalphabets[ind+1] == 1) & (ind < (length(diffalphabets) - 1))) {
        endpos = endpos + 1;
    } else {
      if (ind == (length(diffalphabets) - 1)) {
        if (diffalphabets[ind+1] == 1) {
          endpos = endpos + 1;
        } 
      }
      start_index = c(start_index,startpos)
      end_index = c(end_index,endpos)
      startpos = (ind + 1);
      endpos = (ind + 1);
    }
  }
  
  maxlengthwordpos = which.max(end_index - start_index)
  
  maxwordloc = allalphabetspos[start_index[maxlengthwordpos]:end_index[maxlengthwordpos]]
  
  return(substr(text,min(maxwordloc),
                max(maxwordloc)))
}

# longestWord <- function(text) {
#   allalphabetpos = gregexpr("[A-Za-z]",text)[[1]]
#   if (length(allalphabetpos) == nchar(text)) {
#     return(text)
#   }
#   diffalphabets = diff(allalphabetpos)
#   
#   #get max diff 
#   startpos = 1;
#   endpos = 1;
#   startarray <- c()
#   endarray <- c()
#   for (ind in 1:length(diffalphabets)) {
#     if ((diffalphabets[ind] == 1) & (ind < (length(diffalphabets)))) {
#       endpos = endpos + 1;
#     } else {
#       startarray = c(startarray,startpos);
#       if (ind == length(diffalphabets)) {
#         endpos = endpos + 1;
#       }
#       endarray = c(endarray,endpos);
#       startpos = ind;
#       endpos = startpos;
#     }
#   }
#   #get position of the longest word
#   maxlength = which.max(endarray - startarray)
#   longestwordloc = allalphabetpos[(startarray[maxlength] + 1):endarray[maxlength]]
#   return(substr(text,
#                 min(longestwordloc),
#                 max(longestwordloc) + 1))
# }
