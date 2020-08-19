# Write a function that reverses characters in (possibly nested) parentheses in the input string.
# 
# Input strings will always be well-formed with matching ()s.
# 
# Example
# 
# For inputString = "(bar)", the output should be
# reverseInParentheses(inputString) = "rab";
# For inputString = "foo(bar)baz", the output should be
# reverseInParentheses(inputString) = "foorabbaz";
# For inputString = "foo(bar)baz(blim)", the output should be
# reverseInParentheses(inputString) = "foorabbazmilb";
# For inputString = "foo(bar(baz))blim", the output should be
# reverseInParentheses(inputString) = "foobazrabblim".
# Because "foo(bar(baz))blim" becomes "foo(barzab)blim" and then "foobazrabblim".
# 
# Input/Output
# 
# [execution time limit] 5 seconds (r)
# 
# [input] string inputString
# 
# A string consisting of lowercase English letters and the characters ( and ). It is guaranteed that all parentheses in inputString form a regular bracket sequence.
# 
# Guaranteed constraints:
#   0 ≤ inputString.length ≤ 50.
# 
# [output] string
# Return inputString, with all the characters that were in parentheses reversed.

library(data.table)
library(stringr)
# inputString = "(bar)"
# inputString = "foo(bar)baz(blim)"
# inputString = "foo(bar(baz))blim"

#in nested cases, rev the order of endpositions
# in non nested cases, life is good.
reverseInParentheses <- function(inputString) {
 getAllParenthesesPos <- as.data.table(str_locate_all(inputString,"\\("))
 if (nrow(getAllParenthesesPos) == 0) {
    return(inputString)
 }
 getAllParenthesesPos[,ParenthesesBool := 1]
 getAllParenthesesPos <- rbind(getAllParenthesesPos,as.data.table(str_locate_all(inputString,"\\)")),fill = TRUE)
 getAllParenthesesPos[is.na(ParenthesesBool), ParenthesesBool := -1]
 getAllParenthesesPos[,end := NULL]
 getAllParenthesesPos <- getAllParenthesesPos[order(start),]
 outputString <- inputString
 outputString <- unlist(strsplit(outputString,""))
 #to to innermost or last 
 
 for (ind in seq(1: nrow(as.data.table(str_locate_all(inputString,"\\("))))) {
   #extract part of string to be reversed
   
   #go to last input parenthesis
   LastOpeningParenthesisPosition <- getAllParenthesesPos[ParenthesesBool == 1,max(start)]
   #next row position is corresponding end
   CorrespondingEndParenthesisPosition <- getAllParenthesesPos[(which(getAllParenthesesPos[,start] == getAllParenthesesPos[ParenthesesBool == 1,max(start)]) + 1),start]
   #do the reversal
   outputString[(LastOpeningParenthesisPosition + 1):(CorrespondingEndParenthesisPosition - 1)] <- rev(outputString[(LastOpeningParenthesisPosition + 1):(CorrespondingEndParenthesisPosition - 1)])
   #remove parenthesis that were catered to: 
   outputString <- outputString[-c(LastOpeningParenthesisPosition,CorrespondingEndParenthesisPosition)]
   #now the old positions done make sense as the parentheses catered to are removed 
   #recalc the position of remaining braces
   if (ind < nrow(as.data.table(str_locate_all(inputString,"\\(")))) {
      outputString2 <- paste(outputString,collapse = "")
      getAllParenthesesPos <- as.data.table(str_locate_all(outputString2,"\\("))
      getAllParenthesesPos[,ParenthesesBool := 1]
      getAllParenthesesPos <- rbind(getAllParenthesesPos,as.data.table(str_locate_all(outputString2,"\\)")),fill = TRUE)
      getAllParenthesesPos[is.na(ParenthesesBool), ParenthesesBool := -1]
      getAllParenthesesPos[,end := NULL]
      getAllParenthesesPos <- getAllParenthesesPos[order(start),]
   }

 }
 finalOutput <- paste(outputString,collapse = "")
 return(finalOutput)
}

#   getAllStartParenthesisPos <- as.data.table(str_locate_all(inputString,"\\("))
# getAllStartParenthesisPos[,end := NULL]
# getAllStartParenthesisPos[,ParenthesisBool := 1]
# # names(getAllStartParenthesisPos) <- c("StartParIndex","Level")
# 
# getAllEndParenthesisPos <- as.data.table(str_locate_all(inputString,"\\)"))
# # getAllEndParenthesisPos[,pos := seq(from = nrow(getAllEndParenthesisPos),to = 1)]
# # getAllEndParenthesisPos[,pos := seq(from = 1,to = nrow(getAllEndParenthesisPos))]
# getAllEndParenthesisPos[,end := NULL]
# # names(getAllEndParenthesisPos) <- c("EndParIndex","Level")
# getAllEndParenthesisPos[,ParenthesisBool := -1]
# #match parenthesis: 
# MatchingStartNEndParenthesisPos <-rbind(getAllStartParenthesisPos,getAllEndParenthesisPos)
# MatchingStartNEndParenthesisPos <- MatchingStartNEndParenthesisPos[order(start),]
# MatchingStartNEndParenthesisPos[,rowindex := seq(1:nrow(MatchingStartNEndParenthesisPos))]
# outputStr <- inputString
# outputStr <- strsplit(outputStr,"")
# for (ind in 1:nrow(as.data.table(str_locate_all(inputString,"\\(")))) {
#   #go to innermost start parenthesis
#   StrToInvStartParanthesis <- MatchingStartNEndParenthesisPos[ParenthesisBool == 1,max(start)]
#   StrToInvEndParanthesis <- MatchingStartNEndParenthesisPos[MatchingStartNEndParenthesisPos[,which.max(start)] + 1, start]
#   
#   outputStr[[1]][(StrToInvStartParanthesis + 1):(StrToInvEndParanthesis - 1)] <- rev(outputStr[[1]][(StrToInvStartParanthesis + 1):(StrToInvEndParanthesis - 1)])
#   #remove parentheses
#   # outputStr[[1]] <- outputStr[[1]][-c(StrToInvStartParanthesis,StrToInvEndParanthesis)]
#   
#   #remove parenthesis from the table as well
#   MatchingStartNEndParenthesisPos <- MatchingStartNEndParenthesisPos[-(MatchingStartNEndParenthesisPos[ParenthesisBool == 1,which.max(start)]:MatchingStartNEndParenthesisPos[ParenthesisBool == 1,which.max(start) + 1]),]
# }
# outputStr <- paste(unlist(outputStr),collapse = "")
# return(outputStr)
