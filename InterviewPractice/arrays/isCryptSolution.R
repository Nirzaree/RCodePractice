# A cryptarithm is a mathematical puzzle for which the goal is to find the correspondence between letters and digits, such that the given arithmetic equation consisting of letters holds true when the letters are converted to digits.
# 
# You have an array of strings crypt, the cryptarithm, and an an array containing the mapping of letters and digits, solution. The array crypt will contain three non-empty strings that follow the structure: list(word1, word2, word3), which should be interpreted as the word1 + word2 = word3 cryptarithm.
# 
# If crypt, when it is decoded by replacing all of the letters in the cryptarithm with digits using the mapping in solution, becomes a valid arithmetic equation containing no numbers with leading zeroes, the answer is true. If it does not become a valid arithmetic solution, the answer is false.
# 
# Note that number 0 doesn't contain leading zeroes (while for example 00 or 0123 do).

# crypt = list("SEND", 
#    "MORE", 
#    "MONEY")
# solution = list(list("O","0"), 
#    list("M","1"), 
#    list("Y","2"), 
#    list("E","5"), 
#    list("N","6"), 
#    list("D","7"), 
#    list("R","8"), 
#    list("S","9"))


# crypt = list("TEN", 
#              "TWO", 
#              "ONE")
# solution =  list(list("O","1"), 
#                  list("T","0"), 
#                  list("W","9"), 
#                  list("E","5"), 
#                  list("N","4"))

# crypt = list("A", 
#              "A", 
#              "A")
# solution = list(list("A","0"))

isCryptSolution <- function(crypt, solution) {
  word1 <- crypt[[1]]
  word2 <- crypt[[2]]
  word3 <- crypt[[3]]
  
  solution <- rbindlist(solution)
  names(solution) <- c("alphabet","nummapping")
  
  word1mapping <-   lapply(strsplit(word1,"")[[1]],function(x) {
    return(solution[alphabet == x,nummapping])
  })
  
  if (word1mapping[[1]] == "0" && length(word1mapping) > 1) {
    return(FALSE)
  }
  
  word1mapping <- as.numeric(paste(rbind(word1mapping),collapse = ""))
  
  word2mapping <-   lapply(strsplit(word2,"")[[1]],function(x) {
    return(solution[alphabet == x,nummapping])
  })
  
  if (word2mapping[[1]] == "0" && length(word2mapping) > 1) {
    return(FALSE)
  }
  
  word2mapping <- as.numeric(paste(rbind(word2mapping),collapse = ""))
  
  word3mapping <-   lapply(strsplit(word3,"")[[1]],function(x) {
    return(solution[alphabet == x,nummapping])
  })
  
  
  if (word3mapping[[1]] == "0" && length(word3mapping) > 1) {
    return(FALSE)
  }
  
  word3mapping <- as.numeric(paste(rbind(word3mapping),collapse = ""))

  
  if  (word1mapping + word2mapping == word3mapping) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
