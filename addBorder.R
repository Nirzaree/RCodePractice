# Given a rectangular matrix of characters, add a border of asterisks(*) to it.
# 
# Example
# 
# For
# 
# picture = ["abc",
#            "ded"]
# 
# the output should be
# 
# addBorder(picture) = ["*****",
#                       "*abc*",
#                       "*ded*",
#                       "*****"]
# picture = list("abc",
#            "ded")
addBorder <- function(picture) {
  pictureLength <- nchar(picture[[1]]) 
  pictureWidth <- length(picture)
  
  #adding a border on each size would mean adding each of the dimensions by 2 extra pixels
  OutputPicture <- list()
  OutputPicture[[1]] <- paste(replicate(pictureLength + 2,"*"),collapse = "")
  
  MidData <- lapply(picture, function(x) {
    return(paste0("*",x,"*"))
  })
  OutputPicture <- c(OutputPicture,MidData)
  OutputPicture[[pictureWidth + 2]] <- paste(replicate(pictureLength + 2,"*"),collapse = "")
  return(OutputPicture)
}
