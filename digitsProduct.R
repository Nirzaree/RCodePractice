# Given an integer product, find the smallest positive (i.e. greater than 0)
# integer the product of whose digits is equal to product. If there is no such
# integer, return -1 instead.
#
# Example
#
# For product = 12, the output should be digitsProduct(product) = 26; For
# product = 19, the output should be digitsProduct(product) = -1.
#
# Input/Output
#
# [execution time limit] 4 seconds (py3)
#
# [input] integer product
#
# Guaranteed constraints: 0 ≤ product ≤ 600.
#
# [output] integer

#attempt3: attempt2 worked but not very elegant solution.
#trying another time. 
#where u dont have to merge. 
#good solution: all that was needed was to break out of the loop
#each time a factor was found, so that u dont split into smaller 
#factors as long as a bigger factor exists (between 2-9)

digitsProduct <- function(product) {
  if (product == 1) {
    return(1)
  }
  if (product == 0) {
    return(10)
  }
  
  numbertodivide = product
  finalnumber <- c()
  factors <- c()
  while (numbertodivide > 1) {
    somefactor = FALSE
    for (ind in seq(from = 9, to = 2, by = -1)) {
      if (numbertodivide %% ind == 0) {
        numbertodivide = (numbertodivide / ind)
        factors <- c(factors,ind)
        somefactor = TRUE
        break
      }
    }
    if (!somefactor) {
      return(-1)
    }
  }
  return(as.numeric(paste(sort(factors),collapse = "")))
}

#attempt2:
#1. get prime factors 
#it worked but I am not happy with the solution yet.
#esp with the merging part, coz it only does 2 digits merge
#not all possible. 
#need to work on that. 
digitsProduct <- function(product) {
  if (product == 1) {
    return(1)
  }
  if (product == 0) {
    return(10)
  }
  numbertodivide = product
  finalnumber <- c()
  
  divideby2to9 <- function(x) {
    factors = c()
    for (ind in seq(from = 9, to = 2,by = -1)) {
      if (x %% ind == 0) {
        x = (x / ind)
        factors <- c(factors,ind)
      }
    }
    return(list(x,factors))
  }
  while (numbertodivide > 1) {
   output <- divideby2to9(numbertodivide)
   numbertodivide = output[[1]]
   finalnumber <- c(finalnumber,output[[2]])
   if (output[[1]] == numbertodivide & is.null(output[[2]])) {
       return(-1)
     }
   }

  
  #now make the smallest number out of finalnumber digits
  #if any digits can be clubbed into a single digits < 9, then 
  #do that and find the product. compute the min of those combinations
  

  mergedigits <- function(x) {
    #replace 2,2 by 4
    #replace 2,3 by 6
    #replace 3,3 by 9
    if (length(grep("2|3|4",x)) >= 2) {
      allreplacements = combn(grep("2|3|4", x),2)
      allfinalnumbers <- c()
      for (ind in seq(1:ncol(allreplacements))) {
        if (prod(x[allreplacements[,ind]]) <= 9) {
          newfinalnumber <- c(x[-(c(allreplacements[,ind]))], prod(x[allreplacements[,ind]]))
          #make it min (sort it and collapse it)
          allfinalnumbers <- c(allfinalnumbers, 
                               as.numeric(paste(sort(newfinalnumber),
                                                collapse = "")))
        }
      }
      return(allfinalnumbers)
    }
  }
  morefinalnumbers <- c(as.numeric(paste(sort(finalnumber),collapse = "")),
                        mergedigits(finalnumber))
  return(min(morefinalnumbers))
}

#this gets the smallest number but the problem asks for 
#the number where product of each bit is the answer

#for e.g. for product = 450, output the following code 
# would generate is 509, however what the problem is seeking
# is 2559 as 5 * 0 * 9 is not 450 but 2 * 5* 5 * 9 = 450
#thus we are looking at factors between 2 to 9. 

# digitsProduct <- function(product) {
#   allproducts <- vector()
#   index = 1;
#   for (ind in 2:(product - 1)) {
#     if (product %% ind == 0) {
#       #get quotient
#       quotient = (product / ind)
#       #combine
#       smallestproduct = paste0(ind,
#                               quotient,
#                               collapse = "")
#       allproducts[index] <- as.numeric(smallestproduct)
#       index <- index + 1;
#     }
#   }
#   allproducts = allproducts[allproducts > 0]
#   if (length(allproducts) > 0) {
#     return(min(allproducts))
#   } else {
#     return(-1)
#   }
# }

digitsProduct(19)
digitsProduct(13)
digitsProduct(12)
digitsProduct(36)
digitsProduct(450)

digitsProduct(576)
