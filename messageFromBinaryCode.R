# You are taking part in an Escape Room challenge designed specifically for
# programmers. In your efforts to find a clue, you've found a binary code
# written on the wall behind a vase, and realized that it must be an encrypted
# message. After some thought, your first guess is that each consecutive 8 bits
# of the code stand for the character with the corresponding extended ASCII
# code.
#
# Assuming that your hunch is correct, decode the message.
#
# Example
#
# For code = "010010000110010101101100011011000110111100100001", the output
# should be messageFromBinaryCode(code) = "Hello!".
#
# The first 8 characters of the code are 01001000, which is 72 in the binary
# numeral system. 72 stands for H in the ASCII-table, so the first letter is H.
# Other letters can be obtained in the same manner.
#
# Input/Output
#
# [execution time limit] 5 seconds (r)
#
# [input] string code
#
# A string, the encrypted message consisting of characters '0' and '1'.
#
# Guaranteed constraints: 0 < code.length < 800.
#
# [output] string
#
# The decrypted message.

#new functions alert:
# 1. regex to split into 8 bit words
# 2. strtoi to convert string to integer 
# 3. intToUtf8 for int to ascii. phew.

messageFromBinaryCode <- function(code) {
  #some crazy regex which I can never remember: 
  # https://stackoverflow.com/questions/26497583/split-a-string-every-5-characters
  return(intToUtf8(strtoi(strsplit(gsub("(.{8})","\\1 ",code)," ")[[1]],
                 base = 2)))
}
