# An email address such as "John.Smith@example.com" is made up of a local part
# ("John.Smith"), an "@" symbol, then a domain part ("example.com").
#
# The domain name part of an email address may only consist of letters, digits,
# hyphens and dots. The local part, however, also allows a lot of different
# special characters. Here you can look at several examples of correct and
# incorrect email addresses.
#
# Given a valid email address, find its domain part.
#
# Example
#
# For address = "prettyandsimple@example.com", the output should be
# findEmailDomain(address) = "example.com"; For address =
# "fully-qualified-domain@codesignal.com", the output should be
# findEmailDomain(address) = "codesignal.com".

# address = "\"very.unusual.@.unusual.com\"@usual.com"

findEmailDomain <- function(address) {
  getlocnofattherate <- gregexpr("@",address)
  #if more than 1 @ take the last one
  numberofinstances = length(getlocnofattherate[[1]])
  relevantstartindex = getlocnofattherate[[1]][numberofinstances]
  return(substr(address,(relevantstartindex + 1),nchar(address)))
}
