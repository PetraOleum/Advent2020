library(stringr)
input <- readLines("input5.txt")
im <- str_match(input, "^([BF])([BF])([BF])([BF])([BF])([BF])([BF])([RL])([RL])([RL])$")

rowspec <- apply(im[,2:8], c(1,2), function(x) {x == "B"})
colspec <- apply(im[,9:11], c(1,2), function(x) {x == "R"})
rownums <- as.vector(rowspec%*%(2^((ncol(rowspec)-1):0)))
colnums <- as.vector(colspec%*%(2^((ncol(colspec)-1):0)))
id <- rownums * 8 + colnums

# Part a
max(id)

# Part b
idl <- sort(id)
idl[which.max(diff(idl))] + 1
