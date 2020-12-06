library(stringr)
input <- readLines("input2.txt")

itab <- sapply(letters, function(l) { str_count(input, l)})
w2 <- apply(itab, 1, function(r) {2 %in% r})
w3 <- apply(itab, 1, function(r) {3 %in% r})
sum(w2) * sum(w3)

am <- sapply(input, function(x) {
           l <- nchar(x)
           sapply(1:l, function(i) {
                paste0(str_sub(x, 0, i-1), ".", str_sub(x, i+1))
           })
       })

ctab <- table(as.character(am))

which.max(ctab)
