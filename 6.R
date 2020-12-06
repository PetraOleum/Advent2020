library(stringr)
library(dplyr)
input <- readLines("input6.txt")
ig <- paste0(input, collapse = "\n") %>% str_split("\n\n") %>% unlist()

igt <- ig %>% str_replace_all("\n", "")
qans <- sapply(letters, function(l) {str_count(igt, l) > 0})
sum(rowSums(qans))
sum(qans) #or actually...

tans <- sapply(letters, function(l) {str_count(igt, l)})

numpg <- str_count(ig, "\n") + 1

qtans <- apply(tans, 2, function(x) {x == numpg})
sum(qtans)
