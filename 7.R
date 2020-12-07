library(stringr)
library(dplyr)
input <- readLines("input7.txt")
# pc <- str_match(input, "^(\\w+\\W\\w+\\W\\w+) contain (.*)\\.$")
pc <- str_match(input, "^(.*) bags contain (.*)\\.$")
parents <- pc[,2]
childs <- pc[,3]
head(pc)
head(parents)
head(childs)

nodes <- lapply(childs, function(n) {
           nodv <- sapply(str_split(n, ", ")[[1]], function(nv) {
               v <- as.numeric(str_match(nv, "(^\\d+) .* bags?$")[,2])
               return(v)
           })
           # bcols <-
           nds <- str_match(names(nodv), "^\\d+ (.*) bags?$")
           names(nodv) <- nds[,2]
           nodv
})

names(nodes) <- parents
head(nodes)


# Part 1

bags <- c()
nbags <- c("shiny gold")

cont <- TRUE

while (cont) { #this would probably be better recursively but urgh
    ebags <- sapply(nodes, function(n) {
               TRUE %in% (nbags %in% names(n))
    })
    bags <- unique(c(bags, nbags))
    nbags <- names(nodes)[ebags]
    cont <- min(nbags %in% bags) == 0
}

# Part 2
numbags <- function(bag) {
    nbags <- nodes[[bag]]
    sbags <- names(nbags)
    v <- 0
    if (length(nbags) > 0) {
        v <- sum(sapply(sbags, numbags) * nbags)
    }
    ifelse(is.na(v), 1, v + 1)
}

bag <- "shiny gold"
numbags(bag) - 1
