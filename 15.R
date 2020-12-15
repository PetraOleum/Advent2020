library(hashmap)

input <- as.integer(c(2,0,1,7,4,14,18))

vals <- numeric(0)
sapply(1:2020, function(x) {
           if (x <= length(input)) {
               vals <<- c(vals, input[x])
               return(input[x])
           } else {
               can <- vals[x - 1]
               pos <- which(can == vals[1:(x - 2)])
               if (length(pos) == 0) {
                   vals <<- c(vals, 0)
                   return(0)
               } else {
                   le <- max(pos)
                   vals <<- c(vals, x - 1 - le)
                   return(x - 1 - le)
               }
           }
})


# Part 2

hasha <- hashmap(as.integer(input[1:(length(input) - 1)]), as.integer(1:(length(input) - 1)))
# hashb <- hashmap(numeric(0), numeric(0))

last <- tail(input, 1)
lb1 <- input[length(input) - 1]

for (x in (length(input)):30000000) {
    if ((x %% 10000) == 0) {
        print(x)
    }
    hl <- hasha[[last]]
    lb1 <- last
    if (is.na(hl)) {
        last <- 0
    } else {
        last <- (x) - hl
    }
    hasha[[lb1]] <- x
    # print(lb1)
}
print(lb1)
