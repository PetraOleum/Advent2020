library(stringr)
input <- readLines("input13.txt")
v <- as.numeric(input[1])
vals <- unlist(str_split(input[2], ","))
bnums <- as.numeric(vals[vals != "x"])
times <- bnums - (v %% bnums)
bindex <- which.min(times)

bnums[bindex] * times[bindex]



vbt <- which(vals != "x") - 1

target.mod <- (bnums - vbt) %% bnums


f2 <- sapply(1:bnums[2], function(x) {
           c(x*bnums[1], ((x*bnums[1]) %% bnums[2]) == target.mod[2])
})

val.2 <- f2[1,f2[2,] == 1]


tmp.v <- 0
sapply(2:length(bnums), function(x) {
           ft <- sapply(1:bnums[x], function(y) {
                            cand <- tmp.v + y*prod(bnums[1:(x - 1)])
                            c(cand, (cand %% bnums[x]) == target.mod[x])
    })
           tmp.v <<- ft[1, ft[2,] == 1]
           tmp.v
})

print(tmp.v, digits = 15)
