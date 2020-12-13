library(stringr)
input <- str_split(readLines("input11.txt"), "", simplify=TRUE)
input <- ifelse(input == ".", 0, ifelse(input == "L", -1, 1))
di <- dim(input)
adj <- as.matrix(expand.grid(-1:1, -1:1))[c(1:4, 6:9), 1:2]
getadj <- function(x, state) {
    apply(adj, 1, function(a) {
              n <- a + x
              ifelse(any(n > di) || any(n < 1), 0, state[n[1], n[2]])
    })
}
getnext <- function(state) {
    t(sapply(1:nrow(state), function(x) {
               sapply(1:ncol(state), function(y) {
                          cv <- state[x, y]
                          if (cv == 0) {
                              return(0)
                          }
                          av <- getadj(c(x, y), state)
                          if (cv == -1 && sum(av == 1) == 0) {
                              return(1)
                          }
                          if (cv == 1 && sum(av == 1) >= 4) {
                              return(-1)
                          }
                          return(cv)
            })
        }))
}

last <- 0
cur <- input

while (!identical(cur, last)) {
    last <- cur
    cur <- getnext(cur)
    print(sum(cur == 1))
}

getadj2 <- function(x, state) {
    apply(adj, 1, function(a) {
              f <- 0
              while (TRUE) {
                  f <- f + 1
                  n <- a*f + x
                  if (any(n > di) || any(n < 1)) {
                      return(0)
                  }
                  tv <- state[n[1], n[2]]
                  if (tv != 0) {
                      return(tv)
                  }
              }
    })
}

getnext2 <- function(state) {
    t(sapply(1:nrow(state), function(x) {
               sapply(1:ncol(state), function(y) {
                          cv <- state[x, y]
                          if (cv == 0) {
                              return(0)
                          }
                          av <- getadj2(c(x, y), state)
                          if (cv == -1 && sum(av == 1) == 0) {
                              return(1)
                          }
                          if (cv == 1 && sum(av == 1) >= 5) {
                              return(-1)
                          }
                          return(cv)
            })
        }))
}

last <- 0
cur <- input

while (!identical(cur, last)) {
    last <- cur
    cur <- getnext2(cur)
    print(sum(cur == 1))
}
