input <- as.numeric(readLines("input9.txt"))

allplus <- function(vec) {
    unlist(lapply(1:(length(vec) - 1), function(i) {
                      vec[i] + vec[(i + 1):length(vec)]
    }))
}

vvals <- sapply(1:length(input), function(x) {
           if (x <= 25) {
               return(TRUE)
           }
           tnum <- input[x]
           l25 <- input[(x - 25):(x - 1)]
           return(tnum %in% allplus(l25))
           
})

inval <- input[!vvals][1]
inval

alladd <- sapply(1:(length(input) - 1), function(x) {
           sapply((x + 1):length(input), function(y) {
                      sel <- input[x:y]
                      if (sum(sel) == inval) {
                          print(sel)
                          print(min(sel) + max(sel))
                      }
                      return(sum(sel))
    })
})
