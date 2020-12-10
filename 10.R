library(parallel)
library(stringr)
library(dplyr)
input <- sort(as.numeric(readLines("input10.txt")))
diffs <- c(diff(c(0, input)), 3)

sum(diffs == 1) * sum(diffs == 3)

maxv <- max(input) + 3

# brute force is absolutely not possible

# combn(input, 2, FUN = function(sel) {
#           all(unique(diff(c(0, sel, maxv))) %in% 1:3)
# })
# output <- mclapply(1:length(input), 
#        function(m) {
#         combn(input, m, FUN = function(sel) {
#                   all(unique(diff(c(0, sel, maxv))) %in% 1:3)
#         })
#        }, mc.cores = 7)


ds<- paste0(diffs, collapse = "")
dvec <- str_split(ds, "3+") %>% sapply(nchar) %>% as.numeric()
pvec <- ifelse(dvec < 1, 1, 2^(dvec - 1))
pvec <- ifelse(pvec == 8, 7, pvec)

print(prod(pvec), digits = 12)

# 14173478093824

# 0 1
# 1 1
# 2 2
# 3 4
# 4 7
