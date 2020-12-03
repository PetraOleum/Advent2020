library(stringr)
library(dplyr)

input <- read.table("input3.txt", stringsAsFactors = FALSE, comment.char = '')[[1]] %>% str_split("")

# actually a transposed matrix of the original
input <- sapply(input, function(x) {x == '#'})
width <- nrow(input)
height <- ncol(input)
# Unroll! Actually this is not needed
# iv <- as.vector(input)
grad <- function(map, down, across) {
    # Horizontal and vertical coordinates
    v <- seq(1, ncol(map), down)
    h <- ((0:(length(v) - 1) * across) %% nrow(map)) + 1
    # get vals at coordinates
    apply(cbind(h, v), 1, function(x) {map[x[1], x[2]]})
}

# Part a (3:1 slope)
sum(grad(input, 1, 3))

# Part b (1:1, 3:1, 5:1, 7:1, 1:2 slopes)
prod(sum(grad(input, 1, 1)), sum(grad(input, 1, 3)), sum(grad(input, 1, 5)), sum(grad(input, 1, 7)), sum(grad(input, 2, 1)))
