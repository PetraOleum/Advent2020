# Target: find *three* numbers that sum to 2020, submit them multiplied together

# Get input
input <- read.csv("input1a.csv", header=F)[[1]]

# Add every pair of numbers together
i2 <- outer(input, input, FUN = "+")

# Find the value that would have to be present for them to add to 2020
alt <- 2020 - i2

# Returns the three numbers
vals <- input[input %in% alt]
vals

# 144554112
prod(vals)

