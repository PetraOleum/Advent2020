# Target: find two numbers that sum to 2020, submit them multiplied together

# Get input
input <- read.csv("input1a.csv", header=F)[[1]]

# Find the value that would have to be present for them to add to 2020
alt <- 2020 - input

# Returns the three numbers
vals <- input[input %in% alt]
vals

# 719796
prod(vals)
