input <- read.csv("input1a.csv", header=F)[[1]]

alt <- 2020 - input

vals <- input[input %in% alt]
vals
# 719796
vals[1] * vals[2]
