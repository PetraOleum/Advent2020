input <- as.numeric(readLines("input1.txt"))

sum(input)

# I should have worked out how to automate finding this, but I didn't want to do a loop
ic <- cumsum(rep(input, length.out = 139435))

table(table(ic))
