library(stringr)
library(dplyr)
input <- read.csv("input2.csv", header=F, stringsAsFactors=F)
names(input) = "Orig"

# First question
input %>% mutate(minv = as.numeric(str_extract(Orig, "[0-9]+")),
                 maxv = as.numeric(str_extract(str_extract(Orig, "-[0-9]+"), "[0-9]+")),
                 letter = str_extract(Orig, "[a-z]"),
                 pword =  str_extract(Orig, "[a-z]+$"),
                 count = str_count(pword, letter),
                 valid = (count <= maxv) & (count >= minv)) -> result
# 607
sum(result$valid)

# Second question
result %>% mutate(fpos = str_sub(pword, minv, minv) == letter,
                  spos = str_sub(pword, maxv, maxv) == letter,
                  valid2 = (fpos + spos) == 1) -> result
sum(result$valid2)
