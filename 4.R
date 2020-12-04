library(dplyr)
library(stringr)
library(tidyr)
input <- readLines("input4.txt") %>% paste(collapse = "\n") %>% str_split("\n\n") %>% unlist() %>% str_replace_all("\n", " ")
flds <- str_match(input, "([a-z]+):")[,2] %>% unique()
df <- data.frame(Orig = input)
for (f in flds) {
    df[[f]] <- str_match(input, paste0(f, ":([a-zA-Z0-9#]+)"))[,2]
}
df.valid <- df %>% drop_na(-cid)
str(df)
str(df.valid)
nrow(df.valid)

ecs <- c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
df.valid %>% mutate(
        byr = ifelse(between(as.numeric(str_match(byr, "^([0-9]+)$")[,2]), 1920, 2002), byr, NA),
        iyr = ifelse(between(as.numeric(str_match(iyr, "^([0-9]+)$")[,2]), 2010, 2020), iyr, NA),
        eyr = ifelse(between(as.numeric(str_match(eyr, "^([0-9]+)$")[,2]), 2020, 2030), eyr, NA),
        hgt = ifelse(between(as.numeric(str_match(hgt, "^([0-9]+)cm$")[,2]), 150, 193) | between(as.numeric(str_match(hgt, "^([0-9]+)in$")[,2]), 59, 76), hgt, NA),
        hcl = str_extract(hcl, "^[#]{1}[0-9a-f]{6}$"),
        ecl = ifelse(ecl %in% ecs, ecl, NA),
        pid = str_extract(pid, "^[0-9]{9}$")
    ) %>% drop_na(-cid) -> dv2
nrow(dv2)
