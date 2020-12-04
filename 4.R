library(dplyr)
library(stringr)
library(tidyr)
input <- readLines("input4.txt") %>% paste(collapse = "\n") %>% str_split("\n\n") %>% unlist() %>%str_replace_all("\n", " ")
flds <- input %>% str_extract_all("[a-z]+:") %>% unlist() %>% unique() %>% str_replace(":", "")
df <- data.frame(Orig = input)
for (f in flds) {
    df[[f]] <- str_extract(input, paste0(f, ":[a-zA-Z0-9#]+")) %>% str_replace_all(paste0(f, ":"), "")
}
df.valid <- df %>% drop_na(-cid)
str(df)
str(df.valid)
nrow(df.valid)
