library(stringr)
input <- readLines("input8.txt")
code.parsed <- str_match(input, "^([\\w]{3}) ([\\+-]\\d+)$")
cmds <- code.parsed[,2]
params <- as.numeric(code.parsed[,3])


runints <- function(cmds, params) {
    pos.hist <- c()
    cpos <- 1
    acc <- 0
    target <- length(cmds) + 1
    while ((!(cpos %in% pos.hist)) && cpos < target && cpos > 0) {
        pos.hist <- c(pos.hist, cpos)
        cin <- cmds[cpos]
        cpa <- params[cpos]
        if (cin == "jmp") {
            cpos <- cpos + cpa
        } else if (cin == "acc") {
            acc <- acc + cpa
            cpos <- cpos + 1
        } else {
            cpos <- cpos + 1
        }
    }
    # print(acc)
    ifelse(cpos == target, acc, NA)
}

runints(cmds, params)

# crude rude brute force
outputs <- sapply(1:length(cmds), function(x) {
                      cmds1 <- cmds
                      if (cmds[x] == "jmp") {
                          cmds1[x] <- "nop"
                          return(runints(cmds1, params))
                      } else if (cmds[x] == "nop") {
                          cmds1[x] <- "jmp"
                          return(runints(cmds1, params))
                      } else {
                          return(NA)
                      }
})

max(outputs, na.rm = TRUE)
