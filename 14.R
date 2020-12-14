library(stringr)
input <- readLines("input14.txt")

# bits <- numeric(36)
twop <- 2^(35:0)
dat <- list()

cmask <- rep("X", 36)


for (ln in input) {
    if (grepl("^mask", ln)) {
        cmask <- unlist(str_split(str_extract(ln, "[X10]+$"), ""))
    } else if (grepl("^mem", ln)) {
        vp <- str_match(ln, "^mem\\[(\\d+)\\] = (\\d+$)")
        memadr <- vp[1,2]
        tval <- as.numeric(vp[1,3])
        bits <- numeric(36)
        for (x in 1:length(twop)) {
            if (tval >= twop[x]) {
                tval = tval - twop[x]
                bits[x] <- 1
            }
        }
        bits <- ifelse(cmask == "X", bits, as.numeric(cmask))
        dat[[memadr]] <- sum(bits * twop)
    }
}

print(sum(unlist(dat)), digits = 15)

num2bin <- function(x) {
    bits <- numeric(36)
    for (i in 1:length(twop)) {
        if (x >= twop[i]) {
            x = x - twop[i]
            bits[i] <- 1
        }
    }
    bits
}

allposs <- function(bits) {
    isX <- which(is.na(bits))
    if(length(isX) != 0) {
        gridm <- as.matrix(expand.grid(lapply(isX, function(x) {0:1})))
        colnames(gridm) <- NULL
        return(apply(gridm, 1, function(r) {
                  bits[isX] <- r
                  sum(bits * twop)
        }))
    } else {
        return(sum(bits * twop))
    }
}

dat <- list()

for (ln in input) {
    if (grepl("^mask", ln)) {
        cmask <- unlist(str_split(str_extract(ln, "[X10]+$"), ""))
        cmask <- ifelse(cmask == "X", NA, ifelse(cmask == "1", 1, 0))
    } else if (grepl("^mem", ln)) {
        vp <- str_match(ln, "^mem\\[(\\d+)\\] = (\\d+$)")
        memadr <- num2bin(as.numeric(vp[1,2]))
        tval <- as.numeric(vp[1,3])
        memadr <- ifelse(is.na(cmask), NA, ifelse(cmask == 1, 1, memadr))
        memadr <- allposs(memadr)
        for (m in memadr) {
            dat[[as.character(m)]] <- tval
        }
        
    }
}

print(sum(unlist(dat)), digits = 15)
