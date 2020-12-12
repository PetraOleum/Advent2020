library(stringr)
library(dplyr)

input <- readLines("input12.txt") %>% str_match("^(\\w)(\\d+)$") %>% as.data.frame(stringsAsFactors = FALSE)
names(input) <- c("Orig", "Arg", "Param")

input %>% mutate(
                 Param = as.numeric(Param),
                 angChange = ifelse(Arg == "R", Param, ifelse(Arg == "L", -Param, 0)),
                 Angle = cumsum(angChange) %% 360,
                 MovEast = ifelse(Arg == "E", Param,
                                  ifelse(Arg == "W", -Param,
                                         ifelse(Arg == "F", cos(Angle * pi / 180) * Param, 0))),
                 MovSouth = ifelse(Arg == "S", Param,
                                  ifelse(Arg == "N", -Param,
                                         ifelse(Arg == "F", sin(Angle * pi / 180) * Param, 0))),
                 PosEast = cumsum(MovEast),
                 PosSouth = cumsum(MovSouth)
                 ) -> shipsLog

mandist <- abs(sum(shipsLog$MovEast)) + abs(sum(shipsLog$MovSouth))

shipsLog %>% select(Orig:Angle) %>%
    mutate(
           tPMovEast = ifelse(Arg == "E", Param, ifelse(Arg == "W", -Param, 0)),
           tPMovSouth = ifelse(Arg == "S", Param, ifelse(Arg == "N", -Param, 0)),
           PMovEast = ifelse(Angle == 0, tPMovEast,
                             ifelse(Angle == 90, tPMovSouth,
                                    ifelse(Angle == 180, -tPMovEast,
                                           -tPMovSouth))),
           PMovSouth = ifelse(Angle == 0, tPMovSouth,
                              ifelse(Angle == 90, -tPMovEast,
                                     ifelse(Angle == 180, -tPMovSouth,
                                            tPMovEast))),
           PREastPos = cumsum(PMovEast) + 10,
           PRSouthPos = cumsum(PMovSouth) - 1,
           PTEastPos = ifelse(Angle == 0, PREastPos,
                              ifelse(Angle == 90, -PRSouthPos,
                                     ifelse(Angle == 180, -PREastPos,
                                            PRSouthPos))),
           PTSouthPos = ifelse(Angle == 0, PRSouthPos,
                              ifelse(Angle == 90, PREastPos,
                                     ifelse(Angle == 180, -PRSouthPos,
                                            -PREastPos))),
           ShipMoveEast = ifelse(Arg == "F", Param * PTEastPos, 0),
           ShipMoveSouth = ifelse(Arg == "F", Param * PTSouthPos, 0)
           ) -> shipsLog2
                                           

mandist2 <- abs(sum(shipsLog2$ShipMoveEast)) + abs(sum(shipsLog2$ShipMoveSouth))
