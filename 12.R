library(stringr)
library(dplyr)

# Make a dataframe of input, with column 2 as the letter and column 3 as the number
input <- readLines("input12.txt") %>% str_match("^(\\w)(\\d+)$") %>% as.data.frame(stringsAsFactors = FALSE)
names(input) <- c("Orig", "Arg", "Param")

input %>% mutate(
                 Param = as.numeric(Param), # Make number numeric type
                 angChange = ifelse(Arg == "R", Param, ifelse(Arg == "L", -Param, 0)), # Calculate what each R/L does to the angle
                 Angle = cumsum(angChange) %% 360, # Calculate running angle, within 0-359
                 # Calculate amount to move East, South at each line
                 MovEast = ifelse(Arg == "E", Param,
                                  ifelse(Arg == "W", -Param,
                                         ifelse(Arg == "F", cos(Angle * pi / 180) * Param, 0))),
                 MovSouth = ifelse(Arg == "S", Param,
                                  ifelse(Arg == "N", -Param,
                                         ifelse(Arg == "F", sin(Angle * pi / 180) * Param, 0))),
                 # Running tally of position
                 PosEast = cumsum(MovEast),
                 PosSouth = cumsum(MovSouth)
                 ) -> shipsLog

# Calculate manhattin distance
mandist <- abs(sum(shipsLog$MovEast)) + abs(sum(shipsLog$MovSouth))

# Drop stuff from previous dataframe that wont be used this time
shipsLog %>% select(Orig:Angle) %>%
    mutate(
           # Calculate the east, south amount being specified by param value
           tPMovEast = ifelse(Arg == "E", Param, ifelse(Arg == "W", -Param, 0)),
           tPMovSouth = ifelse(Arg == "S", Param, ifelse(Arg == "N", -Param, 0)),
           # Convert to a rotating reference frame according to the angle
           PMovEast = ifelse(Angle == 0, tPMovEast,
                             ifelse(Angle == 90, tPMovSouth,
                                    ifelse(Angle == 180, -tPMovEast,
                                           -tPMovSouth))),
           PMovSouth = ifelse(Angle == 0, tPMovSouth,
                              ifelse(Angle == 90, -tPMovEast,
                                     ifelse(Angle == 180, -tPMovSouth,
                                            tPMovEast))),
           # Calculate running position of waypoint in rotated frame
           PREastPos = cumsum(PMovEast) + 10,
           PRSouthPos = cumsum(PMovSouth) - 1,
           # Return to original fixed reference frame
           PTEastPos = ifelse(Angle == 0, PREastPos,
                              ifelse(Angle == 90, -PRSouthPos,
                                     ifelse(Angle == 180, -PREastPos,
                                            PRSouthPos))),
           PTSouthPos = ifelse(Angle == 0, PRSouthPos,
                              ifelse(Angle == 90, PREastPos,
                                     ifelse(Angle == 180, -PRSouthPos,
                                            -PREastPos))),
           # Calculate movement of ship at each line
           ShipMoveEast = ifelse(Arg == "F", Param * PTEastPos, 0),
           ShipMoveSouth = ifelse(Arg == "F", Param * PTSouthPos, 0)
           ) -> shipsLog2
                                           

# Calculate manhattin distance
mandist2 <- abs(sum(shipsLog2$ShipMoveEast)) + abs(sum(shipsLog2$ShipMoveSouth))
