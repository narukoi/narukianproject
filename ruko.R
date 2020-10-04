library(pacman)
p_load(tidyverse)

board <- data.frame(matrix("~", nrow = 10, ncol = 10))
colnames(board) <- seq.int(1,10)
rownames(board) <- LETTERS[seq.int(1,10)]
board

carrier_fun <- function(){
  #orientation determines the direction of a ship's placement
  orientation <- unlist(sample(c('horizontal','vertical'),1))
  
  
  #change_pos will be a vector of 5 numbers designating a ship's position along one axis
  #if orientation is horizontal, change_pos = columns
  #if orientation is vertical, change_pos = rows
  start_change <- unlist(sample(seq.int(1,10),1))
  change_pos <- c(seq.int(start_change,start_change + 4))
  
  
  #we will also ensure that no part of the ship goes off the board
  #if a number in change_pos is over 10, we subtract 5 from that element
  for (i in seq(1,5))  {if (change_pos[i]>10) {
    change_pos[i] <- change_pos[i] - 5
  }}
  
  
  #static_pos will be a single number denoting a ship's position along the other axis
  static_pos <- unlist(sample(seq.int(1,10),1))
  
  
  #now we will determine the final dataset for the carrier's position
  if (orientation == 'horizontal') {
    carrier_positions <- data.frame(
      column = change_pos,
      row = static_pos,
      combination = paste(LETTERS[static_pos],change_pos,sep = "")
    )}
  else {
    carrier_positions <- data.frame(
      column = static_pos,
      row = change_pos,
      combination = paste(LETTERS[change_pos],static_pos,sep = "")
    )}
 return(carrier_positions)
}
carrier <- carrier_fun()

#board 1/5: 1 board adding each ship progressively
board_1_5 <- board
board_1_5[unique(carrier$row),unique(carrier$column)] <- "C"



### BATTLESHIP FUNCTION
battleship_fun <- function(x = sample(1:1000)) {
  seed <- sample(x)
  set.seed(seed)
  #orientation determines the direction of a ship's placement
  orientation <- unlist(sample(c('horizontal','vertical'),1))
  
  
  #change_pos will be a vector of 5 numbers designating a ship's position along one axis
  #if orientation is horizontal, change_pos = columns
  #if orientation is vertical, change_pos = rows
  start_change <- unlist(sample(seq.int(1,10),1))
  change_pos <- c(seq.int(start_change,start_change + 3))
  
  
  #we will also ensure that no part of the ship goes off the board
  #if a number in change_pos is over 10, we subtract 5 from that element
  for (i in seq(1,4))  {if (change_pos[i]>10) {
    change_pos[i] <- change_pos[i] - 4
  }}
  
  
  #static_pos will be a single number denoting a ship's position along the other axis
  static_pos <- unlist(sample(seq.int(1,10),1))
  
  
  #now we will determine the final dataset for the carrier's position
  if (orientation == 'horizontal') {
    battleship_positions <- data.frame(
      column = change_pos,
      row = static_pos,
      combination = paste(LETTERS[static_pos],change_pos,sep = "")
    )}
  else {
    battleship_positions <- data.frame(
      column = static_pos,
      row = change_pos,
      combination = paste(LETTERS[change_pos],static_pos,sep = "")
    )}
  
  return(battleship_positions)
  
}
battleship <- battleship_fun()

#Create carrier red zone where no other ships can be positioned
red_zone <- function(ship_df) {
  ship_name <- deparse(substitute(ship_df))
  eval(parse(text = paste(ship_name,"_inclusive <- expand.grid(",
                            "column = (min(",ship_name,"$column) - 1):(max(",ship_name,"$column) + 1),",
                            "row = (min(",ship_name,"$row) - 1):(max(",ship_name,"$row) + 1))",
                          sep = ""
                          )))
  eval(parse(text = paste(ship_name,"_null <- expand.grid(",
                            "column = c(min(",ship_name,"_inclusive$column),max(",ship_name,"_inclusive$column)),",
                            "row = c(min(",ship_name,"_inclusive$row),max(",ship_name,"_inclusive$row)))",
                          sep = ""
                          )))
  eval(parse(text = paste(ship_name,"_red <- anti_join(",
                            ship_name,"_inclusive,",
                            ship_name,"_null)",
                          sep = ""
                          )))
  return(as.data.frame(eval(parse(text = paste(ship_name,"_red <- anti_join(",ship_name,"_inclusive,",ship_name,"_null)",sep = "")))))
}

carrier_red <- red_zone(carrier)

#Now create battleship redzone (including the carrier red zone as well)
battleship_red <- red_zone(battleship) %>% 
  rbind(carrier_red)

#Now what? Put the new ship on the original board
board_2_5 <- board_1_5
board_2_5[unique(battleship$row),unique(battleship$column)] <- "B"

sub_fun <- function(x = sample(1:1000)) {
  seed <- sample(x)
  set.seed(seed)
  #orientation determines the direction of a ship's placement
  orientation <- unlist(sample(c('horizontal','vertical'),1))
  
  
  #change_pos will be a vector of 5 numbers designating a ship's position along one axis
  #if orientation is horizontal, change_pos = columns
  #if orientation is vertical, change_pos = rows
  start_change <- unlist(sample(seq.int(1,10),1))
  change_pos <- c(seq.int(start_change,start_change + 2))
  
  
  #we will also ensure that no part of the ship goes off the board
  #if a number in change_pos is over 10, we subtract 5 from that element
  for (i in seq(1,3))  {if (change_pos[i]>10) {
    change_pos[i] <- change_pos[i] - 2
  }}
  
  
  #static_pos will be a single number denoting a ship's position along the other axis
  static_pos <- unlist(sample(seq.int(1,10),1))
  
  
  #now we will determine the final dataset for the carrier's position
  if (orientation == 'horizontal') {
    sub_positions <- data.frame(
      column = change_pos,
      row = static_pos,
      combination = paste(LETTERS[static_pos],change_pos,sep = "")
    )}
  else {
    sub_positions <- data.frame(
      column = static_pos,
      row = change_pos,
      combination = paste(LETTERS[change_pos],static_pos,sep = "")
    )}
  
  return(sub_positions)
  
}
submarine <- sub_fun()


cruiser <- sub_fun()
