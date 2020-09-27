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
      columns = change_pos,
      rows = static_pos
    )}
  else {
    carrier_positions <- data.frame(
      columns = static_pos,
      rows = change_pos
    )}
 return(carrier_positions)
}
carrier <- carrier_fun()

#board 1/5: 1 board adding each ship progressively
board_1_5 <- board
board_1_5[unique(carrier$rows),unique(carrier$columns)] <- "C"
