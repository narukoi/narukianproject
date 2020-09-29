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
board_1_5[unique(carrier$rows),unique(carrier$columns)] <- "C"



### BATTLESHIP FUNCTION
battleship_fun <- function(carrier_df) {
  seed <- sample(seq.int(1,1000))
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


#How to determine if a position is invalid?
#check to see if for any particular point in battleship, does that point exist in carrier?
#perhaps use df to create a region that ships cannot be a part of?
battleship
carrier

carrier_col_inclusive <- seq(min(carrier$column) - 1, max(carrier$column) + 1)
carrier_col_exclusive <- seq(min(carrier$column),max(carrier$column))
carrier_row_inclusive <- seq(min(carrier$row) - 1, max(carrier$row) + 1)
carrier_row_exclusive <- seq(min(carrier$row),max(carrier$row))

any(
  (battleship$column %in% carrier_col_inclusive) & 
    (battleship$row %in% carrier_row_exclusive)
  )|any(
    (battleship$column %in% carrier_col_exclusive) &
      (battleship$row %in% carrier_row_inclusive)
    )


if (any(
  (battleship$column %in% carrier_col_inclusive) & 
  (battleship$row %in% carrier_row_exclusive)
  )|any(
    (battleship$column %in% carrier_col_exclusive) &
    (battleship$row %in% carrier_row_inclusive)
    )) {
  battleship <- battleship_fun()
  
}
battleship
