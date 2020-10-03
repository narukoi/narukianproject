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
battleship_fun <- function(x = sample(seq(1,1000))) {
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

#Placement test will return TRUE if invalid position, and FALSE if the position is ok
battleship
carrier

carrier_col_inclusive <- seq(min(carrier$column) - 1, max(carrier$column) + 1)
carrier_col_exclusive <- seq(min(carrier$column),max(carrier$column))
carrier_row_inclusive <- seq(min(carrier$row) - 1, max(carrier$row) + 1)
carrier_row_exclusive <- seq(min(carrier$row),max(carrier$row))

placement_test <- any((
  (battleship$column %in% carrier_col_inclusive) & 
    (battleship$row %in% carrier_row_exclusive)
  )|(
    (battleship$column %in% carrier_col_exclusive) &
      (battleship$row %in% carrier_row_inclusive)
    ))
placement_test

#while statement remakes the object battleship until there's an appropriate position
#check by using not to look for an invalid position
#STILL NOT WORKING
while (any((
  (battleship$column %in% carrier_col_inclusive) & 
  (battleship$row %in% carrier_row_exclusive)
)|
  (battleship$column %in% carrier_col_exclusive) &
  (battleship$row %in% carrier_row_inclusive)
)) {
  battleship <- battleship_fun(sample(seq(1,1000)))
}
battleship
carrier
#Now what? Put the new ship on the original board
board_2_5 <- board_1_5
board_2_5[unique(battleship$row),unique(battleship$column)] <- "B"

sub_fun <- function(x = sample(seq(1,1000))) {
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

bs_col_inclusive <- seq(min(battleship$column) - 1, max(battleship$column) + 1)
bs_col_exclusive <- seq(min(battleship$column),max(battleship$column))
bs_row_inclusive <- seq(min(battleship$row) - 1, max(battleship$row) + 1)
bs_row_exclusive <- seq(min(battleship$row),max(battleship$row))


while (any((
  (submarine$column %in% carrier_col_inclusive) & 
  (submarine$row %in% carrier_row_exclusive)
)|
(battleship$column %in% carrier_col_exclusive) &
(battleship$row %in% carrier_row_inclusive)
)) {
  battleship <- battleship_fun(sample(seq(1,1000)))
}