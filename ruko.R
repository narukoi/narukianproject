library(pacman)
p_load(tidyverse)

board <- data.frame(matrix("~", nrow = 10, ncol = 10))
colnames(board) <- seq.int(1,10)
rownames(board) <- LETTERS[seq.int(1,10)]

carrier <- function(){
  
}