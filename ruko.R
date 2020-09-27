library(pacman)
p_load(tidyverse)

board <- data.frame(matrix("~", nrow = 10, ncol = 10))
colnames(board) <- seq.int(1,10)
rownames(board) <- LETTERS[seq.int(1,10)]

carrier <- function(){
  orientation <- unlist(sample(c('h','v'),1))
  start_change <- unlist(sample(seq.int(1,10),1))
}