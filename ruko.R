library(pacman)
p_load(tidyverse)

#Start by creating empty board from a matrix
board <- data.frame(matrix("~", nrow = 10, ncol = 10))
colnames(board) <- seq.int(1,10)
rownames(board) <- LETTERS[seq.int(1,10)]
board

### CARRIER FUNCTION
#creates position df for ship its named after
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
 return(carrier_positions %>%
          mutate(ship = "carrier")
        )
}
carrier <- carrier_fun()

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
  
  return(battleship_positions %>%
           mutate(ship = "battleship")
         )
  
}
battleship <- battleship_fun()

#while statement for valid positioning 
while(any(do.call(paste,battleship[,1:2]) %in% do.call(paste,carrier_red))) {
  battleship <- battleship_fun(sample(1:1000))
}

#Battleship redzone (including the carrier red zone as well)
battleship_red <- red_zone(battleship) %>% 
  rbind(carrier_red)


###SUB FUNCTION 
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
    change_pos[i] <- change_pos[i] - 3
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
  
  return(sub_positions %>%
           mutate(ship = "submarine")
         )
  
}
submarine <- sub_fun()

#valid positioning sub
while(any(do.call(paste,submarine[,1:2]) %in% do.call(paste,battleship_red))) {
  submarine <- sub_fun(sample(1:1000))
}

#sub redzone
sub_red <- red_zone(submarine) %>% 
  rbind(battleship_red)

#cruiser has same size as sub (3), so we just use the sub function again
cruiser <- sub_fun() %>%
  mutate(ship = "cruiser")

#valid positioning cruiser
while(any(do.call(paste,cruiser[,1:2]) %in% do.call(paste,sub_red))) {
  cruiser <- sub_fun(sample(1:1000)) %>%
    mutate(ship = "cruiser")
}

#cruiser redzone
cruiser_red <- red_zone(cruiser) %>%
  rbind(sub_red)

###DESTROYER FUNCTION
des_fun <- function(x = sample(1:1000)) {
  seed <- sample(x)
  set.seed(seed)
  #orientation determines the direction of a ship's placement
  orientation <- unlist(sample(c('horizontal','vertical'),1))
  
  
  #change_pos will be a vector of 5 numbers designating a ship's position along one axis
  #if orientation is horizontal, change_pos = columns
  #if orientation is vertical, change_pos = rows
  start_change <- unlist(sample(seq.int(1,10),1))
  change_pos <- c(seq.int(start_change,start_change + 1))
  
  
  #we will also ensure that no part of the ship goes off the board
  #if a number in change_pos is over 10, we subtract 5 from that element
  for (i in seq(1,2))  {if (change_pos[i]>10) {
    change_pos[i] <- change_pos[i] - 2
  }}
  
  
  #static_pos will be a single number denoting a ship's position along the other axis
  static_pos <- unlist(sample(seq.int(1,10),1))
  
  
  #now we will determine the final dataset for the carrier's position
  if (orientation == 'horizontal') {
    des_positions <- data.frame(
      column = change_pos,
      row = static_pos,
      combination = paste(LETTERS[static_pos],change_pos,sep = "")
    )}
  else {
    des_positions <- data.frame(
      column = static_pos,
      row = change_pos,
      combination = paste(LETTERS[change_pos],static_pos,sep = "")
    )}
  
  return(des_positions %>%
           mutate(ship = "destroyer")
         )
}

destroyer <- des_fun()

#valid positioning destroyer
while(any(do.call(paste,destroyer[,1:2]) %in% do.call(paste,cruiser_red))) {
  destroyer <- des_fun(sample(1:1000))
}

#final ship position df
ships <- rbind(carrier,battleship,submarine,cruiser,destroyer)

#create the board with ships on it
final_board <- board
for (i in 1:17) {
  final_board[ships$row[i],ships$column[i]] <- ifelse(ships$ship[i] == "cruiser",
                                                      "R",
                                                      ships$ship[i] %>% 
                                                        substr(1,1) %>% toupper()
                                                      )
}

final_board

#Now lets feel out the process of the computer playing the game
#Lets start with random guesses, no constraints
#Let's do a guess function
single_guess <- function(final_board) {
  pos <- sample(1:10,2,replace = TRUE)
  
  while (grepl("O|X",final_board[pos[1],pos[2]])) {
    pos <- sample(1:10,2,replace = TRUE)
  } 
  final_board[pos[1],pos[2]] <- ifelse(
     grepl("C|B|S|R|D",final_board[pos[1],pos[2]]),
    "X",
    "O"
    )
  return(final_board)
}

single_guess(final_board)
boards <- list(final_board)
turn <- 0

while (any(grepl("C|B|S|R|D",final_board))) {
  final_board <- single_guess(final_board)
  turn <- turn + 1
  eval(parse(text = paste("boards$turn",turn," <- final_board",sep = "")))
}
turn
final_board
boards
#now we have turn, which will end up giving us how many turns it takes to win a game
#we also have boards: the dataframes for the board at each turn
#we can take this three ways: 
#1. We create individual game simulations capable of repeating to test different guessing methodologies
#2. An individual game displayed through animation
#3. a game capable of being played by two people
