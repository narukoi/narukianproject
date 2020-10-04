#This is going to be a script where I flesh out the alternate methods found in testing.R
####LINE 99#####
#red_zone will be a function spitting out a dataframe of row-column combinations that other ships cannot occupy
red_zone <- function(ship_df) {
  ship_name <- deparse(substitute(ship_df))
  eval(parse(text = paste(ship_name,"_inclusive <- expand.grid(column = (min(",ship_name,"$column) - 1):(max(",ship_name,"$column) + 1),row = (min(",ship_name,"$row) - 1):(max(",ship_name,"$row) + 1))",sep = "")))
  eval(parse(text = paste(ship_name,"_null <- expand.grid(column = c(min(",ship_name,"_inclusive$column),max(",ship_name,"_inclusive$column)),row = c(min(",ship_name,"_inclusive$row),max(",ship_name,"_inclusive$row)))",sep = "")))
  eval(parse(text = paste(ship_name,"_red <- anti_join(",ship_name,"_inclusive,",ship_name,"_null)",sep = "")))
  return(as.data.frame(eval(parse(text = paste(ship_name,"_red <- anti_join(",ship_name,"_inclusive,",ship_name,"_null)",sep = "")))))
}
carrier_red <- red_zone(carrier)

###INSERT BATTLESHIP BULLSHIT###
while(any((battleship$column %in% carrier_red$column)&(battleship$row %in% carrier_red$row)) {
  battleship <- battleship_fun(sample(1:1000))
}