board_2_5

carrier
battleship
submarine

carrier_inclusive <- expand.grid(
  column = (min(carrier$column) - 1):(max(carrier$column) + 1),
  row = (min(carrier$row) - 1):(max(carrier$row) + 1)
  ) 
carrier_null <- expand.grid(
  column = c(min(carrier_inclusive$column),max(carrier_inclusive$column)),
  row = c(min(carrier_inclusive$row),max(carrier_inclusive$row))
)
carrier_red <- anti_join(carrier_inclusive,carrier_null)

carrier[1,]
battleship[,-3] %in% carrier_red
ship_name <- "battleship"
ships <- c("carrier","battleship","submarine","cruiser","destroyer")
lapply(ships,)
red_zone <- function(ship_df) {
  ship_name <- deparse(substitute(ship_df))
    eval(parse(text = paste(ship_name,"_inclusive <- expand.grid(column = (min(",ship_name,"$column) - 1):(max(",ship_name,"$column) + 1),row = (min(",ship_name,"$row) - 1):(max(",ship_name,"$row) + 1))",sep = "")))
    eval(parse(text = paste(ship_name,"_null <- expand.grid(column = c(min(",ship_name,"_inclusive$column),max(",ship_name,"_inclusive$column)),row = c(min(",ship_name,"_inclusive$row),max(",ship_name,"_inclusive$row)))",sep = "")))
    eval(parse(text = paste(ship_name,"_red <- anti_join(",ship_name,"_inclusive,",ship_name,"_null)",sep = "")))
    return(as.data.frame(eval(parse(text = paste(ship_name,"_red <- anti_join(",ship_name,"_inclusive,",ship_name,"_null)",sep = "")))))
}
battleship_inclusive
battleship_null
battleship_red
red_zone(carrier)
battleship[,1:2] %in% red_zone(carrier)



while(any(do.call(paste,battleship[1:2]) %in% do.call(paste,carrier_red[1:2]))) {
  battleship <- battleship_fun(sample(1:1000))
}

battleship[(battleship$column %in% carrier_red$column)&(battleship$row %in% carrier_red$row)]

test1 <- carrier_fun()
test2 <- battleship_fun()
while(!any((test2$column %in% test1$column)&(test2$row %in% test1$row))) {
  test2 <- battleship_fun(sample(1:1000))
}


do.call(paste,test1[,1:2])
        