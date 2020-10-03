board_2_5

carrier
battleship
submarine

carrier_inclusive <- expand.grid(
  column = (min(carrier$column) - 1):(max(carrier$column) + 1),
  row = (min(carrier$row) - 1):(max(carrier$row) + 1)
  ) 
carrier_null <- expand.grid(
  column = c(min(carrier_red$column),max(carrier_red$column)),
  row = c(min(carrier_red$row),max(carrier_red$row))
)
carrier_red <- anti_join(carrier_inclusive,carrier_null)

carrier[1,]
battleship[,-3] %in% carrier_red
