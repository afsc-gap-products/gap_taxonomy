# Re-establish Oracle connection
path_gapproducts_connect <- "Z:/Projects/ConnectToOracle_GAPproducts.R" # GAP_PRODUCTS schema

if (file.exists(path_oracle_connect)) {
  source(path_oracle_connect)
} else {
  gapindex::get_connected()
}