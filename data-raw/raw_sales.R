## code to prepare `raw_sales` dataset goes here

raw_sales <- readr::read_csv("data-raw/sales.csv") %>% janitor::clean_names()
raw_sales$orderdate = lubridate::mdy_hm(raw_sales$orderdate)

usethis::use_data(raw_sales, overwrite = TRUE)
