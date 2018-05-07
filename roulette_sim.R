library('tidyverse')


roulette <- sample(-1:36, 10000, replace=T)
as_data_frame(roulette)

write_csv(roulette, "roulette.csv")


str(roulette)

roulette <- as_tibble(roulette)
