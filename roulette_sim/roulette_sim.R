library('tidyverse')


roulette <- sample(-1:36, 10000, replace=T)

write_csv(roulette, "roulette.csv")

str(roulette)

roulette <- as_tibble(roulette)
