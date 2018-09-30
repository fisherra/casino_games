# Code from Roulette part I 

# Load Library 
library('tidyverse')

# loading roulette.csv
roulette <- read_csv("input/roulette.csv")
roulette <- as.integer(roulette$value)


# create bets 
# The Gamblers Bets
black <- c(2, 4, 7, 8, 10, 11, 13, 15, 17, 20, 22, 24, 26, 28, 29, 31, 33, 35)
third_dozen <- c(25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36)
lucky_23 <- 23



# Initialize gambler A
ga_earn <- vector("double", length(roulette))
ga_total_earn <- 0 
ga_wins <- vector("double", length(roulette))
ga_total_wins <- 0 

# Initialize gambler B
gb_earn <- vector("double", length(roulette))
gb_total_earn <- 0 
gb_wins <- vector("double", length(roulette))
gb_total_wins <- 0 

# Initialize gambler C
gc_earn <- vector("double", length(roulette))
gc_total_earn <- 0 
gc_wins <- vector("double", length(roulette))
gc_total_wins <- 0 



for (i in seq_along(roulette)) {
  # All gamblers bet $10
  ga_total_earn = ga_total_earn - 10
  gb_total_earn = gb_total_earn - 10
  gc_total_earn = gc_total_earn - 10
  # If gambler A wins, +$20 and +1 win
  if (roulette[[i]] %in% black) {
    ga_total_earn = ga_total_earn + 20 
    ga_total_wins = ga_total_wins + 1 
  }
  # If gambler B wins, +$30 and +1 win
  if (roulette[[i]] %in% third_dozen) {
    gb_total_earn = gb_total_earn + 30 
    gb_total_wins = gb_total_wins + 1
  }
  # If gambler C wins, +$360 and +1 win
  if (roulette[[i]] == lucky_23) {
    gc_total_earn = gc_total_earn + 360
    gc_total_wins = gc_total_wins + 1
  }
  # End of round - save totals 
  ga_earn[[i]] <- ga_total_earn
  ga_wins[[i]] <- ga_total_wins
  gb_earn[[i]] <- gb_total_earn
  gb_wins[[i]] <- gb_total_wins
  gc_earn[[i]] <- gc_total_earn
  gc_wins[[i]] <- gc_total_wins
}

plot(roulette)

# Define each gambler as a variable 
ga <- "Gambler A"
gb <- "Gambler B"
gc <- "Gambler C"

# Break up the simulation into the defined magnitude intervals
results <- tibble(spin_num = 1:10000, ga, ga_earn, ga_wins, gb, gb_earn, gb_wins, gc, gc_earn, gc_wins)
results_100 <- head(results, n=100)
results_1000 <- head(results, n=1000)


# results of 100 spins
ggplot(results_100) + 
  geom_point(aes(x = spin_num, y = ga_wins, color = ga), alpha = 0.8) + 
  geom_point(aes(x = spin_num, y = gb_wins, color = gb), alpha = 0.8) + 
  geom_point(aes(x = spin_num, y  = gc_wins, color = gc), alpha = 0.8) + 
  theme_minimal() +
  theme(legend.title=element_blank()) + 
  scale_colour_manual(values = c("black", "#E0080B", "#016D29")) + 
  labs(title = "100 Spin Results - Wins", 
       y = "Number of Wins",
       x = "Number of Spins"
  )

ga_wins[100]
gb_wins[100]
gc_wins[100]

ggplot(results_100) + 
  geom_point(aes(x = spin_num, y = ga_earn, color = ga), alpha = 0.8) + 
  geom_point(aes(x = spin_num, y = gb_earn, color = gb), alpha = 0.8) + 
  geom_point(aes(x = spin_num, y = gc_earn, color = gc), alpha = 0.8) + 
  theme_minimal() +
  theme(legend.title=element_blank()) + 
  scale_colour_manual(values = c("black", "#E0080B", "#016D29")) + 
  labs(title = "100 Spin Results - Earnings", 
       y = "Earnings ($)",
       x = "Number of Spins"
  )
