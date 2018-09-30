# Code from roll_the_dice.rmd

# load libraries
library('tidyverse')

# roll a single die
sample(1:6,1)


# use the sample function to simulate 100 independent rolls of a die
single_die_1 <- sample(1:6, 50, replace=T)

# order the values into a dataframe named sd_1_freq
sd_1_freq <- data_frame(num = 1:6, roll = table(single_die_1)/50)

# plot the frequency data
ggplot(sd_1_freq) + 
  geom_bar(aes(num,roll),
           stat = "identity",
           color = "darkslateblue",
           fill = "darkslateblue") + 
  geom_hline(yintercept = (1/6),
             col = "firebrick",
             size = 1
  ) + 
  scale_x_continuous(name = "Result",
                     breaks = sd_1_freq$num, 
                     labels = sd_1_freq$num) + 
  ylab("Relative Frequency") + 
  ggtitle("50 Roll Relative Frequency") + 
  theme_minimal()



# use the sample function to simulate 50,000 independent rolls of a die
single_die_2 <- sample(1:6, 50000, replace=T)

# order the values into a dataframe named sd_1_freq
sd_2_freq <- data_frame(num = 1:6, roll = table(single_die_2)/50000)

# plot the frequency data
ggplot(sd_2_freq) + 
  geom_bar(aes(num,roll), 
           stat="identity",
           color = "darkslateblue",
           fill = "darkslateblue"
  ) + 
  geom_hline(
    yintercept = (1/6),
    col = "firebrick",
    size = 1
  ) + 
  scale_x_continuous(name = "Result",
                     breaks = sd_2_freq$num, 
                     labels = sd_2_freq$num
  ) + 
  ylab("Relative Frequency") + 
  ggtitle("50,000 Roll Relative Frequency") + 
  theme_minimal()

sum(sample(1:6,1), sample(1:6,1))

# create empty dataframe of 10,000 values
roll_vector <- vector("double", 10000)

# for each element in roll_vector
for (i in seq_along(roll_vector)) {
  # the element equals the sum of two dice
  roll_vector[i] = sum(sample(1:6,1), sample(1:6,1))
}

# turn the resulting vector into a dataframe to plot
roll_df <- as_data_frame(table(roll_vector)/10000)
# name the variables roll, and freq
colnames(roll_df) <- c("roll", "freq")
# turn the table-created roll variable into a numeric
roll_df$roll <- as.numeric(roll_df$roll)

# plot the relative frequencies 
ggplot(roll_df) + 
  geom_bar(aes(roll, freq),
           stat = "identity",
           color = "mediumpurple4",
           fill = "mediumpurple4"
  ) + 
  scale_x_continuous(name = "Sum of Dice",
                     breaks = roll_df$roll, 
                     labels = roll_df$roll
  ) + 
  ylab("Relative Frequency") + 
  ggtitle("10,000 Two-Dice Roll Relative Frequency") + 
  theme_minimal()






# Create the function "roll_em" to sum any number of dice for any number of rolls and produce a relative frequency plot as the output. 

# naming the function
roll_em <- function(n_dice, n_roll) {
  
  # check for input errors, both inputs must be non-zero integers  
  if (n_dice == 0 | is.na(n_dice) | n_dice %% 1 != 0) {
    stop("Enter a non-zero integer for the number of dice to roll")
  } 
  else if (n_roll == 0 | is.na(n_roll) | n_roll %% 1 != 0) {
    stop("Enter a non-zero integer for the number of rolls to perform")
  } 
  else {
    
    # if no errors are detected, move on to the body of the function
    
    # declare a counter, i, and set it to zero
    i <- 0  
    
    # create roll vector of user-specified length
    roll_vector <- vector("double", n_roll)
    
    
    # while the counter doesn't equal the user-specified length of roll_vector, loop through the following commands
    while (i <= length(roll_vector)) {
      
      # create a new dice_vector of user-specified length
      dice_vector <- vector("double", n_dice)
      
      # for each element j in dice vector, roll a single die and record the value
      for (j in seq_along(dice_vector)) {
        dice_vector[j] = sample(1:6,1)
      }
      
      # sum the values in the dice vector and put that value into the current roll_vector element
      roll_vector[i] <- sum(dice_vector)
      
      # add one iteration to the roll_vector counter
      i <- i + 1
      
      # repeat the sequence
    }
    
    
    # once all elements of the roll_vector are filled, create a roll dataframe
    roll_df <- as_data_frame(table(roll_vector)/n_roll)
    
    # name the columns, or variables, roll and freq (frequency)
    colnames(roll_df) <- c("roll", "freq")
    
    # ensure the roll numbers are numerics, instead of characters (default in table)
    roll_df$roll <- as.numeric(roll_df$roll)
    
    # plot the output
    ggplot(roll_df) + 
      geom_bar(aes(roll, freq),
               stat="identity", 
               color = "midnightblue",
               fill = "midnightblue"
      ) + 
      scale_x_continuous(name = "Sum of Dice",
                         breaks = roll_df$roll, 
                         labels = roll_df$roll
      ) + 
      ylab("Relative Frequency") + 
      ggtitle("Roll 'EM! Results") + 
      theme_minimal()
    
    # conclude else statement function
  }
  
  # conclude roll_em function
}

# test function
roll_em(5,200)
roll_em(0,1)
roll_em(2,500.1)
roll_em(502,7)
roll_em(5, 91572)

