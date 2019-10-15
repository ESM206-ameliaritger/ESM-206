# -------------------------------------
# ESM 206 Lab 1 
# Basic data inport, wrangling, and viz
# Amelia Ritger
#  ------------------------------------

# assignment operator shortcut: alt - :: <-
# ctrl shift enter to run ALL lines of code

# Attach required packages 
library(tidyverse)

# Read in the hp_aggresion.csv file readr = package, read_csv = function
hp_data <- readr::read_csv("hp_aggression.csv")

# Checking out my data
View(hp_data)

# Get variable names
names(hp_data)

# Get a quick summary of my data
summary(hp_data)

# View top or bottom 'n' rows in a data frame
head(hp_data)
tail(hp_data)

# create subsets of a df by columns
# select :: COLUMNS
# filter :: ROWS
hp_ex_1 <- select(hp_data, character, book)
# now let's do the same thing using the pipe operator
# pipe operator shortcut: ctrl shift m :: %>% 
hp_ex_2 <- hp_data %>% 
  select(character, book)

# Example of selecting multiple sequential columns using ':'
hp_ex_3 <- hp_data %>% 
  select(abb:aggressions)

# Example of selecting multiple sequential columns excluding one column using '-'
hp_ex_4 <- hp_data %>%
  select(character:aggressions, -book)

# Set conditions to only keep observations from the book The Goblet of Fire
hp_ex_5 <- hp_data %>%
  filter(book == "The Goblet of Fire")

# To keep observations that match multiple conditions, the long way ("OR" statement)
hp_ex_6 <- hp_data %>%
  filter(abb == "harr" | abb == "vold")
# Or use a more concise way
hp_ex_7 <- hp_data %>%
  filter(abb %in% c("harr","vold","herm","ronw"))
# If I only want to keep observations where the book is The Deathly Hallows and the number of aggressions is greater than 5
hp_ex_8 <- hp_data %>%
  filter(book == "The Deathly Hallows", aggressions > 5)
# Other operators also work like <= >= etc

# Add new columns while keeping existing ones
hp_ex_9 <- hp_data %>%
  mutate(apm = aggressions/mentions)

# Find summary statistics by group, hierarchically first by book then by character abbreviation
hp_ex_10 <- hp_data %>%
  group_by(book, abb) %>%
  summarize(tot_agg = sum(aggressions),
            max_agg = max(aggressions),
            mean_agg = mean(aggressions))

# Linking multiple wranging steps with the pipe operator
# We only want to keep rows that contain observations for HP and Voldemort (and Hermione and Snape), we then want to only keep columns for character, book, and mentions, then we want to find the total number of mentions for each character
hp_ex_11 <- hp_data %>%
  filter(character %in% c("Harry", "Voldemort", "Hermione Granger", "Severus Snape")) %>%
  select(character, book, mentions) %>%
  group_by(character) %>%
  summarize(total = sum(mentions))
  
# Tell R things for a bare minimum graph: 1. We're using ggplot2 2. Which data we're using, including what are the x/y variables 3. What type of graph (geom) we want to create
ggplot(data = hp_ex_11, aes(x = character, y = total)) +
  geom_col() +
  labs(x="Character", y="Total mentions", title="HP aggressions") +
  coord_flip()

# Example of a scatterplot
ggplot(data = hp_data, aes(x=mentions, y=aggressions)) +
  geom_point(aes(color=book)) +
  theme_bw()

# Histogram
ggplot(data=hp_data, aes(x=aggressions)) +
  geom_histogram()
