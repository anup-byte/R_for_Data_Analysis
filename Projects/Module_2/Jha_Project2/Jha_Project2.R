# Assignment Part 1

# Load the necessary libraries
library(tidyverse)  # For data manipulation and analysis

# Question 1: Load the dataset '2015.csv' and  Display the first few rows of the dataset to confirm loading
data_2015 <- read.csv("2015.csv")
head(data_2015)

# Question 2: Display the column names of the dataset
names(data_2015)

# Question 3: Use the view function to view the dataset in a separte tab
view(data_2015)

# Question 4: Use the glimpse function to view the structure of the dataset
glimpse(data_2015)

# Question 5: Install and load the janitor package and Clean column names to make them consistent and R-friendly
library(janitor)     
data_2015 <- clean_names(data_2015)
data_2015

# Question 6: Select specific columns (country, region, happiness_score, freedom) to create a new dataframe
happy_df <- data_2015 %>% select(country, region, happiness_score, freedom)
head(happy_df)

# Question 7: Slice the first 10 rows to create 'top_ten_df'
top_ten_df <- happy_df %>% slice(1:10)
print(top_ten_df)

# Question 8: Filter rows where 'freedom' is less than 0.20 to create 'no_freedom_df'
no_freedom_df <- happy_df %>% filter(freedom < 0.20)
print(no_freedom_df)

# Question 9: Arrange rows in descending order by 'freedom' to create 'best_freedom_df'
best_freedom_df <- happy_df %>% arrange(desc(freedom))
best_freedom_df

# Question 10: Add a new column 'gff_stat' as the sum of 'family', 'freedom', and 'generosity'
data_2015 <- data_2015 %>%
  mutate(gff_stat = family + freedom + generosity)
print(head(data_2015 %>% select(country, family, freedom, generosity, gff_stat)))
data_2015

# Question 11: Group the data by 'region' and calculate summary statistics
regional_stats_df <- happy_df %>%
  group_by(region) %>%
  summarise(
    country_count = n(),
    mean_happiness = mean(happiness_score, na.rm = TRUE),
    mean_freedom = mean(freedom, na.rm = TRUE)
  )
print(regional_stats_df)

#------------------------------------------------------------------------------------#

# Assignment Part 2

# Question 12: Load the necessary libraries
library(tidyverse)  # For data manipulation and analysis

# Load the dataset 'baseball.csv'
baseball <- read.csv("baseball.csv")
baseball


# Question 13: Exploratory Data Analysis on 'baseball' dataset
glimpse(baseball)


# Question 14: Remove rows where the number of at-bats (AB) is 0
baseball <- baseball %>% filter(AB > 0)
print(head(baseball))

# Question 15: Add a new column 'BA' for batting average (H / AB)
baseball <- baseball %>%
  mutate(BA = H / AB)
head(baseball %>% select(Last, First, AB, H, BA))

# Question 16: Add a new column 'OBP' for on-base percentage ((H + BB) / (AB + BB))
baseball <- baseball %>%
  mutate(OBP = (H + BB) / (AB + BB))
print(head(baseball %>% select(Last, First, H, BB, AB, OBP)))

# Question 17: Identify the top 10 players with the most strikeouts (SO)
strikeout_artist <- baseball %>%
  arrange(desc(SO)) %>%
  slice(1:10)
print(strikeout_artist)

# Question 18: Filter players eligible for end-of-season awards (at least 300 AB or 100 G)
eligible_df <- baseball %>%
  filter(AB >= 300 | G >= 100)
print(head(eligible_df))

# Question 19: Create a histogram of batting averages (BA) for eligible players
hist(eligible_df$BA, main = "Histogram of Batting Averages (BA)",
     xlab = "Batting Average", ylab = "Frequency", col = "skyblue", border = "black")

# Question 20: Analyze eligible players to choose the MVP
# Example: Selecting the player with the highest combination of HR, RBI, and OBP
mvp_candidate <- eligible_df %>%
  arrange(desc(HR + RBI + OBP)) %>%
  slice(1)
print(mvp_candidate)


#------------------------------------#
