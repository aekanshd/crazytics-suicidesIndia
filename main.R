# Import dplyr
library(dplyr)
library(magrittr)

dataset <- read.csv("dataset.csv")

# Select the President, Date, and Approve columns and filter to observations where President is equal to "Trump"
dataset %>%
  select(State,Type,Total) %>%
  filter(State == "Karnataka") %>%
  head()


