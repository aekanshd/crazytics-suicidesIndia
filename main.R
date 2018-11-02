# Import dplyr
library(dplyr)
library(magrittr)

dataset <- read.csv("dataset.csv")

# Select the President, Date, and Approve columns and filter to observations where President is equal to "Trump"
dataset %>%
  select(State,Type,Total) %>%
  filter(State == "Karnataka") %>%
  head()

newdata <- dataset %>%
            select(State,Type,Total) %>%
            group_by(Type) %>%
            summarise(Died = mean(Total))

deathByLoveAffairs <- dataset %>% 
                        select(State,Type,Total) %>%
                        filter(Type == "Love Affairs") %>%
                        pull(Total)

# Take a mean of the How Many Died
mean(deathByLoveAffairs)


