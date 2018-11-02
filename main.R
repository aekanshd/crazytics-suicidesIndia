# Import dplyr
library(dplyr)
library(magrittr)
library(lubridate)
library(zoo)
library(ggplot2)

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
                        select(State,Year,Type,Total) %>%
                        filter(Type == "Love Affairs")

deathByHanging <- dataset %>% 
  select(State,Year,Type,Total) %>%
  filter(Type == "Family Problems")

# Take a mean of the How Many Died
mean(deathByHanging)

deathByHanging <- deathByHanging %>%
  mutate(AvgDeath = rollmean(Total, 10, na.pad=TRUE, align = "right"))

ggplot(data = deathByHanging, aes(x=Year,y=AvgDeath)) + 
  geom_line()

deathByLoveAffairs %>%
  group_by(Year) %>%
  summarise(Total = mean(Total)) 

deathByLoveAffairs %>%
  arrange(Year)

deathByLoveAffairs <- deathByLoveAffairs %>%
  mutate(AvgDeath = rollmean(Total, 10, na.pad=TRUE, align = "right"))

ggplot(data = deathByLoveAffairs, aes(x=Year,y=AvgDeath)) + 
  geom_line()

allDeaths <- dataset %>%
  group_by(Type) %>%
  mutate(AvgDeath = rollmean(Total, 10, na.pad=TRUE, align = "right"))

# Graph an moving average of each president's approval rating
ggplot(data = allDeaths, aes(x=Year, y=AvgDeath, col=Type)) + 
  geom_line()


