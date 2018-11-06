# Import dplyr
library(dplyr)
library(magrittr)
library(lubridate)
library(zoo)
library(ggplot2)

dataset <- read.csv("dataset.csv")

# Select the President, Date, and Approve columns and filter to observations where President is equal to "Trump"
# Select State, Type, and Total Deaths
selectFew <- dataset %>%
  select(State,Type,Total) %>%
  filter(State == "Karnataka") %>%
  head()

write.csv(selectFew, "selectFew.csv")

dummyEx <- fastDummies::dummy_cols(dataset)

write.csv(dummyEx, "categories_encoded.csv")

womenDied <- dummyEx %>%
              select(Year, Gender_Female, Total) %>%
              group_by(Year) %>%
              summarise(Died = mean(Total))
ggplot(data = womenDied, aes(x=Year,y=Died)) + 
  geom_line()

write.csv(womenDied, "womenDied.csv")

normalized<-function(y) {
  
  x<-y[!is.na(y)]
  
  x<-(x - min(x)) / (max(x) - min(x))
  
  y[!is.na(y)]<-x
  
  return(y)
}

womenDiedNorm <- cbind(womenDied[1], apply(womenDied[,ncol(womenDied)],2,normalized))

write.csv(womenDiedNorm, "womenDied_Normalised.csv")

library("ggpubr")
ggqqplot(womenDiedNorm$Died)
ggdensity(womenDiedNorm$Died, 
          main = "Density plot of Women Died",
          xlab = "Women Died")

shapiro.test(womenDiedNorm$Died)

qqnorm(womenDiedNorm$Died, pch = 1, frame = FALSE)
qqline(womenDiedNorm$Died, col = "red", lwd = 2)

library("car")
qqPlot(womenDiedNorm$Died)

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


