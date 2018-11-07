# Import dplyr
library(dplyr)
library(magrittr)
library(lubridate)
library(zoo)
library(ggplot2)

dataset <- read.csv("dataset.csv")
# Check for missing values
apply(dataset, 2, function(x) any(is.na(x)))

# Find out number of entries
nrow(dataset)
nrow(dataset[sample(nrow(dataset), 0.1*nrow(dataset)), ])

# take 10% sample
sampleData <- dataset[sample(nrow(dataset), 0.1*nrow(dataset)), ]
sampleData <- as.numeric(rownames(NAdata))

# Check for missing values
dataset[NAdata,5] <- NA
apply(dataset, 2, function(x) any(is.na(x)))

res<-do.call(cbind,lapply(lapply(dataset[,5:7],
                                 function(x) data.frame(x)),
                          function(x) x[sample(1:nrow(x),0.1*nrow(x)),])) 
dataset[,5][dataset[,5]%in%res[,1]]<-NA 
dataset[,6][dataset[,6]%in%res[,2]]<-NA 
dataset[,7][dataset[,7]%in%res[,3]]<-NA 
head(dataset)  


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


