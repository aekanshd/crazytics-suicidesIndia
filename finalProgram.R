dataset <- read.csv("dataset.csv")

# Select State (Karnataka), Type, and Total Deaths
dataset %>%
  select(State,Type,Total) %>%
  filter(State == "Karnataka") %>%
  head()

# Check for missing values
apply(dataset, 2, function(x) any(is.na(x)))

# Find out number of entries
nrow(dataset)

# take 10% sample
sampleData <- dataset[sample(nrow(dataset), 0.1*nrow(dataset)), ]
sampleDataRowNumbers <- as.numeric(rownames(sampleData))

# Find out number of entries in sample
nrow(sampleData)

# Delete the data from 5th column
dataset[sampleDataRowNumbers,5] <- NA

# Check for missing values
apply(dataset, 2, function(x) any(is.na(x)))

#View all the columns of the sample data
dataset[sampleDataRowNumbers,]

#Replace the value with the previous value
fixedDataset <- dataset %>% mutate(Gender = na.locf(Gender))

#View all the columns of the sample data
fixedDataset[sampleDataRowNumbers,]

# Encode all the categorical data in dummy variables
library(fastDummies)
encodedData <- fastDummies::dummy_cols(fixedDataset)

# Number of women comitting suicide regardless of cause
womenDied <- encodedData %>%
  select(Year, Gender_Female, Type_code, Total) %>%
  filter(Gender_Female == 1, Total > 0, Type_code == "Causes")

# Number of women comitting suicide per year
aggWomen <- as.data.frame(aggregate(womenDied$Total, by=list(Category=womenDied$Year), FUN=sum))

means <- mean(aggWomen$x)
sds <- sd(aggWomen$x)


scaled.dat <- scale(as.data.frame(aggWomen[,2]))

# Normalise the data using formula
womenDiedNorm <- as.data.frame(apply(as.data.frame(aggWomen[,2]), 2, function(x) (x-min(x))/(max(x)-min(x))))

# Plot a Q-Q Graph for the normalised data
library(ggpubr)
ggqqplot(womenDiedNorm$`aggWomen[, 2]`)

# Plot the trend of women commiting suicide
library(ggplot2)
ggplot(data = aggWomen, aes(x=Category, y=x, col=Category)) + 
  geom_line()

# Number of people comitting suicide regardless of thier age
ageDied <- encodedData %>%
  select(Year, Type_code, Age_group, Total) %>%
  filter(Total > 0, Type_code == "Causes")

# Number of people comitting suicide per age group per year
aggAgeDied <- as.data.frame(aggregate(ageDied$Total, by=list(Category=ageDied$Age_group), FUN=sum))

# Plot the graph of the trends of suicide by age groups
barplot(aggAgeDied$x, main="Suicides By Age Group", 
        names.arg = aggAgeDied$Category,
        ylab = "Number of Deaths",
        xlab = "Age Group")

# Number of suicides regardless of thier cause
peopleDied <- encodedData %>%
  select(Year, Type_code, Type, Total) %>%
  filter(Total > 0, Type_code == "Causes", Type != "Causes Not known", Type != "Other Causes (Please Specity)")

# Number of students comitting suicide  regardless of thier cause per year
aggPeopleDied <- as.data.frame(aggregate(peopleDied$Total, by=list(Category=peopleDied$Type), FUN=sum))

# Plot a bar plot
bp<- ggplot(aggPeopleDied, aes(x="", y=aggPeopleDied$x, fill=aggPeopleDied$Category))+
  geom_bar(width = 1, stat = "identity")

# Plot a pie chart
pie <- bp + coord_polar("y", start=0)
