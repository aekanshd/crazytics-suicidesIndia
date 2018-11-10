library(dplyr)
library(magrittr)
library(zoo)

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

# Number of students died
studentsDied <- encodedData %>%
  select(Year, Type_code, Age_group, Type, Total) %>%
  filter(Total > 0, Type_code == "Professional_Profile", Type == "Student")

# Number of students died per age grouo
aggstudentsDied <- as.data.frame(aggregate(studentsDied$Total, by=list(Category=studentsDied$Age_group), FUN=sum))


# People that died in the age group of 0-29
ageWisestudentsDied <- encodedData %>%
  select(Year, Type_code, Age_group, Type, Total) %>%
  filter(Total > 0, Age_group == "0-14" | Age_group == "15-29", Type_code == "Causes", Type != "Causes Not known", Type != "Other Causes (Please Specity)")

# Filtered by their causes
aggAgeWisestudentsDied <- as.data.frame(aggregate(ageWisestudentsDied$Total, by=list(Category=ageWisestudentsDied$Type), FUN=sum))

# Top 5 reasons to commit suicide
aggAgeWisestudentsDied[order(aggAgeWisestudentsDied$x,decreasing=T)[1:5],]

# People that died in the age group of 0-14
ageWisestudentsDied2 <- encodedData %>%
  select(Year, Type_code, Age_group, Type, Total) %>%
  filter(Total > 0, Age_group == "0-14", Type_code == "Causes", Type != "Causes Not known", Type != "Other Causes (Please Specity)")

# Filtered by their causes
aggAgeWisestudentsDied2 <- as.data.frame(aggregate(ageWisestudentsDied2$Total, by=list(Category=ageWisestudentsDied2$Type), FUN=sum))

# Top 5 reasons to commit suicide
aggAgeWisestudentsDied2[order(aggAgeWisestudentsDied2$x,decreasing=T)[1:5],]

# People that died in the age group of 15-29
ageWisestudentsDied3 <- encodedData %>%
  select(Year, Type_code, Age_group, Type, Total) %>%
  filter(Total > 0, Age_group == "15-29", Type_code == "Causes", Type != "Causes Not known", Type != "Other Causes (Please Specity)")

# Filtered by their causes
aggAgeWisestudentsDied3 <- as.data.frame(aggregate(ageWisestudentsDied3$Total, by=list(Category=ageWisestudentsDied3$Type), FUN=sum))

# Top 5 reasons to commit suicide
aggAgeWisestudentsDied3[order(aggAgeWisestudentsDied3$x,decreasing=T)[1:5],]

# People that died with Family Problems
familyProblemsDied <- encodedData %>%
  select(Year, Type_code, Age_group, Type, Total) %>%
  filter(Total > 0, Type_code == "Causes", Type == "Family Problems")

# Filtered by their causes
aggFamilyProblemsDied <- as.data.frame(aggregate(familyProblemsDied$Total, by=list(Category=familyProblemsDied$Year), FUN=sum))

# To test hypothesis
mutate(percent = Total/sum(Total))

# People that died 
hyp_died <- encodedData %>%
  select(Year, Type_code, Age_group, Type, Total) %>%
  filter(Total > 0, Type_code == "Causes", Type != "Causes Not known", Type != "Other Causes (Please Specity)")

# Filtered by their causes
agg_hyp_died <- as.data.frame(aggregate(hyp_died$Total, by=list(Category=hyp_died$Type), FUN=sum))
hyp_grouped <- group_by(hyp_died, Year) %>% group_by(Type)  %>% mutate(Died = Total, TotalDied = sum(Total))


# Find total number of deaths by cause
library(plyr)
groupColumns = c("Year","Type")
dataColumns = c("Total")
res <- ddply(hyp_died, groupColumns, function(x) colSums(x[dataColumns]))

# Total number of suicides per year
hyp_grouped <- aggregate(res$Total, by=list(Category=res$Year), FUN=sum)

# Add total number of deaths in year to res
res <- res %>% 
        rowwise %>% 
        do({
          result = as_data_frame(.)
          result$TotalDied = hyp_grouped[hyp_grouped$Category == result$Year, 2]
          result
        })

# Find percentage cause of each suicide.
res <- res %>% 
  rowwise %>% 
  do({
    result = as_data_frame(.)
    result$Percentage = (result$Total/(result$TotalDied))*100
    result
  })

# Find percentage of suicides by Family Problems each year.
hyp_familyProblems <- res %>%
  filter(Total > 0, Type == "Family Problems")

write.csv(hyp_familyProblems, "hyp_familyProblems.csv")

# Use t-test to find out if hypothesis is true or not
summary(hyp_familyProblems$Percentage)
t.test(hyp_familyProblems$Percentage, mu = 30, alternative = "less")
