# setting working directory
setwd("C:/Users/HanYong/Desktop/R/Assignment_chocolate")

# import library
library(dplyr)
library(ggplot2)

# load Data set
chocolate <- read.csv("flavors_of_cacao.csv")

# character vector to be coerced to syntactically valid names
names(chocolate) <- make.names(names(chocolate), unique=TRUE)

# rename  first column
chocolate <- rename(chocolate, Company.Maker.if.known = Company..Maker.if.known.)

# remove percentage signs in the Cocoa.Percent
chocolate$Cocoa.Percent <- sapply(chocolate$Cocoa.Percent, function(x) gsub("%", "", x))

## finish Data Cleansinglibrary(tidyverse)

library(tidyverse)
library(ggplot2)
library(readr)
library(stringr)
library(lubridate)
  ggplot(chocolate,aes(x = 'Cocoa.Percent', y = 'Rating')) +
  geom_jitter(alpha = .75) +
  coord_cartesian( ylim = c(0,5)) +
  labs(x = 'Cocoa Percentage', y = 'Rating') +
  theme_minimal() +
  geom_smooth(method = 'lm', se = FALSE, col ='red')

  
as.Date(paste(chocolate$Review.Date))  

model <- lm(formula = Rating ~ Cocoa.Percent + factor(year(Review.Date)),data = chocolate)
  
# check Dataset
View(chocolate)
head(chocolate)
tail(chocolate)
str(chocolate)
summary(chocolate)

# Average and standard deviation by year
chocolate_average_sd <- chocolate %>%
  group_by(Review.Date) %>%
  summarise(averageRating = mean(Rating),
            sd_Rating = sd(Rating))

chocolate_average_sd

ggplot(chocolate_average_sd, aes(x=Review.Date, y = sd_Rating)) + geom_point() + geom_line()

# graphing data
ggplot(chocolate, aes(x= Review.Date, y = Rating)) + geom_point() 
# move each point just a little bit so we can actually see how many of them there are
ggplot(chocolate, aes(x= Review.Date, y = Rating), ) + geom_point() + geom_jitter()+geom_smooth(method = 'lm')


# average rating by year. x = review date, y = average rating
averageRatingByYear <- chocolate %>%
  group_by(Review.Date) %>%
  summarise(averageRating = mean(Rating))

# plot individual points
ggplot(averageRatingByYear, aes(y= averageRating, x = Review.Date )) + 
  geom_point() + geom_line() 

# Now, we wanna check Rating by Bean origin
# and we wanna see the result sorted by its name

# So, i used variable "Broad.Bean.Origin"
table_origin <- table(chocolate$Broad.Bean.Origin)
table_origin
sort(table_origin, decreasing = T)

# This way is kind of uncomfortable to see. So i made it as data frame and arranged by Frequency
as.data.frame(table(chocolate$Broad.Bean.Origin)) %>% arrange(desc(Freq))

# To see the result with graph, i made it as boxplot.
Box_origin <- as.data.frame(table(chocolate$Broad.Bean.Origin)) %>% arrange(desc(Freq))
boxplot(Box_origin)

# We can see 5 points are outliers. This is what i'm looking for.
# I decided to compare Frequency of variable "Broad.Bean.Origin" with its average. 5 Origins have more than 100 Frequency.

average_rating_Venezuela <- chocolate %>% group_by(Broad.Bean.Origin) %>%
  summarise(mean_rate = mean(Rating)) %>% 
  filter(Broad.Bean.Origin == "Venezuela") 
average_rating_Ecuador <- chocolate %>% group_by(Broad.Bean.Origin) %>%
  summarise(mean_rate = mean(Rating)) %>% 
  filter(Broad.Bean.Origin == "Ecuador") 
average_rating_Peru <- chocolate %>% group_by(Broad.Bean.Origin) %>%
  summarise(mean_rate = mean(Rating)) %>% 
  filter(Broad.Bean.Origin == "Peru")
average_rating_Madagascar <- chocolate %>% group_by(Broad.Bean.Origin) %>%
  summarise(mean_rate = mean(Rating)) %>% 
  filter(Broad.Bean.Origin == "Madagascar")
average_rating_Dominican_Republic <- chocolate %>% group_by(Broad.Bean.Origin) %>%
  summarise(mean_rate = mean(Rating)) %>% 
  filter(Broad.Bean.Origin == "Dominican Republic")
# Because we can't use funtion filter continuously, i used it separately and combinded.
average_ratings <- bind_rows(average_rating_Venezuela, average_rating_Ecuador, average_rating_Peru ,average_rating_Madagascar, average_rating_Dominican_Republic)

# get data frame with ratings
data_frame_average <- as.data.frame(average_ratings)

# get data frame with Bean Origin
data_frame_Freq <- as.data.frame(table(chocolate$Broad.Bean.Origin)) %>% arrange(desc(Freq)) %>% head(5)

# Rename column
data_frame_Freq <- rename(data_frame_Freq, Broad.Bean.Origin = Var1)

# Combine data frames
left_join(data_frame_average, data_frame_Freq, by = "Broad.Bean.Origin")

# make it as dataframe
df <- left_join(data_frame_average, data_frame_Freq, by = "Broad.Bean.Origin")
as.data.frame(df)

# see the result with graph
ggplot(data = df, aes(x = Broad.Bean.Origin, y = mean_rate)) + geom_col() + coord_flip()
# This way is hard to recongsize. Let's see more precisely
ggplot(data = df, aes(x = Broad.Bean.Origin, y = mean_rate)) + geom_point()

############### result 1 ##################
##  On average, Madagascar has higher rating point than Ecuador.
##  On average, Venezuela has higher rating point than Peru.
##  From now on, we will see how accurate it is Statiscally.

# t-test

chocolate_diff <- chocolate %>%
  select(Broad.Bean.Origin, Rating) %>%
  filter(Broad.Bean.Origin %in% c("Madagascar", "Ecuador"))

chocolate_diff
table(chocolate_diff$Broad.Bean.Origin)

t.test(data = chocolate_diff, Rating ~ Broad.Bean.Origin, var.equal = T)

## p-value = 0.01379?? ?̹Ƿ? ???? ?? ???̰? ??????��?? ��????
## 95 percent confidence interval: -0.23472530 , -0.02687913
## Average rating of Madagascar's choloate is higher than Ecuador's

# t-test

chocolate_diff1 <- chocolate %>%
  select(Broad.Bean.Origin, Rating) %>%
  filter(Broad.Bean.Origin %in% c("Venezuela", "Peru"))

chocolate_diff1
table(chocolate_diff1$Broad.Bean.Origin)

t.test(data = chocolate_diff1, Rating ~ Broad.Bean.Origin, var.equal = T)

## p-value = 0.03878 ?̹Ƿ? ???? ?? ???̰? ??????��?? ��????
## 95 percent confidence interval: -0.209328908 , -0.005567722
## Average rating of Venezuela's choloate is higher than Peru's

# Correlation
chocolate$Cocoa.Percent <- as.numeric(chocolate$Cocoa.Percent)

cor.test(chocolate$Rating, chocolate$Cocoa.Percent)

## p-value = 2.122e-12 ?̹Ƿ? ???? ?? ???̰? ??????��?? ��????
# cor  :  -0.1648202 
# There are few correlation between Cocoa.Percent and Rating

# Let's see this with graph. Before, we made a graph about review date and rating and i will
# add Cocoa.Percent with using option 'color'
# Let's see Rating by Review Date with Cocoa percent.
# see how the cocoa percentage of chocolate bars changes over time and how that affects ratings.
chocolategraph <- ggplot(chocolate, aes(x= Review.Date, y = Rating, color = Cocoa.Percent),ylim=c(0,5)) + geom_point() + geom_jitter()+geom_smooth(method = 'lm')

chocolategraph

#############  result 2 #############
## Zoom in graph 
## Although the total correlationship is week,
## In my opinion, it looks like that chocolate bars with very very high cocoa percents tend to get lower ratings

#saving Image
ggsave("chocolategraph.png")

chocolate %>%
  filter(Broad.Bean.Origin %in% c("Madagascar", "Ecuador")) %>%
  group_by(Broad.Bean.Origin) %>%
  mutate(Average_rating = mean(Rating)) %>%
  ggplot(aes(x = Rating, color = Broad.Bean.Origin)) +
  geom_density() +
  geom_vline(aes(xintercept = Average_rating, color = Broad.Bean.Origin))

choco_perm <- chocolate %>%
  filter(Broad.Bean.Origin %in% c("Madagascar", "Ecuador")) %>%
  mutate(Rating = sample(Rating, replace = T)) %>%
  group_by(Broad.Bean.Origin)

choco_perm1 <- chocolate %>%
  filter(Broad.Bean.Origin %in% c("Madagascar", "Ecuador")) %>%
  mutate(Rating = sample(Rating, replace = T)) %>%
  group_by(Broad.Bean.Origin)

choco_perm2 <- chocolate %>%
  filter(Broad.Bean.Origin %in% c("Madagascar", "Ecuador")) %>%
  mutate(Rating = sample(Rating, replace = T)) %>%
  group_by(Broad.Bean.Origin)

choco_perm %>%
  summarize(orig_mean = mean(Rating)) %>%
  head(n = 10)



repr <- function(df, r)
{
  copy <- data.frame()
  for (replication in 1:r)
  {
    df$rep <- replication
    copy <- rbind(df,copy)
  }
  copy
}

set.seed(321)
choco_copy <- chocolate %>%
  filter(Broad.Bean.Origin %in% c("Madagascar", "Ecuador")) %>%
  repr(100) %>%
  mutate(Rating.copy = sample(Rating, replace = T)) %>%
  group_by(rep, Broad.Bean.Origin)

choco_copy %>%
  summarize(orig_mean = mean(Rating), copy_mean = mean(Rating.copy)) %>%
  head(n = 10)

choco_diff <- choco_copy %>%
  summarize(orig_mean = mean(Rating), copy_mean = mean(Rating.copy)) %>%
  summarize(diff_rating = diff(orig_mean), diff_rating_copy = diff(copy_mean))

cor.test(choco_diff$diff_rating, choco_diff$diff_rating_copy)

head(choco_diff, n = 10)

ggplot(choco_diff, aes(x = diff_rating_copy)) +
  geom_histogram(binwidth = .02) +
  geom_vline(aes(xintercept = diff_rating), col = "red") +
  geom_text(aes(x=diff_rating - .03, y = 10, label = paste("original difference", round(diff_rating, 4)), color = "red", angle=0)) +
  ggtitle("Mean Differences")
  xlab("Difference of Means by Permution") +
  ylab("Count")


p_value <- choco_diff %>% summarize(mean(diff_rating <= diff_rating_copy))
p_value
