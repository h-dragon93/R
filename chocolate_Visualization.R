# setting working directory
setwd("C:/Users/tyy/Desktop/Data_analyzing/Chocolate")

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

## finish Data Cleansing

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

# graphing data
ggplot(chocolate, aes(x= Review.Date, y = Rating)) + geom_point() 
# move each point just a little bit so we can actually see how many of them there are
ggplot(chocolate, aes(x= Review.Date, y = Rating)) + geom_point() + geom_jitter()+geom_smooth(method = 'lm')


# Rating by Review Date with Cocoa percent.
# see how the cocoa percentage of chocolate bars changes over time and how that affects ratings.
chocolategraph <- ggplot(chocolate, aes(x= Review.Date, y = Rating, color = Cocoa.Percent)) + geom_point() + geom_jitter()+geom_smooth(method = 'lm')

chocolategraph

#############  result 1 #############
## Zoom in graph and we can see that chocolate bars with very high cocoa percents tend to get lower ratings

#saving Image
ggsave("chocolategraph.png")

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
as.data.frame(average_ratings)

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

############### result 2 ##################
##  On average, Madagascar has highest rating point than other contries.
##  From now on, we will see how accurate it is Statiscally.
