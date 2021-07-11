#Project 2

library(dplyr)
library(ggplot2)
library(lubridate)
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')



ny$city <- "New York"
wash$city <- "Washington"
chi$city <- "Chicago"
df <- full_join(ny, wash, by = NULL)
df <- full_join(df, chi, by = NULL)
#to have all three datasets in one, you have to join them.



by(df$Trip.Duration/60, df$city, mean, na.rm = TRUE) 
#trip duration is measured in seconds. Minutes are easier to understand.

ggplot(aes(x = city, y = Trip.Duration/60), data = df) +
  geom_bar(stat = 'summary', fun.y = mean,  na.rm = TRUE, colour = "black", fill="purple4") +
  ggtitle('Average Travel Time') +
  labs(x = "City", y = "Trip Duration (in minutes)", size = 5, caption = "(based on data from Motivate Bike Share Data)") +
  geom_text(stat = 'summary',aes(label= round(..y.., digits = 1)), fun.y = mean, size = 4, vjust = -0.5, na.rm = TRUE)+
  theme_bw() 
#since it is easier to understand, the trip duration is displayed as minutes and rounded to one digit.



df$month <- lubridate::month(as.Date(df$Start.Time),label = T,abbr = T) 
# The timestamp in Washington isn't stored as date, 
# hence the warning at the joins. Using "as.Date" as solution for this problem. The month-function pulls only the month from 
# the timestamp. it wworks.
table(df$month)




ggplot(aes(x = month), data = df) +
  geom_bar(na.rm = TRUE, colour = "black", fill = "purple4") +
  ggtitle('Rental month')+
  labs(x = "Month", y = "Count", size = 5, caption = "(based on data from Motivate Bike Share Data)") +
  geom_text(stat = 'count', aes(label =..count..), vjust = -1, size = 2.5)+
  theme_bw() +
  facet_wrap(~ city)
             
             
             
             
levels(ny$Gender)[levels(ny$Gender)==""]<-NA
 # NA was blank space in the original variable "Gender", so it is easier. 
             
ggplot(aes(x = Gender), data = ny) +
  geom_bar(na.rm = TRUE, colour = "black", fill = "purple4") +
  ggtitle('Rental by gender')+
  labs(x = "Gender", y = "Count", size = 5, caption = "(based on data from Motivate Bike Share Data)") +
  geom_text(stat = 'count', aes(label =..count..), vjust = -1, size = 2.5)+
  theme_bw()

system('python -m nbconvert Explore_bikeshare_data.ipynb')

#Submission date: July 2021#