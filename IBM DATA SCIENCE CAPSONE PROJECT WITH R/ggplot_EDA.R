library(tidyverse)
#loading seoul_bike_sharing 
seoul_bike_sharing <- read.csv("seoul_bike_sharing.csv")
#Task 1: date read as type character 
seoul_bike_sharing$DATE <- as.character(seoul_bike_sharing$DATE)
#Task 2 : Recast DATE as a date !! Use the format of the data, namely "%d/%m/%Y".
seoul_bike_sharing$DATE <- as.Date(seoul_bike_sharing$DATE, format = "%d/ %m /%Y")
### Task 3 - Cast `HOURS` as a categorical variable
seoul_bike_sharing$HOUR <- as.factor(seoul_bike_sharing$HOUR)
#Checking the sturcture of dataframe
str(seoul_bike_sharing)
#Ensuring there are no missing values
sum(is.na(seoul_bike_sharing))
#Summary of the dataset
summary(seoul_bike_sharing)
#Task 5: Counting NO. of holidays
holidays_count <- seoul_bike_sharing %>%
  filter(HOLIDAY=='Holiday') %>%
  count(HOLIDAY) %>%
  summarise(count=n/24)

holidays_count
#Task 6: Percentage of records that falls on holidays
total_records <- seoul_bike_sharing%>%
  count() %>%
  summarise(count=n/24)
holiday_per <- (holidays_count/total_records) *100
holiday_per
#Task 7: Given there is exactly a full year of data, determine how many records we expect to have.
total_records
### Task 8 - Given the observations for the 'FUNCTIONING_DAY' how many records must there be?
seoul_bike_sharing %>%
  count(FUNCTIONING_DAY)
### Task 9 - Load the dplyr package, group the data by `SEASONS`, and use the `summarize()` function to 
#calculate the seasonal total rainfall and snowfall.
library(dplyr)

 seoul_bike_sharing%>%
  group_by(SEASONS)%>%
  summarize(total_rainfall=sum(RAINFALL), total_snowfall=sum(SNOWFALL))
 
 #DATA VISUALIZATION
 library(ggplot2)
 
 ### Task 10 - Create a scatter plot of `RENTED_BIKE_COUNT` vs `DATE`.
 ggplot(seoul_bike_sharing, aes(x = DATE, y = RENTED_BIKE_COUNT)) +
   geom_point(alpha=0.4,color="red")
 
 ### Task 11 - Create the same plot of the `RENTED_BIKE_COUNT` time series, but now add `HOURS` as the colour.
 ggplot(seoul_bike_sharing, aes(x = DATE, y = RENTED_BIKE_COUNT,color=HOUR,alpha=0.25)) +
   geom_point( )
 
#### Task 12 - Create a histogram overlaid with a kernel density curve
# Normalize the histogram so the y axis represents 'density'. This can be done by setting `y=..density..` 
#in the aesthetics of the histogram.
 ggplot(seoul_bike_sharing, aes(x = RENTED_BIKE_COUNT)) +  # X-axis for rented bike count
   geom_histogram(aes(y = ..density..), color = "red",fill="lightgray", alpha = 1) +  # Histogram with density
   geom_density() +
   theme_bw()
 
 ## Correlation between two variables (scatter plot)
 ### Task 13 - Use a scatter plot to visualize the correlation between `RENTED_BIKE_COUNT` and `TEMPERATURE` by `SEASONS'
 ggplot(seoul_bike_sharing, aes(x=TEMPERATURE, y=RENTED_BIKE_COUNT,alpha=0.5))+
   geom_point(color="blue")+
   facet_wrap(~SEASONS)
 
 
 ## Outliers (boxplot)
 ### Task 14 - Create a display of four boxplots of `RENTED_BIKE_COUNT` vs. `HOUR` grouped by `SEASONS`.
 ggplot(seoul_bike_sharing, aes(x = HOUR, y = RENTED_BIKE_COUNT)) +
   geom_boxplot(fill = "bisque") +
   facet_wrap(~SEASONS)
 
 ### Task 15 - Group the data by `DATE`, and use the summarize() function to calculate the daily total rainfall and snowfall.
 seoul_bike_sharing %>%
   group_by(DATE) %>%
   summarize(total_rainfall = sum(RAINFALL), total_snowfall = sum(SNOWFALL)) %>%
   pivot_longer(!DATE) %>%
   ggplot(aes(x = DATE, y = value,fill=name)) +
   geom_col(alpha = 0.8)  
 
 
 
 ### Task 16 - Determine how many days had snowfall.
 seoul_bike_sharing%>%
   group_by(DATE) %>%
   summarize(daily_snowfall= sum(SNOWFALL)) %>%
   filter(daily_snowfall>0) %>%
   count()
   
 
 

 