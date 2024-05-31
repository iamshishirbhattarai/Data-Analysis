#import libraries
library(tidymodels)
library(tidyverse)
library(gridExtra)

#task 1
#downloading NOAA weather Dataset
download.file('https://dax-cdn.cdn.appdomain.cloud/dax-noaa-weather-data-jfk-airport/1.1.4/noaa-weather-sample-data.tar.gz', destfile = "NOAAWeather.gz")
#untaring the file
untar("NOAAWeather.gz")

#task 2: extracting and reading into project
weather_data <- read.csv("noaa-weather-sample-data/jfk_weather_sample.csv")
head(weather_data)
glimpse(weather_data)

#task 3: Select subsets of Columns
weather_data_modified <- select(weather_data, c('HOURLYRelativeHumidity','HOURLYDRYBULBTEMPF','HOURLYPrecip','HOURLYWindSpeed','HOURLYStationPressure'))
head(weather_data_modified, 10)

#task 4: clean-up columns
#Inspecting unique values present in the column HOURLYPrecip
unique(weather_data_modified$HOURLYPrecip)
#Replace all the T values with "0.0" and "s" from values
weather_data_filtered <- weather_data_modified%>%
  mutate(HOURLYPrecip= str_replace_all(HOURLYPrecip, "T", "0.0")%>%
           str_remove_all("s$"))
unique(weather_data_filtered$HOURLYPrecip)

#task 5: checking the types of columns
#glimpse(weather_data_filtered)
#converting columns to Numerical Types
weather_data_converted <- weather_data_filtered%>%
  mutate(HOURLYPrecip= as.numeric(HOURLYPrecip))
#checking that all fields have numeric data types
glimpse(weather_data_converted)

#task 6: Rename columns
weather_data_renamed <- weather_data_converted%>%
  rename(
    relative_humidity= HOURLYRelativeHumidity,
    dry_bulb_temp_f= HOURLYDRYBULBTEMPF,
    precip= HOURLYPrecip,
    wind_speed= HOURLYWindSpeed,
    station_pressure= HOURLYStationPressure
  )
glimpse(weather_data_renamed)

#task 7: Exploratory Data Analysis
set.seed(1234)
split_data <- initial_split(weather_data_renamed, prop = 0.8)
training_set <- training(split_data)
test_set <- training(split_data)

#relative_humidity histogram
p1<- ggplot(training_set, aes(x=relative_humidity))+
  geom_histogram(bins=50,color="black", fill="skyblue")+
  theme_minimal()
#dry_bulb_temp_f histogram
p2 <- ggplot(training_set, aes(x=dry_bulb_temp_f))+
  geom_histogram(bins=50,color="black", fill="skyblue")+
  theme_minimal()
#precip
p3 <- ggplot(training_set, aes(x=precip))+
  geom_histogram(bins=50,color="black", fill="skyblue")+
  theme_minimal()
#wind_speed
p4 <- ggplot(training_set, aes(x=wind_speed))+
  geom_histogram(bins=50,color="black", fill="skyblue")+
  theme_minimal()
#station_pressure
p5 <- ggplot(training_set, aes(x=station_pressure))+
  geom_histogram(bins=50,color="black", fill="skyblue")+
  theme_minimal()
# Arrange the plots in a 3x2 grid
grid.arrange(p1, p2, p3, p4, p5, nrow=3, ncol=2)


#Linear regression for precip as response variable
#Taking relative_humidity
lm1 <- ggplot(training_set, aes(x=relative_humidity, y=precip))+
  geom_point()+
  geom_smooth(method="lm",
              formula= y~x
              )
lm1
#dry_bulb_temp_f
lm2 <- ggplot(training_set, aes(x=dry_bulb_temp_f, y=precip))+
  geom_point()+
  geom_smooth(method="lm",
              formula= y~x
  )
lm2
#wind_speed
lm3 <- ggplot(training_set, aes(x=wind_speed, y=precip))+
  geom_point()+
  geom_smooth(method="lm",
              formula= y~x
  )
lm3

lm4 <- ggplot(training_set, aes(x=station_pressure, y=precip))+
  geom_point()+
  geom_smooth(method="lm",
              formula= y~x
  )
lm4

grid.arrange(lm1,lm2,lm3,lm4)

#task:9 improving the above models
#model 1: Taking more features/ predictors
linear_model <- linear_reg(mode="regression", engine="lm")%>%
  fit(precip~. , data=training_set)

summary(linear_model$fit)

linear_model_trainResults <- linear_model%>%
  predict(new_data = training_set)%>%
  mutate(truth= training_set$precip)

linear_model_trainResults

linear_model_trainResults$.pred <- replace(linear_model_trainResults$.pred, linear_model_trainResults$.pred<0, 0)

rsq_linear_model <- rsq(linear_model_trainResults,
                        truth=truth,
                        estimate=.pred)

rmse_linear_model <- rmse(linear_model_trainResults,
                          truth=truth,
                          estimate=.pred)


#model 2: Taking polynomial model
poly_model <- linear_reg(mode= "regression", engine="lm")%>%
  fit(precip~ .+relative_humidity*relative*humidity*relative_humidity+ relative_humidity * wind_speed+ station_pressure* dry_bulb_temp_f* relative_humidity ,
      data=training_set)

summary(poly_model$fit)

poly_model_trainResults <- poly_model%>%
  predict(new_data = training_set)%>%
  mutate(truth= training_set$precip)

poly_model_trainResults

poly_model_trainResults$.pred <- replace(poly_model_trainResults$.pred, poly_model_trainResults$.pred<0, 0)

rsq_poly_model <- rsq(poly_model_trainResults,
                        truth=truth,
                        estimate=.pred)

rmse_poly_model <- rmse(poly_model_trainResults,
                          truth=truth,
                          estimate=.pred)


#task 10: Finding Best model, only using rmse value 
# Testing for linear model on test_set data
linear_model_testResults <- linear_model%>%
  predict(new_data = test_set)%>%
  mutate(truth= test_set$precip)

linear_model_testResults

linear_model_testResults$.pred <- replace(linear_model_testResults$.pred, linear_model_testResults$.pred<0, 0)

rmse_linear_modelTest <- rmse(linear_model_testResults,
                            truth=truth,
                            estimate=.pred)

#Testing for polynomial model on test_set data

poly_model_testResults <- poly_model%>%
  predict(new_data = test_set)%>%
  mutate(truth= test_set$precip)

poly_model_testResults

poly_model_testResults$.pred <- replace(poly_model_testResults$.pred, poly_model_testResults$.pred<0, 0)

rmse_poly_modelTest <- rmse(poly_model_testResults,
                        truth=truth,
                        estimate=.pred)

rmse_linear_model
rmse_poly_model
rmse_linear_modelTest
rmse_poly_modelTest

model_names <- c("Linear_model", "Polynomial_model")
train_error <- c(0.0422, 0.0418)
test_error <- c(0.0422, 0.0418)
comparision_df <- data.frame(model_names, train_error, test_error)

comparision_df
