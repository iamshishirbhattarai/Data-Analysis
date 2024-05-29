#sqlite connection
library(DBI)
library(RSQLite)
con <- dbConnect(RSQLite::SQLite(),"RDB.Sqlite")
print(con)

#Creating tables
#Creating for world_cities
world_cities_table <- "CREATE TABLE WORLD_CITIES(
  CITY VARCHAR(50),
  CITY_ASCII VARCHAR(50),
  LAT DECIMAL(20,2),
  LNG DECIMAL (20,2),
  COUNTRY VARCHAR(50),
  ISO2 VARCHAR(5),
  ISO3 VARCHAR(5),
  ADMIN_NAME VARCHAR(100),
  CAPITAL VARCHAR(50),
  POPULATION BIGINT,
  ID BIGINT NOT NULL
);"

dbExecute(con, world_cities_table)

#Creating for bike_sharing_systems
bike_sharing_table <- "CREATE TABLE BIKE_SHARING_SYSTEMS(
  COUNTRY VARCHAR(50),
  CITY VARCHAR(50),
  SYSTEM VARCHAR(50),
  BICYCLES NUMERIC
);"

#Creating for cities_weather_forecast
forecast_table <- "CREATE TABLE CITIES_WEATHER_FORECAST(
  CITY VARCHAR(50),
  WEATHER VARCHAR(50),
  VISIBILITY INT,
  TEMP DECIMAL(6,2),
  TEMP_MIN DECIMAL(6,2),
  TEMP_MAX DECIMAL(6,2),
  PRESSURE INT,
  HUMIDITY INT,
  WIND_SPEED DECIMAL(6,2),
  WIND_DEG INT,
  SEASON VARCHAR(6),
  FORECAST_DATETIME TIMESTAMP
);"

#Creating for Seoul_bike_sharing
seoul_bike_table <- "CREATE TABLE SEOUL_BIKE_SHARING(
  DATE VARCHAR(30),
  RENTED_BIKE_COUNT SMALLINT,
  HOUR SMALLINT,
  TEMPERATURE DECIMAL(4,1),
  HUMIDITY SMALLINT,
  WIND_SPEED DECIMAL(3,1),
  VISIBILITY SMALLINT,
  DEW_POINT_TEMPERATURE DECIMAL(4,1),
  SOLAR_RADIATION DECIMAL(5,2),
  RAINFALL DECIMAL(3,1),
  SNOWFALL DECIMAL(3,1),
  SEASONS VARCHAR(10),
  HOLIDAY VARCHAR(20),
  FUNCTIONING_DAY VARCHAR(5)
);"

#Executing tables
dbExecute(con, bike_sharing_table);
dbExecute(con, forecast_table)
dbExecute(con, seoul_bike_table)

#reading csv into R dataframe
world_cities <- read.csv("raw_worldcities.csv")
bike_sharing_systems <- read.csv("bike_sharing_systems.csv")
cities_weather_forecast <- read.csv("raw_cities_weather_forecast.csv")
seoul_bike_sharing <- read.csv("seoul_bike_sharing.csv")

#Loading datasets into SQLite Tables
dbWriteTable(con, "WORLD_CITIES", world_cities, overwrite=TRUE)
dbWriteTable(con, "BIKE_SHARING_SYSTEMS", bike_sharing_systems, overwrite=TRUE)
dbWriteTable(con, "CITIES_WEATHER_FORECAST", cities_weather_forecast, overwrite=TRUE)
dbWriteTable(con, "SEOUL_BIKE_SHARING", seoul_bike_sharing, overwrite=TRUE)

#Listing tables
dbListTables(con)

#TASK 1 : FIND NO. OF RECORDS IN THE SEOUL_BIKE_SHARING_DATASET
dbGetQuery(con, "SELECT COUNT(*) AS NO_OF_RECORDS FROM SEOUL_BIKE_SHARING")

#TASK 2: Determine how many hours had non-zero rented bike count.
dbGetQuery(con, 
           "SELECT COUNT(HOUR) FROM seoul_bike_sharing
           WHERE RENTED_BIKE_COUNT >0")

#TASK 3: Weather Outlook Query the the weather forecast for Seoul over the next 3 hours. 
#Recall that the records in the CITIES_WEATHER_FORECAST dataset are 3 hours apart, 
#so we just need the first record from the query.
dbGetQuery(con,
           "SELECT * FROM CITIES_WEATHER_FORECAST
           WHERE CITY='Seoul'
           LIMIT 1
            ")

#TASK 4: Find which seasons are included in the seoul bike sharing dataset
dbGetQuery(con, 
           "SELECT DISTINCT(SEASONS) AS SEASONS_INCLUDED
           FROM SEOUL_BIKE_SHARING")

#TASK 5: Find the first and last dates in the Seoul Bike Sharing dataset.
dbGetQuery(con,
           "SELECT MIN(DATE) AS FIRST_DATE, MAX(DATE) AS LAST_DATE
           FROM SEOUL_BIKE_SHARING")

#TASK 6: determine which date and hour had the most bike rentals
dbGetQuery(con,
           "SELECT DATE,HOUR,RENTED_BIKE_COUNT, SEASONS
           FROM SEOUL_BIKE_SHARING
           WHERE RENTED_BIKE_COUNT= (SELECT MAX(RENTED_BIKE_COUNT)
           FROM SEOUL_BIKE_SHARING)")

## Task 7 - Hourly popularity and temperature by season
#### Determine the average hourly temperature and the average number of bike rentals per hour over each season. 
#List the top ten results by average bike count.
dbGetQuery(con,
           "SELECT SEASONS, HOUR, AVG(RENTED_BIKE_COUNT),AVG(TEMPERATURE)
           FROM SEOUL_BIKE_SHARING
           GROUP BY SEASONS, HOUR
           ORDER BY AVG(RENTED_BIKE_COUNT) DESC
           LIMIT 10")

## Task 8 - Rental Seasonality
#### Find the average hourly bike count during each season. 
#Also include the minimum, maximum, and standard deviation of the hourly bike count for each season. 
dbGetQuery(con, "
  SELECT SEASONS,
         AVG(RENTED_BIKE_COUNT),
         MIN(RENTED_BIKE_COUNT),
         MAX(RENTED_BIKE_COUNT),
         SQRT(AVG(RENTED_BIKE_COUNT * RENTED_BIKE_COUNT) - AVG(RENTED_BIKE_COUNT) * AVG(RENTED_BIKE_COUNT)) AS STANDARD_DEVIATION
  FROM SEOUL_BIKE_SHARING
  GROUP BY SEASONS
")

## Task 9 - Weather Seasonality
#### Consider the weather over each season.
#On average, what were the TEMPERATURE, HUMIDITY, WIND_SPEED, VISIBILITY, DEW_POINT_TEMPERATURE, 
#SOLAR_RADIATION, RAINFALL, and SNOWFALL per season?
#Include the average bike count as well , and 
#rank the results by average bike count so you can see if it is correlated with the weather at all. 

dbGetQuery(con,
           "SELECT SEASONS,
           AVG(TEMPERATURE),
           AVG(HUMIDITY),
           AVG(WIND_SPEED),
           AVG(VISIBILITY),
           AVG(DEW_POINT_TEMPERATURE),
           AVG(SOLAR_RADIATION),
           AVG(RAINFALL),
           AVG(SNOWFALL),
           AVG(RENTED_BIKE_COUNT)
           FROM SEOUL_BIKE_SHARING
           GROUP BY SEASONS
           ORDER BY AVG(RENTED_BIKE_COUNT) DESC
           ")

### Task 10 - Total Bike Count and City Info for Seoul
#### Use an implicit join across the WORLD_CITIES and the BIKE_SHARING_SYSTEMS tables to determine the total number of bikes avaialble in Seoul,
#plus the following city information about Seoul: CITY, COUNTRY, LAT, LON, POPULATION, in a single view.

dbGetQuery(con, 
           "SELECT B.BICYCLES, B.CITY, B.COUNTRY, W.LAT, W.LNG, W.POPULATION 
            FROM BIKE_SHARING_SYSTEMS AS B 
            RIGHT JOIN WORLD_CITIES AS W ON B.CITY = W.CITY_ASCII 
            WHERE W.CITY_ASCII ='Seoul'"
)


## Task 11 - Find all city names and coordinates with comparable bike scale to Seoul's bike sharing system
#### Find all cities with total bike counts between 15000 and 20000. Return the city and country names, 
#plus the coordinates (LAT, LNG), population, and number of bicycles for each city.

dbGetQuery(con,
           "SELECT W.CITY, W.COUNTRY, W.LAT, W.LNG , W.POPULATION, B.BICYCLES 
            FROM BIKE_SHARING_SYSTEMS AS B
            INNER JOIN WORLD_CITIES AS W ON B.CITY= W.CITY_ASCII
            WHERE B.BICYCLES BETWEEN 15000 AND 20000
           ")

