library(RSQLite)
#problem1
#establishing database connnection
con <- dbConnect(RSQLite::SQLite(), "FinalDB.sqlite")
con

#creating tables
df1 <- dbExecute(con, 
                 "CREATE TABLE CROP_DATA (
                   CD_ID INTEGER NOT NULL,
                   YEAR DATE NOT NULL,
                   CROP_TYPE VARCHAR(20) NOT NULL,
                   GEO VARCHAR(20) NOT NULL, 
                   SEEDED_AREA INTEGER NOT NULL,
                   HARVESTED_AREA INTEGER NOT NULL,
                   PRODUCTION INTEGER NOT NULL,
                   AVG_YIELD INTEGER NOT NULL,
                   PRIMARY KEY (CD_ID)
                 )"
)

df2 <- dbExecute(con,
                 "CREATE TABLE FARM_PRICES(
                   CD_ID INTEGER NOT NULL,
                   DATE DATE NOT NULL,
                   CROP_TYPE VARCHAR(50) NOT NULL,
                   GEO VARCHAR(20) NOT NULL,
                   PRICE_PRERMT FLOAT NOT NULL,
                   PRIMARY KEY (CD_ID)
                 )",
                 errors = FALSE
)

df3 <- dbExecute(con, 
                 "CREATE TABLE DAILY_FX (
                   DFX_ID INTEGER NOT NULL,
                   DATE DATE NOT NULL,
                   FXUSDCAD FLOAT NOT NULL,
                   PRIMARY KEY (DFX_ID)
                 )"
)

df4 <- dbExecute(con, 
                 "CREATE TABLE MONTHLY_FX (
                   DFX_ID INTEGER NOT NULL,
                   DATE DATE NOT NULL,
                   FXUSDCAD FLOAT NOT NULL,
                   PRIMARY KEY (DFX_ID)
                 )"
)

dbListTables(con)


#problem 2: Read the datasets into R dataframes from the url provided and loading the tables in database
#Reading data
crop_data <- read.csv('https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBM-RP0203EN-SkillsNetwork/labs/Final%20Project/Annual_Crop_Data.csv')
farm_prices <- read.csv('https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBM-RP0203EN-SkillsNetwork/labs/Final%20Project/Monthly_Farm_Prices.csv')
daily_fx <- read.csv('https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBM-RP0203EN-SkillsNetwork/labs/Final%20Project/Daily_FX.csv')
monthly_fx <- read.csv('https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBM-RP0203EN-SkillsNetwork/labs/Final%20Project/Monthly_FX.csv')

#Loading into table
dbWriteTable(con,"CROP_DATA", crop_data, overwrite=TRUE, header=TRUE)
dbWriteTable(con,"FARM_PRICES", farm_prices, overwrite=TRUE, header=TRUE)
dbWriteTable(con,"DAILY_FX", daily_fx, overwrite=TRUE, header=TRUE)
dbWriteTable(con,"MONTHLY_FX", monthly_fx, overwrite=TRUE, header=TRUE)

#problem 3: How many records are in the farm prices dataset?
dbGetQuery(con, 'SELECT COUNT(CD_ID) FROM FARM_PRICES')

#problem 4: Which geographies are included in the farm prices dataset?
dbGetQuery(con, 'SELECT DISTINCT(GEO) FROM FARM_PRICES')

#problem 5: How many hectares of Rye were harvested in Canada in 1968?
dbGetQuery(con, 'SELECT SUM(HARVESTED_AREA) FROM CROP_DATA
                 WHERE CROP_TYPE="Rye" AND YEAR="1968-12-31" AND GEO="Canada"')

#problem 6: Query and display the first 6 rows of the farm prices table for Rye.
dbGetQuery(con, 'SELECT * FROM FARM_PRICES
                 WHERE CROP_TYPE="Rye"
                 LIMIT 6')

#problem 7: Which provinces grew Barley?
dbGetQuery(con, 'SELECT DISTINCT(GEO) FROM CROP_DATA
                 WHERE CROP_TYPE="Barley"')

#problem 8: Find the first and last dates for the farm prices data
dbGetQuery(con, 'SELECT MIN(DATE) AS FIRST_DATE, MAX(DATE) AS LAST_DATE 
                 FROM FARM_PRICES')

#problem 9: Which crops have ever reached a farm price greater than or equal to $350 per metric tonne?
dbGetQuery(con, 'SELECT CROP_TYPE, PRICE_PRERMT FROM FARM_PRICES
                WHERE PRICE_PRERMT >= 350')

#problem 10: Rank the crop types harvested in Saskatchewan in the year 2000 by their average yield.
#Which crop performed best?

dbGetQuery(con,'SELECT CROP_TYPE, AVG_YIELD 
                FROM CROP_DATA
                WHERE GEO="Saskatchewan" AND YEAR="2000-12-31"
                ORDER BY AVG_YIELD DESC ')

#problem 11: Rank the crops and geographies by their average yield (KG per hectare) since the year 2000. 
#Which crop and province had the highest average yield since the year 2000?

dbGetQuery(con, 'SELECT CROP_TYPE, GEO, AVG_YIELD FROM CROP_DATA
                WHERE YEAR >="2000"
                GROUP BY CROP_TYPE, GEO
                ORDER BY AVG_YIELD DESC')

#problem 12: Use a subquery to determine how much wheat was harvested in Canada in the most recent year of the data
dbGetQuery(con, 'SELECT YEAR, CROP_TYPE, GEO, HARVESTED_AREA FROM CROP_DATA
                 WHERE CROP_TYPE="Wheat" AND GEO="Canada" AND
                 YEAR= (SELECT MAX(YEAR) FROM CROP_DATA)')

#problem 13: Use an implicit inner join to calculate the monthly price per metric tonne of Canola grown
#in Saskatchewan in both Canadian and US dollars.
#Display the most recent 6 months of the data.
dbGetQuery(con,'SELECT A.DATE, (A.PRICE_PRERMT) AS CAD, (A.PRICE_PRERMT/B.FXUSDCAD) AS USD
                FROM FARM_PRICES AS A
                INNER JOIN MONTHLY_FX AS B USING(DATE)
                WHERE A.GEO= "Saskatchewan" AND A.CROP_TYPE="Canola"
                ORDER BY A.DATE DESC
                LIMIT 6')
')