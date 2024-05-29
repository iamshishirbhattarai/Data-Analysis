library(tidyverse)

bike_sharing_df <- read_csv("raw_seoul_bike_sharing.csv")

summary(bike_sharing_df)
dim(bike_sharing_df)

#DETECTING AND HANDLING MISSING VALUES

# Drop rows with `RENTED_BIKE_COUNT` column == NA
bike_sharing_df <- bike_sharing_df%>%
                    drop_na(RENTED_BIKE_COUNT)

dim(bike_sharing_df)


#Let's look over 'TEMPERATURE' column

bike_sharing_df %>% 
  filter(is.na(TEMPERATURE))

#As the NA value lies in the Summer season (month 6,7 and 8), calculating mean temperature in the summer season
summer_avg_temp <- bike_sharing_df %>%
  filter(SEASONS == "Summer") %>%
  select(TEMPERATURE) %>%
  summarise(mean(TEMPERATURE, na.rm = TRUE)) %>%
  unlist() %>%
  unname()

# Impute missing values for TEMPERATURE column with summer average temperature
bike_sharing_df$TEMPERATURE <- replace_na(bike_sharing_df$TEMPERATURE, summer_avg_temp)

# Print the summary of the dataset again to make sure no missing values in all columns
summary(bike_sharing_df)

#saving
write.csv(bike_sharing_df, "seoul_bike_sharing.csv")


#CREATING INDICATOR(DUMMY) VARIABLES FOR CATEGORICAL VARIABLES

# Using mutate() function to convert HOUR column into character type

bike_sharing_df <- bike_sharing_df%>%
                            mutate(HOUR= as.character(HOUR))

#Convert `SEASONS`, `HOLIDAY`, `FUNCTIONING_DAY`, and `HOUR` columns into indicator columns.
# Define columns to convert into indicator columns
col <- c("SEASONS", "HOLIDAY", "HOUR")

# Function to convert specified columns into indicator columns
convert_to_indicator <- function(data, columns) {
  for (col in columns) {
    data <- data %>%
      mutate(dummy = 1) %>%
      spread(key = col, value = dummy, fill = 0) # %>%
      #select(-dummy)  # Remove the temporary dummy column
  }
  return(data)
}

# Apply the function to your dataframe
bike_sharing_dummy <- convert_to_indicator(bike_sharing_df, col)

# Summary of the resulting dataframe
summary(bike_sharing_dummy)

#Saving
write_csv(bike_sharing_dummy, "seoul_bike_sharing_converted.csv")



#NORMALIZATION

#Apply min-max normalization on `RENTED_BIKE_COUNT`, `TEMPERATURE`, `HUMIDITY`, `WIND_SPEED`, `VISIBILITY`, `DEW_POINT_TEMPERATURE`, `SOLAR_RADIATION`, `RAINFALL`, `SNOWFALL`

min_max_normalize <- function(x){
  (x-min(x))/(max(x)-min(x))
}

normalized_df <- bike_sharing_dummy%>%
  mutate(across(c(RENTED_BIKE_COUNT,TEMPERATURE,HUMIDITY,WIND_SPEED,VISIBILITY,DEW_POINT_TEMPERATURE,SOLAR_RADIATION,RAINFALL,SNOWFALL),
         min_max_normalize))

print(normalized_df)

write_csv(normalized_df, "seoul_bike_sharing_converted_normalized.csv")

#standarizing new datasets
# Dataset list
dataset_list <- c('seoul_bike_sharing.csv', 'seoul_bike_sharing_converted.csv', 'seoul_bike_sharing_converted_normalized.csv')

for (dataset_name in dataset_list){
  # Read dataset
  dataset <- read_csv(dataset_name)
  # Standardized its columns:
  # Convert all columns names to uppercase
  names(dataset) <- toupper(names(dataset))
  # Replace any white space separators by underscore, using str_replace_all function
  names(dataset) <- str_replace_all(names(dataset), " ", "_")
  # Save the dataset back
  write.csv(dataset, dataset_name, row.names=FALSE)
}


print(seoul_bike_sharing.csR)
