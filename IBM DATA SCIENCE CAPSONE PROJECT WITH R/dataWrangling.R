#DATA WWRANGLING

require("tidyverse")
library(tidyverse)

dataset_list <- c('raw_bike_sharing_systems.csv', 'raw_seoul_bike_sharing.csv', 'raw_cities_weather_forecast.csv', 'raw_worldcities.csv')

#Write a `for` loop to iterate over the above datasets and convert their column names 

for (dataset_name in dataset_list){
  # Read dataset
  dataset <- read_csv(dataset_name)
  # Standardized its columns:
  
  # Convert all column names to uppercase
  names(dataset) <- toupper(names(dataset))
  # Replace any white space separators by underscores, using the str_replace_all function
  names(dataset)<- str_replace_all(names(dataset)," ","_")
  # Save the dataset 
  write.csv(dataset, dataset_name, row.names=FALSE)
}

#Reading the resulting datasets back and check whether their column names follow the naming convention
for (dataset_name in dataset_list){
  # Print a summary for each data set to check whether the column names were correctly converted
  dataset<- read_csv(dataset_name)
  print(colnames(dataset))
}


# First load the dataset
bike_sharing_df <- read_csv("raw_bike_sharing_systems.csv")

# Print its head
head(bike_sharing_df)

# Select the four columns
sub_bike_sharing_df <- bike_sharing_df %>% select(COUNTRY, CITY, SYSTEM, BICYCLES)


#Let's see the types of the selected columnssub_bike_sharing_df %>% 
sub_bike_sharing_df %>% 
  summarize_all(class) %>%
  gather(variable, class)

# grepl searches a string for non-digital characters, and returns TRUE or FALSE
# if it finds any non-digital characters, then the bicyle column is not purely numeric
find_character <- function(strings) grepl("[^0-9]", strings)

#Let's try to find any elements in the `Bicycles` column containing non-numeric characters.
sub_bike_sharing_df %>% 
  select(BICYCLES) %>% 
  filter(find_character(BICYCLES)) %>%
  slice(0:10)

# Define a 'reference link' character class, 
# `[A-z0-9]` means at least one character 
# `\\[` and `\\]` means the character is wrapped by [], such as for [12] or [abc]
ref_pattern <- "\\[[A-z0-9]+\\]"
find_reference_pattern <- function(strings) grepl(ref_pattern, strings)

# Check whether the COUNTRY column has any reference links
sub_bike_sharing_df %>% 
  select(COUNTRY) %>% 
  filter(find_reference_pattern(COUNTRY)) %>%
  slice(0:10)

# Check whether the CITY column has any reference links
sub_bike_sharing_df %>% 
  select(CITY) %>% 
  filter(find_reference_pattern(CITY)) %>%
  slice(0:10)

# Check whether the System column has any reference links
sub_bike_sharing_df %>% 
  select(SYSTEM) %>% 
  filter(find_reference_pattern(SYSTEM)) %>%
  slice(0:10)

# remove reference link
remove_ref <- function(strings) {
  ref_pattern <- "\\[[A-Z0-9]+//]"
  # Replace all matched substrings with a white space using str_replace_all()
  sub_bike_sharing_df$CITY <- str_replace_all(sub_bike_sharing_df$CITY,ref_pattern," ")
  sub_bike_sharing_df$SYSTEM <- str_replace_all(sub_bike_sharing_df$SYSTEM,ref_pattern," ")
  # Trim the reslt if you want
  sub_bike_sharing_df$CITY <- trimws(sub_bike_sharing_df$CITY)
  sub_bike_sharing_df$COUNTRY <- trimws(sub_bike_sharing_df$COUNTRY)
  sub_bike_sharing_df$SYSTEM <- trimws(sub_bike_sharing_df$SYSTEM, )
  
  print(sub_bike_sharing_df)
  
}



sub_bike_sharing_df %>%
  select(CITY, SYSTEM, BICYCLES) %>%
  filter(find_reference_pattern(CITY) | find_reference_pattern(SYSTEM) | find_reference_pattern(BICYCLES))

head(sub_bike_sharing_df)

#Write a custom function using `stringr::str_extract` to extract the first digital substring match and convert it into numeric type For example, extract the value '32' from `32 (including 6 rollers) [162]`.
# Extract the first number
extract_num <- function(columns){
  # Define a digital pattern
  digitals_pattern <- "[^0-9]"
  # Find the first match using str_extract
  str_extract(columns,digitals_pattern)
  # Convert the result to numeric using the as.numeric() function
  columns <- as.numeric(columns)
}

# Use the mutate() function on the BICYCLES column

sub_bike_sharing_df <- sub_bike_sharing_df %>%
  mutate(BICYCLES = extract_num(BICYCLES))

#summary
summary(sub_bike_sharing_df$BICYCLES)

#Write dataset to `bike_sharing_systems.csv`
write.csv(sub_bike_sharing_df, "bike_sharing_systems.csv")

