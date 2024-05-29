install.packages("rlang")
install.packages("tidymodels")

library("tidymodels")
library("tidyverse")
library("stringr")

bike_sharing_df <- read.csv("seoul_bike_sharing_converted_normalized.csv")

bike_sharing_df <- bike_sharing_df %>% 
  select(-DATE, -FUNCTIONING_DAY)

# Use the `initial_split()`, `training()`, and `testing()` functions to split the dataset
# With seed 1234
set.seed(1234)
bike_sharing_split <- initial_split(bike_sharing_df, prop=3/4)
bike_sharing_training <- training(bike_sharing_split)
bike_sharing_test <- testing(bike_sharing_split)

# TASK: Build a linear regression model using weather variables only
# Use `linear_reg()` with engine `lm` and mode `regression`
lm_model_weather <- linear_reg(mode="regression", engine="lm" )

# Fit the model called `lm_model_weather`
# RENTED_BIKE_COUNT ~ TEMPERATURE + HUMIDITY + WIND_SPEED + VISIBILITY + DEW_POINT_TEMPERATURE + SOLAR_RADIATION + 
#RAINFALL + SNOWFALL,  with the training data

train_fit <- lm_model_weather %>%
  fit(RENTED_BIKE_COUNT ~ TEMPERATURE + HUMIDITY + WIND_SPEED + VISIBILITY + DEW_POINT_TEMPERATURE +
        SOLAR_RADIATION + RAINFALL + SNOWFALL,
          data= bike_sharing_training )

 print(train_fit)


# TASK: Build a linear regression model using all variables
#_TODO:_ Build a linear regression model called `lm_model_all` using all variables `RENTED_BIKE_COUNT ~ .`

lm_mode_all <- linear_reg(mode="regression",engine="lm")
all_fit <- lm_mode_all %>%
  fit(RENTED_BIKE_COUNT ~ . , data= bike_sharing_training)

summary(all_fit)


# Use predict() function to generate test results for `lm_model_weather` and `lm_model_all`
# and generate two test results dataframe with a truth column:
# test_results_weather for lm_model_weather model
# test_results_all for lm_model_all

test_results_weather <- train_fit %>%
  predict(new_data =bike_sharing_test)%>%
  mutate(truth= bike_sharing_test$RENTED_BIKE_COUNT)

head(test_results_weather)

all_train_results <- all_fit %>%
  predict(new_data= bike_sharing_test) %>%
  mutate(truth= bike_sharing_test$RENTED_BIKE_COUNT)

head(all_train_results)

#_TODO:_ Use `rsq()` and `rmse()` functions to calculate R-squared and RMSE metrics for the two test results
rsq_weather <- rsq(test_results_weather, truth=truth, estimate=.pred)
rmse_weather <- rmse(test_results_weather, truth= truth, estimate= .pred)
rsq_all <- rsq(all_train_results, truth= truth, estimate= .pred)
rmse_all <- rmse(all_train_results, truth= truth, estimate= .pred)

rsq_weather
rsq_all
rmse_weather
rmse_all  #the result shows lm_model_all is better, so taking the model !!

#_TODO:_ Sort the coefficient list in descending order and visualize the result using `ggplot` and `geom_bar` 
all_fit %>%
  tidy()%>%
  arrange(desc(abs(estimate)))

# Visualize the list using ggplot and geom_bar
all_fit %>%
  tidy()%>%
  filter(!is.na(estimate)) %>%
  ggplot(aes(x=fct_reorder(term, abs(estimate)),y=abs(estimate)))+
  geom_bar(fill="blue", stat="identity")+
  coord_flip()+
  xlab("variable")

