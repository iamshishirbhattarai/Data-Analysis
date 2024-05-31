library("tidymodels")
library("tidyverse")
library("stringr")

bike_sharing_df <- read.csv("seoul_bike_sharing_converted_normalized.csv")

bike_sharing_df <- bike_sharing_df %>% 
  select(-DATE, -FUNCTIONING_DAY)

lm_spec <- linear_reg() %>%
  set_engine("lm") %>% 
  set_mode("regression")

set.seed(1234)
data_split <- initial_split(bike_sharing_df, prop = 4/5)
train_data <- training(data_split)
test_data <- testing(data_split)

# TASK: Add polynomial terms
ggplot(data = train_data, aes(RENTED_BIKE_COUNT, TEMPERATURE)) + 
  geom_point() 

# Plot the higher order polynomial fits
ggplot(data=train_data, aes(RENTED_BIKE_COUNT, TEMPERATURE)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x, color="red") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color="yellow") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), color="green") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 6), color="blue")

#_TODO:_ Fit a linear regression model `lm_poly` with higher order polynomial terms on the important variables
#(larger coefficients) found in the baseline model

# Fit a linear model with higher order polynomial on some important variables 
# #HINT: Use ploy function to build polynomial terms, lm_poly <- RENTED_BIKE_COUNT ~ poly(TEMPERATURE, 6) + poly(HUMIDITY, 4) .....
lm_poly <- linear_reg(mode="regression",engine="lm") %>%
  fit(RENTED_BIKE_COUNT ~ poly(TEMPERATURE,6)+ poly(HUMIDITY, 4)+ poly(DEW_POINT_TEMPERATURE, 4)+
        RAINFALL*HUMIDITY+AUTUMN*HOLIDAY+HUMIDITY*TEMPERATURE, data= bike_sharing_training)

summary(lm_poly$fit)

#_TODO:_ Make predictions on test dataset using the `lm_poly` models
test_results <- lm_poly %>%
  predict(new_data= test_data)%>%
  mutate(truth=test_data$RENTED_BIKE_COUNT)

test_results

test_results %>%
  filter(test_results$.pred <0)

test_results$.pred <- replace(test_results$.pred, test_results$.pred <0, 0)

rsq_lm_poly <- rsq(test_results, truth=truth, estimate=.pred)

rmse_lm_poly <- rmse(test_results, truth=truth, estimate=.pred)

rsq_lm_poly

rmse_lm_poly


#ADD REGULARIZATION 

#_TODO:_ Define a linear regression model specification `glmnet_spec` using `glmnet` engine
library(glmnet)
library(rsample)
library(winch)

# Define the formula
lm_glmnet_formula <- formula(RENTED_BIKE_COUNT ~ . + poly(TEMPERATURE, 6) + 
                               poly(HUMIDITY, 6) + poly(DEW_POINT_TEMPERATURE, 4) + 
                               RAINFALL * HUMIDITY + HUMIDITY * TEMPERATURE + 
                               AUTUMN * HOLIDAY + AUTUMN * `18` + AUTUMN * `19` + 
                               AUTUMN * `8`)

# K-fold cross-validation
folds <- vfold_cv(bike_sharing_training, v = 10)

# Define model specification
tune_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

# Define workflow
wf <- workflow() %>%
  add_formula(lm_glmnet_formula) %>%
  add_model(tune_spec)

# Define tuning grid
lambda_tune <- grid_regular(levels = 50, penalty(range = c(-3, 0.3)))
mixture_tune <- grid_regular(levels = 50, mixture(range = c(0, 1)))

# Perform grid search
grid <- tune_grid(
  wf,
  resamples = folds,
  grid = expand.grid(lambda = lambda_tune, mixture = mixture_tune)
)

# Print grid search results
show_best(grid, metric = "rmse" )


#Experiment to search for imporved model

#observing correlations between variables
bike_sharing_df_cor <- cor(bike_sharing_df, method="pearson", use="pairwise.complete.obs")

bike_sharing_df_cor[,1]

# Build at least five different models using polynomial terms, interaction terms, and regularizations.

# Model 1 - I will keep the lm_poly model created before.

# Model 2 - Top cor variables, polynomial top variable, interaction term & L2 regulaizations

model2 <- linear_reg(penalty = 0.02, mixture = 1) %>%
  set_engine("glmnet")
model2_fit <- model2 %>%
  fit(RENTED_BIKE_COUNT ~ .+ poly(TEMPERATURE, 6) + WINTER + DEW_POINT_TEMPERATURE + SOLAR_RADIATION + SUMMER+ HUMIDITY, data = bike_sharing_training)

model2_train_results <- model2_fit %>%
  predict(new_data = bike_sharing_training) %>%
  mutate(truth = bike_sharing_training$RENTED_BIKE_COUNT)

model2$.pred <- replace(model2_train_results$.pred, model2_train_results$.pred < 0, 0)

rsq_model2 <- rsq(model2_train_results,
                  truth = truth,
                  estimate = .pred
)

rmse_model2 <- rmse(model2_train_results,
                    truth = truth,
                    estimate = .pred
)

# Model 3 - Top cor variables, 2 polynomial top variable, interaction term & elastic regularization

model3 <- linear_reg(penalty = 0.02, mixture = 0.2) %>%
  set_engine("glmnet")
model3_fit <- model3 %>%
  fit(RENTED_BIKE_COUNT ~ . + poly(TEMPERATURE, 6) + WINTER + poly(DEW_POINT_TEMPERATURE, 6) + SOLAR_RADIATION + SUMMER * + TEMPERATURE * HUMIDITY, data = bike_sharing_training)

model3_train_results <- model3_fit %>%
  predict(new_data = bike_sharing_training) %>%
  mutate(truth = bike_sharing_training$RENTED_BIKE_COUNT)

model3$.pred <- replace(model3_train_results$.pred, model3_train_results$.pred < 0, 0)

rsq_model3 <- rsq(model3_train_results,
                  truth = truth,
                  estimate = .pred
)

rmse_model3 <- rmse(model3_train_results,
                    truth = truth,
                    estimate = .pred
)

# Model 4 - Top cor variables,4 polynomial top variable, interaction term & elastic regularization

model4 <- linear_reg(penalty = 0.0015, mixture = 0.2) %>%
  set_engine("glmnet")
model4_fit <- model4 %>%
  fit(RENTED_BIKE_COUNT ~ . + poly(TEMPERATURE, 6) + WINTER+ poly(DEW_POINT_TEMPERATURE, 6) + poly(SOLAR_RADIATION, 6) + SUMMER + TEMPERATURE * HUMIDITY + poly(HUMIDITY, 6), data = bike_sharing_training)

model4_train_results <- model4_fit %>%
  predict(new_data = bike_sharing_training) %>%
  mutate(truth = bike_sharing_training$RENTED_BIKE_COUNT)

model4$.pred <- replace(model4_train_results$.pred, model4_train_results$.pred < 0, 0)

rsq_model4 <- rsq(model4_train_results,
                  truth = truth,
                  estimate = .pred
)

rmse_model4 <- rmse(model4_train_results,
                    truth = truth,
                    estimate = .pred
)

# Model 5 - Top cor variables,5 polynomial top variable, interaction term & elastic regularization

model5 <- linear_reg(penalty = 0.0015, mixture = 0.2) %>%
  set_engine("glmnet")
model5_fit <- model5 %>%
  fit(RENTED_BIKE_COUNT ~ . + poly(TEMPERATURE, 6) + WINTER+ poly(DEW_POINT_TEMPERATURE, 6) + poly(SOLAR_RADIATION, 6) + poly(VISIBILITY, 6) + SUMMER+ TEMPERATURE * HUMIDITY + poly(HUMIDITY, 6) + RAINFALL * TEMPERATURE + SNOWFALL * TEMPERATURE + RAINFALL * HUMIDITY + SNOWFALL * HUMIDITY, data = bike_sharing_training)

model5_train_results <- model5_fit %>%
  predict(new_data = bike_sharing_training) %>%
  mutate(truth = bike_sharing_training$RENTED_BIKE_COUNT)

model5_train_results$.pred <- replace(model5_train_results$.pred, model5_train_results$.pred < 0, 0)

rsq_model5 <- rsq(model5_train_results,
                  truth = truth,
                  estimate = .pred
)

rmse_model5 <- rmse(model5_train_results,
                    truth = truth,
                    estimate = .pred
)

# Save their rmse and rsq values

rsq_lm_poly
rmse_lm_poly
rsq_model2
rmse_model2
rsq_model3
rmse_model3
rsq_model4
rmse_model4
rsq_model5
rmse_model5


#grouped barchart for all rmse and rsq values
model_names <- c("lm_poly", "model2", "model3", "model4", "model5")
rsq <- c("0.528", "0.534", "0.704", "0.756", "0.770")
rsme <- c("0.122", "0.130", "0.101", "0.0897", "0.0875")
comparison_df <- data.frame(model_names, rsq, rsme)

comparison_df %>%
  pivot_longer(!model_names) %>%
  ggplot(aes(x = model_names, y = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparing RSME AND RSQ Across Models", fill = "Metric")

#QQ plot
ggplot(model5_train_results) +
  stat_qq(aes(sample = truth), color = "green") +
  stat_qq(aes(sample = .pred), color = "red")