library(rpart)
library(rpart.plot)
library(randomForest)
library(neuralnet)
library(tidymodels)
library(rstanarm)

final_data <- final_data %>%
  drop_na()

final_split <- initial_split(final_data, prop = 0.9)
final_train <- training(final_split)
final_test  <- testing(final_split)
final_folds <- vfold_cv(data = final_split %>% as_tibble(), v = 5)

error <- function(model){
  predict(model, newdata = final_test) %>%
    as_tibble() %>%
    bind_cols(final_test %>% 
                select(CPI)) %>%
    rmse(truth = CPI, estimate = value)
}

stan_model_1 <- stan_glm(CPI ~ gdp_pc + gini + govt_spending + poverty_rate + bur_rem +
                           pcf + infra_spend, 
                         data = final_train, 
         refresh = 0)

stan_1_error <- error(stan_model_1)

stan_model_2 <- stan_glm(data = final_train,
                         CPI ~ gdp_pc + gini*poverty_rate + govt_spending * infra_spend +
                           bur_rem + pcf, 
                         refresh = 0)

stan_2_error <- error(stan_model_2)

stan_model_3 <- stan_glm(CPI ~ log_gdp + gini + govt_spending + poverty_rate + bur_rem +
                           pcf + infra_spend, 
                         data = final_train, 
                         refresh = 0)

stan_3_error <- error(stan_model_3)

simple_stan <- stan_glm(data = final_train, 
                        CPI ~ gdp_pc + bur_rem + poverty_rate,
                        refresh = 0)

stan_4_error <- error(simple_stan)

bind_rows(stan_1_error, stan_2_error, stan_3_error, stan_4_error, 
          tree_error, rf_error, rf_log_error)

tree <- rpart(CPI ~ gdp_pc + gini + govt_spending + poverty_rate + bur_rem +
                pcf + infra_spend,
              data = final_train,
              cp = 0.1)

tree_error <- error(tree)

rpart.plot(tree,
           type = 1)

rf <- randomForest(CPI ~ gdp_pc + gini + govt_spending + poverty_rate + bur_rem +
                     pcf + infra_spend, 
                   data = final_train)

rf_log <- randomForest(CPI ~ log_gdp + gini + govt_spending + poverty_rate + bur_rem +
                     pcf + infra_spend, 
                   data = final_train)

rf_error <- error(rf)
rf_log_error <- error(rf_log)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

normalized_train <- final_train %>%
  select(-country) %>%
  normalize(.) 

normalized_test <- final_test %>%
  select(-country) %>%
  normalize(.) 

nn <- neuralnet(CPI ~ gdp_pc + gini + govt_spending + poverty_rate + bur_rem +
                  pcf + infra_spend,
          data = normalized_train,
          hidden = c(5, 2),
          linear.output = TRUE)

predict(nn, newdata = normalized_test) %>%
  as_tibble() %>%
  bind_cols(final_test %>% 
              select(CPI)) %>%
  rmse(truth = CPI, estimate = V1)