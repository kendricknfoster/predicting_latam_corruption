library(rpart)
library(rpart.plot)
library(randomForest)
library(neuralnet)
library(tidymodels)
library(rstanarm)
library(gtsummary)
library(gt)

final_data <- final_data %>%
  drop_na()

set.seed(7)
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
                           pcf + infra_spend + prop_rights - 1, 
                         data = final_train, 
         refresh = 0)

stan_1_error <- error(stan_model_1) %>%
  mutate(model = "Multivariate Linear Model")

stan_model_2 <- stan_glm(data = final_train,
                         CPI ~ gdp_pc + gini*poverty_rate + govt_spending * infra_spend +
                           bur_rem + pcf + prop_rights - 1, 
                         refresh = 0)

tbl_regression(stan_model_2) %>%
  as_gt()

print(stan_model_2, digits = 3)


stan_2_error <- error(stan_model_2) %>%
  mutate(model = "Multivariate Linear Model with Interaction Terms")

stan_model_3 <- stan_glm(CPI ~ log_gdp + gini + govt_spending + poverty_rate + bur_rem +
                           pcf + infra_spend + prop_rights - 1, 
                         data = final_train, 
                         refresh = 0)

stan_3_error <- error(stan_model_3) %>%
  mutate(model = "Multivariate Linear Model with Log of GDP")

simple_stan <- stan_glm(data = final_train, 
                        CPI ~ gdp_pc + bur_rem + poverty_rate - 1,
                        refresh = 0)

stan_4_error <- error(simple_stan) %>%
  mutate(model = "Multivariate Linear Model with 3 Variables")

tree <- rpart(CPI ~ gdp_pc + gini + govt_spending + poverty_rate + bur_rem +
                pcf + infra_spend + prop_rights,
              data = final_train,
              cp = 0.1)

tree_error <- error(tree) %>%
  mutate(model = "Decision Tree")

rpart.plot(tree,
           type = 1)

rf <- randomForest(CPI ~ gdp_pc + gini + govt_spending + poverty_rate + bur_rem +
                     pcf + infra_spend + prop_rights, 
                   data = final_train)

rf_log <- randomForest(CPI ~ log_gdp + gini + govt_spending + poverty_rate + bur_rem +
                     pcf + infra_spend + prop_rights, 
                   data = final_train)

rf_error <- error(rf) %>%
  mutate(model = "Random Forest")

rf_log_error <- error(rf_log) %>%
  mutate(model = "Random Forest with Log of GDP")

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
                  pcf + infra_spend + prop_rights,
          data = normalized_train,
          hidden = c(5, 2),
          linear.output = TRUE)

nn_error <- predict(nn, newdata = normalized_test) %>%
  as_tibble() %>%
  bind_cols(normalized_test %>% 
              select(CPI)) %>%
  rmse(truth = CPI, estimate = V1) %>%
  mutate(.estimate = (.estimate * 51) + 28) %>%
  mutate(model = "Neural Net")

bind_rows(stan_1_error, stan_2_error, stan_3_error, stan_4_error, 
          tree_error, rf_error, rf_log_error, nn_error) %>%
  select(model, .estimate) %>%
  mutate(.estimate = round(.estimate, digits = 3)) %>%
  gt() %>%
  cols_label(.estimate = "RMSE",
             model = "Model") %>%
  tab_style(cell_borders(sides = "right"),
            location = cells_body(columns = vars(model))) %>%
  tab_style(cell_text(weight = "bold"),
            location = cells_body(columns = vars(model))) %>%
  cols_align(align = "center", columns = TRUE) %>%
  fmt_markdown(columns = TRUE) 

saveRDS(final_test, file = "final_test.RDS")
saveRDS(stan_model_2, file = "model.RDS")
  
