# Load requisite libraries. 

library(rpart)
library(rpart.plot)
library(randomForest)
library(neuralnet)
library(tidymodels)
library(rstanarm)
library(gtsummary)
library(gt)

# I'm dropping the NA values from final_data so that the neural net and decision
# tree models especially will work and won't produce errors!

final_data <- final_data %>%
  drop_na()

# Divide the data into training and testing datasets. 

set.seed(7)
final_split <- initial_split(final_data, prop = 0.9)
final_train <- training(final_split)
final_test  <- testing(final_split)

# Set a function to find the RMSE from the final_test data. 

error <- function(model){
  predict(model, newdata = final_test) %>%
    as_tibble() %>%
    bind_cols(final_test %>% 
                select(CPI)) %>%
    rmse(truth = CPI, estimate = value)
}

# Define a simple stan_glm with no interaction terms. 

stan_model_1 <- stan_glm(CPI ~ gdp_pc + gini + govt_spending + poverty_rate + 
                           bur_rem + pcf + infra_spend + prop_rights - 1, 
                         data = final_train, 
         refresh = 0)

stan_1_error <- error(stan_model_1) %>%
  mutate(model = "Multivariate Linear Model")

# Define a glm with interaction terms between Gini & Poverty Rate and Government
# Spending & Infrastructure Spending. The stan_glm wouldn't work with the
# regression table, so I switch to a glm.

stan_model_2 <- glm(data = final_train,
                         CPI ~ gdp_pc + gini + poverty_rate + govt_spending * infra_spend +
                           bur_rem + pcf + prop_rights - 1)

tbl_regression(stan_model_2,
               estimate_fun = ~style_number(.x, digits = 4)) %>%
  as_gt()

stan_2_error <- error(stan_model_2) %>%
  mutate(model = "Multivariate Linear Model with Interaction Terms")

# Define a simple stan_glm but using the logarithm of GDP per capita. 

stan_model_3 <- stan_glm(CPI ~ log_gdp + gini + govt_spending + poverty_rate + bur_rem +
                           pcf + infra_spend + prop_rights - 1, 
                         data = final_train, 
                         refresh = 0)

stan_3_error <- error(stan_model_3) %>%
  mutate(model = "Multivariate Linear Model with Log of GDP")

# Define a stan_glm but with only three variables. 

simple_stan <- stan_glm(data = final_train, 
                        CPI ~ gdp_pc + bur_rem + poverty_rate - 1,
                        refresh = 0)

stan_4_error <- error(simple_stan) %>%
  mutate(model = "Multivariate Linear Model with 3 Variables")

# Define a decision tree model. 

tree <- rpart(CPI ~ gdp_pc + gini + govt_spending + poverty_rate + bur_rem +
                pcf + infra_spend + prop_rights,
              data = final_train,
              cp = 0.1)

tree_error <- error(tree) %>%
  mutate(model = "Decision Tree")

# Define a random forest model. 

rf <- randomForest(CPI ~ gdp_pc + gini + govt_spending + poverty_rate + bur_rem +
                     pcf + infra_spend + prop_rights, 
                   data = final_train)

# Define a random forest model using the log of GDP per capita. 

rf_log <- randomForest(CPI ~ log_gdp + gini + govt_spending + poverty_rate + bur_rem +
                     pcf + infra_spend + prop_rights, 
                   data = final_train)

rf_error <- error(rf) %>%
  mutate(model = "Random Forest")

rf_log_error <- error(rf_log) %>%
  mutate(model = "Random Forest with Log of GDP")

# Create a function to normalize the data to feed it into the neural net model. 

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Created a normalized set of the training and testing data so that I can mess
# with them separately and not mess up all the other models.

normalized_train <- final_train %>%
  select(-country) %>%
  normalize(.) 

normalized_test <- final_test %>%
  select(-country) %>%
  normalize(.) 

# Feed in everything to the Neural Net model. 

nn <- neuralnet(CPI ~ gdp_pc + gini + govt_spending + poverty_rate + bur_rem +
                  pcf + infra_spend + prop_rights,
          data = normalized_train,
          hidden = c(5, 2),
          linear.output = TRUE)

# For some reason, the regular error function wouldn't work on the neural net
# model, so I do the calculations the long way and de-normalize the resulting
# RMSE for more accurate comparisons with the other models.

nn_error <- predict(nn, newdata = normalized_test) %>%
  as_tibble() %>%
  bind_cols(normalized_test %>% 
              select(CPI)) %>%
  rmse(truth = CPI, estimate = V1) %>%
  mutate(.estimate = (.estimate * 51) + 28) %>%
  mutate(model = "Neural Net")

# I bind the different RMSE tables together, select the relevant columns, and
# take the code to make a table from Problem Set 3 for easy comparison between
# all the models that I ran.

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

# I save the testing file so that I can use it for code in the Shiny App and the
# model that I chose for the same reason.

saveRDS(final_test, file = "final_test.RDS")
saveRDS(stan_model_2, file = "model.RDS")
  
