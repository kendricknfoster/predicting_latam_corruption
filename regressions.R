library(rpart)
library(rpart.plot)
library(randomForest)

stan_glm(data = final_data,
         CPI ~ `GDP Per Capita`*`Gini`*`Poverty Rate` + `Public Campaign Finance` + `Bureaucratic Remuneration` + `Government Spending` + `Infrastructure Spending`,
         refresh = 0) %>%
  print(digits = 5)

robust_model <- rlm(CPI ~ `GDP Per Capita`,
                    data = final_data)

tree <- rpart(CPI ~ `GDP Per Capita` + `Gini` + `Poverty Rate` + `Public Campaign Finance` + `Bureaucratic Remuneration` + `Government Spending` + `Infrastructure Spending`,
              data = final_data,
              cp = 0.1)

rpart.plot(tree,
           type = 1)

rf <- randomForest(CPI ~ `GDP Per Capita` + `Gini` + `Poverty Rate` + `Public Campaign Finance` + `Bureaucratic Remuneration`,
                   data = final_data)