library(h2o)
library(h2otools)
h2o.init(ignore_config=TRUE, bind_to_localhost=FALSE)

cars <- h2o.importFile("https://s3.amazonaws.com/h2o-public-test-data/smalldata/junit/cars_20mpg.csv")

# Set predictors and response; set response as a factor
#cars["economy_20mpg"] <- as.factor(cars["economy_20mpg"])
predictors <- c("displacement", "power", "weight", "acceleration", "year")
response <- "power"


hyper_params <- list(
  ntrees = seq(50, 300, by = 50),
  max_depth = seq(10, 30, by = 10),
  min_rows = c(1, 5, 10),
  sample_rate = c(0.6, .7)
)

search_criteria <- list(
  strategy = "RandomDiscrete",
  max_models = 100,
  seed = 1234
)

grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_grid",
  x = predictors,
  y = response,
  training_frame = as.h2o(cars),
  hyper_params = hyper_params,
  search_criteria = search_criteria
)


grid_perf <- h2o.getGrid(
  grid_id = "rf_grid",
  sort_by = "R2",
  decreasing = TRUE
)

saveRDS(grid_perf, file = "grid_perf.RDS")




best_model <- h2o.getModel(grid_perf@model_ids[[1]])
perf <- h2o.performance(best_model, newdata = validation_data)
print(perf)




# 1st, train a big RF model
cars_drf <- h2o.randomForest(x = predictors, y = response,
                             training_frame = cars,
                             nfolds = 10,
                             ntrees = 500,
                             seed = 1234)

h2otools::automlModelParam(cars_drf)
h2o.performance(cars_drf)
h2o.r2(cars_drf)

tnn <- h2o.automl(x = predictors, y = response,
                  training_frame = cars,
                  include_algos = "DRF",
                  max_models = 2,
                  seed = 1234)
MODELS <- as.vector(tnn@leaderboard)
library(h2otools)
h2o.performance(h2o.getModel(MODELS[1]))
h2o.r2(h2o.getModel(MODELS[1]))
automlModelParam(h2o.getModel(MODELS[1]))

h2o.performance(h2o.getModel(MODELS[1]))
automlModelParam(h2o.getModel(MODELS[2]))
