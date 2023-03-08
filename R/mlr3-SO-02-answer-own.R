#reprex::reprex({
library(mlr3learners)
set.seed(123)

data(iris) # to show that we use this data set

## half of indices for training and other for test subset
id_train  <- sample(1:nrow(iris), nrow(iris) %/% 2)
id_test   <- which(!((1:nrow(iris)) %in% id_train))

## create task, learner and custom resampling strategy
task <- as_task_regr(iris, target = "Petal.Width")
learner <- lrn("regr.nnet", size = 5, trace = FALSE)
resampling <- rsmp("custom")
resampling$instantiate(task, train = list(id_train), test = list(id_test))

## replicate the learners, create and run a benchmark design
learners <- replicate(10, learner$clone())
design <- benchmark_grid(
  tasks = task,
  learners = learners,
  resamplings = resampling
)

bmr <- benchmark(design, store_models = TRUE)

## summary of results and "best" result according to mse
bmr$aggregate(msrs(c("regr.mse", "regr.rmse", "regr.rsq", "time_train")))
(i_best <- which.min(bmr$aggregate()$regr.mse))
best <- bmr$resample_result(i_best)

## prediction for the test set
best$predictions()[[1]]$response

## or for both subsets
best$learners[[1]]$predict(task, row_ids = id_train)$response
best$learners[[1]]$predict(task, row_ids = id_test)$response

## extract raw model and the weights with coef() method from package "nnet"
best$learners[[1]]$model
coef(best$learners[[1]]$model)
#})
