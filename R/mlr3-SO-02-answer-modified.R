library(mlr3learners)
library(dplyr)
library(ggplot2)

set.seed(4123)
x_train <- 1:20
id_train <- 1:20
train <- data.frame(
  x = rep(x_train, 3), # as double is a workaround, may be removed later
  f = factor(rep(c("a", "b", "c"), each = 20)),
  y = c(3 * dnorm(x_train, 10, 3), 5 * dlnorm(x_train, 2, 0.5), dexp(20 - x_train, .5)) +
        rnorm(60, sd = 0.02)
)

x_test <- rep(jitter(x_train), 3) # 60 values, not exactly the same as x
id_test <- 61:240
test <- data.frame(
  x = x_test,
  f = factor(rep(c("a", "b", "c"), each=60)),
  y = c(3 * dnorm(x_test, 10, 3), 5 * dlnorm(x_test, 2, 0.5),
        dexp(20 - x_test, .5)) + rnorm(60, sd = 0.02)
)



dat <- rbind(train, test)
task <- as_task_regr(dat, target = "y")
resampling <- rsmp("custom")
resampling$instantiate(task, train = list(id_train), test = list(id_test))
learner = lrn("regr.nnet", size=5, trace=FALSE)

learners <- replicate(10, learner$clone())
design <- benchmark_grid(
  tasks = task,
  learners = learners,
  resampling
)
bmr <- benchmark(design, store_models = TRUE)

## evaluate quality criteria
bmr$aggregate()[learner_id == "regr.nnet"] # ok
bmr$aggregate(msr("time_train")) # works
# bmr$aggregate(msr("regr.rmse"), msr("regr.rsq"), msr("regr.bias")) # not possible

## select the best fit
i_best  <- which.min(bmr$aggregate()$regr.mse)
best    <- bmr$resample_result(i_best)

## work area
pred <- bmr$learners$learner[[1]]$predict(task)#, row_ids = test)


# Get the model from the first resampling iteration of this ResampleResult
nnet_model_raw <- best$learners[[1]]$model
coef(nnet_model_raw)

## do prediction

#pred_train  <-  train |>  mutate(y = best$predictions()[[1]]$response, subset="train")

pred_test <-  test |>  mutate(y = best$predictions()[[1]]$response, subset="test")

## visualization
ggplot(train, aes(x, y)) + geom_point() +
  geom_line(data = train, mapping = aes(x, y, color = subset)) +
  geom_line(data = pred_test, mapping = aes(x, y, color = subset)) +
  facet_wrap(~f)

