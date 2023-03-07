## modified from:
#https://stackoverflow.com/q/75642879/3677576

library(mlr3learners)
library(dplyr)
library(ggplot2)

set.seed(4123)
x <- 1:20
obs <- data.frame(
  x = rep(x, 3),
  f = factor(rep(c("a", "b", "c"), each = 20)),
  y = c(3 * dnorm(x, 10, 3), 5 * dlnorm(x, 2, 0.5), dexp(20 - x, .5)) + rnorm(60, sd = 0.02)
)

x_test <- seq(0, 20, length.out = 100)
test <- expand.grid(
  x = x_test,
  f = c("a", "b", "c"),
  y = c(3 * dnorm(x_test, 10, 3), 5 * dlnorm(x_test, 2, 0.5), dexp(20 - x_test, .5)) + rnorm(60, sd = 0.02)
)

dat <- rbind(obs, test)
task <- as_task_regr(dat, target = "y")
resampling <- rsmp("custom")
resampling$instantiate(task, list(train = 1:60), test = list(61:90060))
learner = lrn("regr.nnet", size=5, trace=FALSE)

learners <- replicate(10, learner$clone())
design <- benchmark_grid(
  tasks = task,
  learners = learners,
  resampling
)
bmr <- benchmark(design)

## evaluate quality criteria
bmr$aggregate()[learner_id == "regr.nnet"] # ok
bmr$aggregate(msr("time_train")) # works
# bmr$aggregate(msr("regr.rmse"), msr("regr.rsq"), msr("regr.bias")) # not possible

## select the best fit
i_best  <- which.min(bmr$aggregate()$regr.mse)
best    <- bmr$resample_result(i_best)

## do prediction
pr      <- as.data.table(best$predictions()[[1]])$response

## visualization
pred_test <-  test |>  mutate(y = pr)
ggplot(obs, aes(x, y)) + geom_point() +
  geom_line(data = pred_test, mapping = aes(x, y)) +
  facet_wrap(~f)


