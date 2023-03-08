reprex <- purrr::partial(reprex::reprex, venue = "r")

reprex({

library("dplyr", warn.conflicts = FALSE)
library("nnet")
library("mlr3")
library("mlr3learners")
library("ggplot2")
set.seed(123)

## observation data, functional pattern + some random noise
x <- 1:20
obs <- data.frame(
  x = rep(x, 3),
  f = factor(rep(c("A", "B", "C"), each = 20)),
  y = c(3 * dnorm(x, 10, 3), 5 * dlnorm(x, 2, 0.5), dexp(20-x, .5))
      + rnorm(60, sd = 0.02)
)

## fit several networks and return the best
## this is of course a brute-force method that can lead to overtraining
nns <- lapply(1:100,
         \(foo) nnet(y ~ x * f, data = obs, size = 3, maxit=500, trace=FALSE))
nn <- nns[[which.min(lapply(nns, \(x) x$value))]]
nn
##
## Result: Structure of the network is 5-3-1,
## 1 * x, (3-1) * factor levels, 2 * interactions


## === neural network with mlr3, regr.nnet ====================================

## workaround: resolve "unsupported feature types: integer"
obs$x <- as.double(obs$x)

## create task and train model
task    <- as_task_regr(obs, target="y")
learner <- lrn("regr.nnet", size = 3, maxit = 500, trace = FALSE)
learner$train(task)
print(learner$model)
##
## Result: model has only 3 input neurons, i.e. no interactions

## predict response for input data from above
## create a data frame with inputs for prediction
pred_grid <- expand.grid(
  x = seq(0, 20, length.out=100),
  f = c("A", "B", "C"))

pred1 <-
  pred_grid |>
  mutate(y = predict(nn, newdata = pred_grid), method = "nnet basic")

pred2 <-
  pred_grid |>
  mutate(y = predict(learner, newdata = pred_grid), method = "nnet mlr")

ggplot(obs, aes(x, y)) + geom_point() +
  geom_line(data = rbind(pred1, pred2), mapping = aes(x, y, color=method)) +
  facet_wrap(~f)
})
