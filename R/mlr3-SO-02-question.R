library("reprex")
#reprex <- purrr::partial(reprex::reprex, venue = "r")

reprex({
library("mlr3")
library("mlr3learners")
set.seed(123)

## observation data, functional pattern + some random noise
x <- 1:20
obs <- data.frame(
  x = rep(x, 3),
  f = factor(rep(c("A", "B", "C"), each = 20)),
  y = c(3 * dnorm(x, 10, 3), 5 * dlnorm(x, 2, 0.5), dexp(20-x, .5))
      + rnorm(60, sd = 0.02)
)

## uncommenting this solves the error message
#obs$x <- as.double(obs$x)

## create task and train model
task    <- as_task_regr(obs, target="y")
learner <- lrn("regr.nnet", size = 3, maxit = 500, trace = FALSE)
learner$train(task)

})
