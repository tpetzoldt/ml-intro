#https://stackoverflow.com/q/75642879/3677576

library(mlr3learners)
#> Loading required package: mlr3

learner = lrn("regr.nnet")

lgr::get_logger("mlr3")$set_threshold("warn")

set.seed(123)
x = 1:20



obs = data.frame(
  x = rep(x, 3),
  f = factor(rep(c("a", "b", "c"), each = 20)),
  y = c(3 * dnorm(x, 10, 3), 5 * dlnorm(x, 2, 0.5), dexp(20 - x, .5)) + rnorm(60, sd = 0.02)
)

nrow(obs)
#> [1] 60

x_test = seq(0, 20, length.out = 100)
test = expand.grid(
  x = x_test,
  f = c("a", "b", "c"),
  y = c(3 * dnorm(x_test, 10, 3), 5 * dlnorm(x_test, 2, 0.5), dexp(20 - x_test, .5)) + rnorm(60, sd = 0.02)
)

dat = rbind(obs, test)

task = as_task_regr(dat, target = "y")
resampling = rsmp("custom")
resampling$instantiate(task, list(train = 1:60), test = list(61:90060))

learners = replicate(100, learner$clone())

design = benchmark_grid(
  tasks = task,
  learners = learners,
  resampling
)


bmr = benchmark(design)
#> LOG OUTPUT ...
