library("mlr3")
library("mlr3learners")
library("mlr3filters")
library("mlr3tuning")
library("lubridate")

set.seed(324)
dat <- read.csv("phytoplankton-cqc.csv")
#dat$XBE <- sqrt(dat$XBE/max(dat$XBE))
dat$XBE <- sqrt(dat$XBE/max(dat$XBE))

date <- as.Date(dat$DATE)
dat$DATE <- NULL
# or task$select ...

task <- as_task_regr(dat, target="XBE")

mlr_learners

learner = lrn("regr.nnet", maxit=1500, decay=1e-3)
#learner = lrn("regr.rpart")
#learner = lrn("regr.lm")
#learner = lrn("regr.km")
#learner = lrn("regr.ranger", importance="impurity") # random forest

learner = lrn("regr.nnet", maxit=1500, decay=1e-3, size=to_tune(1, 55), trace = FALSE)

learner$param_set

all  <- 1:nrow(dat)
train <- which(year(date) %% 2 == 0) # uneven years
test  <- which(year(date) %% 2 == 1) # even years

#measure <- msr("regr.mse") # mean square error

learner$train(task, row_ids = train)

instance <- ti(
  task = task,
  learner = learner,
  resampling = rsmp("cv", folds = 3),
  measure = msr("regr.mse"),
  terminator = trm("none")
)

tuner <- tnr("grid_search", resolution = 10, batch_size = 5)


tuner$param_set
tuner$optimize(instance)
as.data.table(instance$archive)

learner1 <- lrn("regr.nnet", maxit=1500, decay=1e-3, trace = FALSE)
learner1$param_set$values <- instance$result_learner_param_vals

learner1$train(task, row_ids = train)
learner <- learner1

print(learner$model)
summary(learner$model)

pred_all <- learner$predict(task, row_ids = all)

plot(date, pred_all$truth, pch=16, cex=0.7, col="navy", ylab="sqrt(XBM/max(XBM)")
lines(date, pred_all$response)
lines(date[test], pred_all$response[test], type="h", col="red")
lines(date[train], pred_all$response[train], type="h", col="green")

#filter <- flt("importance", learner = learner)
#filter$calculate(task)
#as.data.table(filter)

filter <- flt("correlation")
filter$calculate(task)
as.data.table(filter)
