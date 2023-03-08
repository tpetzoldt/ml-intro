library(mlr3)
library(mlr3learners)

learner = lrns(c("classif.rpart", "classif.nnet"))
task = tsk("iris")
resampling = rsmp("holdout")

design = benchmark_grid(
  tasks = task,
  learners = learner,
  resamplings = resampling
)

bmr = benchmark(design, store_models = TRUE)
#> INFO  [07:13:52.802] [mlr3] Running benchmark with 2 resampling iterations
#> INFO  [07:13:52.889] [mlr3] Applying learner 'classif.rpart' on task 'iris' (iter 1/1)
#> INFO  [07:13:52.921] [mlr3] Applying learner 'classif.nnet' on task 'iris' (iter 1/1)
#> # weights:  27
#> initial  value 118.756408 
#> iter  10 value 58.639749
#> iter  20 value 45.676852
#> iter  30 value 21.336083
#> iter  40 value 8.646964
#> iter  50 value 6.041813
#> iter  60 value 5.906140
#> iter  70 value 5.902865
#> iter  80 value 5.898339
#> final  value 5.898161 
#> converged
#> INFO  [07:13:52.946] [mlr3] Finished benchmark

bmr$aggregate(msrs(c("classif.acc", "time_train")))
#>    nr      resample_result task_id    learner_id resampling_id iters
#> 1:  1 <ResampleResult[21]>    iris classif.rpart       holdout     1
#> 2:  2 <ResampleResult[21]>    iris  classif.nnet       holdout     1
#>    classif.acc time_train
#> 1:        0.98      0.007
#> 2:        1.00      0.005

# get the first resample result
rr1 = bmr$resample_result(1)

# Get the model from the first resampling iteration of this ResampleResult
rr1$learners[[1]]$model
#> n= 100 
#> 
#> node), split, n, loss, yval, (yprob)
#>       * denotes terminal node
#> 
#> 1) root 100 66 versicolor (0.32000000 0.34000000 0.34000000)  
#>   2) Petal.Length< 2.45 32  0 setosa (1.00000000 0.00000000 0.00000000) *
#>   3) Petal.Length>=2.45 68 34 versicolor (0.00000000 0.50000000 0.50000000)  
#>     6) Petal.Width< 1.75 37  4 versicolor (0.00000000 0.89189189 0.10810811) *
#>     7) Petal.Width>=1.75 31  1 virginica (0.00000000 0.03225806 0.96774194) *