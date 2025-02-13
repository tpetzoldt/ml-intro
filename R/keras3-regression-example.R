## Demonstration of a simple regression problem with Keras/Tensorflow
## ThPe, based on code snippets from different documentations

library(keras3)

## generate some  data
set.seed(123)
x <- runif(500, 0, 50)
y <- 0.5 + (sin(x/5) + rnorm(100, 0, 0.05))/3
plot(x, y, type="p")

## split data into training and test sets
train_size <- 0.8*length(x)
x_train <- x[1:train_size]
y_train <- y[1:train_size]
x_test <- x[(train_size + 1):100]
y_test <- y[(train_size + 1):100]

## normalize the input data
## use same scaling for training and test set taken from training set!
x_train_mean <- mean(x_train)
x_train_sd <- sd(x_train)
x_train_scaled <- (x_train - x_train_mean) / x_train_sd
x_test_scaled <- (x_test - x_train_mean) / x_train_sd


## build a Keras model
model <- keras_model_sequential(input_shape = c(1), input_dtype = "float64") |>
  layer_dense(units = 4, activation = "linear") |>  # Input layer
  #layer_dropout(0.2) |>
  layer_dense(units = 8, activation = "tanh") |>  # Hidden layer
  #layer_dropout(0.2) |>
  #layer_dense(units = 16, activation = "relu") |>  # Hidden layer
  layer_dense(units = 1)                           # Output layer

## compile the model
model |> compile(
  ## general-purpose optimizer
  #optimizer = "adam",
  ## another optimizer with tuned learning rate
  optimizer = keras$optimizers$AdamW(learning_rate = 0.005),
  loss = "mse",       # mean squared error for the regression
  metrics = c("mae")  # additional evaluation metric
)

## train the model
history <- model |> fit(
  x_train_scaled, y_train,
  epochs = 1000,          # adjust as needed
  batch_size = 32,        # adjust as needed
  validation_split = 0.2, # 20% of training data for validation
  ## early stopping if model converged
  callbacks = list(
    callback_early_stopping(
      monitor = "val_loss",
      patience = 20,
      restore_best_weights = TRUE)
    )
)

## Evaluate the model
loss <- model |> evaluate(x_test_scaled, y_test)
cat("Loss of test data set =", loss$loss, "\n")
cat("MAE of test data set  =", loss$mae, "\n")

predictions <- model |> predict(x_test_scaled)
residuals  <-  y_test - predictions

plot(residuals ~ predictions)
plot(predictions ~ y_test)
abline(a=0, b=1, col="red", lwd=2)
cat("R2=", 1 - var(residuals)/var(y_test), "\n")

## Compare data and predictions
head(cbind(x_test,y_test,predictions))
plot(x_test, y_test, main = "Regression Results", xlab = "X", ylab = "Y")
points(x_test, predictions, col = "red", pch=16)


# ## Save fitted model
# save_model_hdf5(model, "test-regression-model.h5")
#
# ## Load model
# loaded_model <- load_model_hdf5("test-regression-model.h5")
# predictions <- loaded_model |> predict(x_test_scaled)
