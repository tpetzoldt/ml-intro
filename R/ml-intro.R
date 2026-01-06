#' ---
#' title: Basic Machine Learning with R with Toy Examples from Aquatic Ecology
#' author: Thomas Petzoldt
#' format:
#'   html:
#'     toc: true
#'     number-sections: true
#'     page-layout: article
#'   pdf:
#'     toc: true
#'     number-sections: true
#'     shift-heading-level-by: -1
#' editor: source
#' bibliography: ann.bib
#' csl: apa
#' always_allow_html: true
#' embed-resources: true
#' ---
#' 


library("knitr")
library("DiagrammeR")
library("nnet")
library("dplyr")
library("ggplot2")
library("mlr3")
library("mlr3learners")
library("mlr3filters")
library("lubridate")


dauta <- read.csv("https://raw.githubusercontent.com/tpetzoldt/ml-intro/main/data/dauta2.csv")

#| label: fig-neuron
#| fig-cap: Neuron with 3 inputs (x1, x2, x3), weights (w1, w2, w3) and one output (y).
grViz("digraph neuron {
  rankdir=LR
  splines=line
 
  {
    node [style=solid, shape=circle, label='', width=.5]
    n
  }
  node [shape=none, margin=0.05, width=0.0];
    x1 x2 x3 y
  
  x1 -> n [label=<w<SUB>1</SUB>>]
  x2 -> n [label=<w<SUB>2</SUB>>]
  x3 -> n [label=<w<SUB>3</SUB>>]
  n  -> y [label=<w<SUB>4</SUB>>]
}")

#| label: fig-sigmoid
#| fig-cap: Flexibility of a sigmoidal activation function. A single neuron can  exhibit
#|   increasing and decreasing sigmoidal pattern, step functions, near-exponential  increase
#|   and decrease, saturation or linear shapes.


sigmoid <- function(x) {
  ifelse(x < -15, 0, ifelse(x > 15, 1, 1.0 / (1.0 + exp(-x))))
}

par(mfrow=c(3,3), las=1)
x <- seq(-5, 5, length.out=100)
plot(x, sigmoid(x), type="l")
plot(x, sigmoid(-x), type="l")
x <- seq(-5, 5, length.out=100)
plot(x, sigmoid(10*x), type="l")
plot(x, sigmoid(-10*x), type="l")
x <- seq(-5, 0, length.out=100)
plot(x, sigmoid(x), type="l")
x <- seq(0, 5, length.out=100)
plot(x, sigmoid(-2*x), type="l")
plot(x, sigmoid(2*x), type="l")
x <- seq(-2.5, 2.5, length.out=100)
plot(x, sigmoid(x/5), type="l")
plot(x, sigmoid(-x/5), type="l")

#| label: fig-forward-network
#| fig-cap: Fully connected feed-forward network with three layers, for example and 3
#|   neurons in the input  layer, 5 in the hidden layer and 2 in the output layer.  Deep
#|   neural networks have more than one hidden layer.

grViz("digraph ANN {
  
  rankdir=LR
  splines=line
  
  node [fixedsize=true, label='']
  
  subgraph cluster_0 {
    color=white
    node [style=solid, shape=circle]
    x1 x2 x3
    label = 'input layer'
  }
  
  subgraph cluster_1 {
    color=white
    node [style=solid,shape=circle]
    a12 a22 a32 a42 a52
    label = 'hidden layer'
  }
  
  subgraph cluster_2 {
    color=white
    node [style=solid, shape=circle]
    o1 o2
    label='output layer'
  }
  
  {x1 x2 x3} -> {a12 a22 a32 a42 a52} -> {o1 o2}
  
}")

#| label: fig-toydata
#| fig-cap: Toy data set.
# Generate some test data
set.seed(123)
x <- seq(0, 100, 1)
y <- 100 * dlnorm(x, 4, .7) + rnorm(x, sd = 0.05)

plot(x, y)

#| label: fig-fitted-ann
#| fig-cap: Toy data set scaled to [0,1] and two fitted neural networks with 1 hidden
#|   neuron (blue) and with 5 hidden neurons (red).
library(nnet)

set.seed(3142)

# Transform y-data (y must be between 0 and 1)
y <- (y - min(y))/(max(y) - min(y))

# Plot the transformed data
plot(x, y)

# Fit the neural net
nn1 <- nnet(x, y, size=1, trace = FALSE)
nn2 <- nnet(x, y, size=5, trace = FALSE, maxit=500)

lines(x, predict(nn1), col="blue")
lines(x, predict(nn2), col="red")

#| label: fig-dauta-dataset
#| fig-cap: Growthrate (1/d) dependent on temperature (°C) and light (μmol·m−2·s−1).

library("dplyr")
library("ggplot2")
dauta <- read.csv("data/dauta2.csv")
ggplot(data=dauta, aes(light, growthrate)) + geom_point() + 
  facet_grid(species ~ temperature)


dauta <- subset(dauta, species %in% c("Chlorella", "Fragilaria"))
ymin  <- min(dauta$growthrate)
ymax  <- max(dauta$growthrate)
y     <- (dauta$growthrate - ymin)/(ymax - ymin)


dauta$species_i <- as.numeric(factor(dauta$species))
x <- dauta[, c("species_i", "temperature", "light")]


set.seed(1423)
hidden <- 10    # number of hidden neurons
maxit  <- 1000  # maximum number of iterations per training replicate
rep    <- 5     # number of training replicates
value <- Inf
for (i in 1:rep) {
  net <- nnet(x, y, size=hidden, maxit=maxit, trace=FALSE)
  if (net$value < value) {
    n1 <- net
    value <- net$value
  }
}

#| label: fig-dauta-pred-obs
#| fig-cap: Comparison between predicted and observed values of the Dauta data set.
plot(y, n1$fitted.values, pch = "+", 
     col = dauta$species_i, xlab = "observed", ylab = "predicted")
cat("R^2=", 1 - var(residuals(n1))/var(y), "\n") # coefficient of determination

#| label: fig-dauta-ann-single
#| fig-cap: Comparison between observed (points) and predicted growth rates for species
#|   = 1 at 25°C.


## Test of neural net with a single data set
sdat <- subset(dauta, species_i == 1 & temperature == 25)

## set a series of values for the x axis
light  <- seq(0, 700, 5)
yy   <- predict(n1, data.frame(species_i = 1, temperature = 25, light = light))

## retransform growth rate from [0, 1] to original scale
growthrate   <- yy * (ymax - ymin) + ymin

plot(sdat$light, sdat$growthrate)
lines(light, growthrate, col = "red", lwd = 2)

#| label: fig-dauta-ann-multi
#| fig-cap: Comparison between observed (points) and predicted growth rates for two species.

newdata <- expand.grid(
  species_i = unique(dauta$species_i),
  temperature = unique(dauta$temperature),
  light = seq(0, 700, 5)
)

yy <- predict(n1, newdata)[, 1]

## retransform predicted values to original scale
newdata$growthrate <- yy * (ymax - ymin) + ymin

## assign species names corresponding to the species number
species <- levels(factor(dauta$species))
newdata$species <- species[newdata$species_i]

ggplot(data=dauta, mapping=aes(x=light, y=growthrate)) +
  geom_point() +
  geom_line(data=newdata, mapping=aes(x=light, y=growthrate), color="red", linewidth=1) +
  facet_grid(species ~ temperature)

#| label: tbl-variables
#| tbl-cap: Variables in the plankton data set.
vars <- read.csv("data/variables.csv")
knitr::kable(vars)


library(nnet)

## load datas set and convert DATE to date format
dat <- read.csv("data/phytoplankton-cqc.csv")
dat$DATE <- as.Date(dat$DATE)

## optional: transform dependent variable,
## because it contains very extreme values
dat$XBE <- dat$XBE^0.5

## rescale dependent variable to interval [0, 1]
dat$XBE <- 0.1 + dat$XBE/(1.2 * max(dat$XBE))


## split into target and explanation variables
select <- c("DEPTH", "ZMIX", "ST", "TE", "TH",
           "PO4_PE", "PO4_PH", "NO3E", "NO3H",
           "SIE", "SIH", "O2_SATE", "O2_SATH")

y <- dat$XBE
x <- dat[select]

#| label: fig-fitted-plankton
#| fig-cap: Measured phytoplankton data (circles, transformed scale) and fitted neural
#|   network (red line).

set.seed(123)

n_wts <- 15
nn <- nnet(x, y, size = n_wts, decay = 1e-3, maxit = 500, trace=FALSE)
err <- nn$value

yhat <- predict(nn)[,1]

plot(dat$DATE, dat$XBE, xlab="Date", ylab="Phytoplankton (transformed axis)")
lines(dat$DATE, yhat, col="red")

#| label: fig-measured-fitted-plankton
#| fig-cap: Neural network predictions versus measured phytoplankton data. The dotted
#|   line shows the 1:1 ratio between predicted and observed

plot(dat$XBE, yhat, xlab="observed", ylab="predicted")
abline(a=0, b=1, col="grey", lty="dotted")


for (i in 1:10) {
  ## fit another candidate network nn_try
  nn_try <- nnet(x, y, size=n_wts, decay=1e-3, abstol=1e-6, trace = FALSE, maxit=500)

  ## and store it if better
  if (nn_try$value < err) {
    err <- nn_try$value
    nn <- nn_try
    #cat(err, "\n")
  }
}

yhat <- predict(nn)[,1]

plot(dat$DATE, dat$XBE, xlim="Date", ylim="Phytoplankton (transformed axis)")
lines(dat$DATE, yhat, col="red")


library("mlr3")
library("mlr3learners")
library("lubridate")

dat <- read.csv("data/phytoplankton-cqc.csv")
dat$XBE <- sqrt(dat$XBE/max(dat$XBE))

date <- as.Date(dat$DATE)

all  <- 1:nrow(dat)
train <- which(year(date) %% 2 == 0) # uneven years
test  <- which(year(date) %% 2 == 1) # even years

task <- as_task_regr(dat, target="XBE")

## select all columns except DATE and XBE
task$select(c("DEPTH", "ZMIX", "ST", "TE", "TH", "PO4_PE", 
  "PO4_PH", "NO3E", "NO3H", "SIE", "SIH", "O2_SATE", "O2_SATH"))


mlr_learners


install.packages("ranger")


learner = lrn("regr.ranger")
#learner = lrn("regr.nnet", maxit=1500, decay=1e-3)


set.seed(123)
learner$train(task, row_ids = train)


print(learner$model)


mlr_measures


pred <- learner$predict(task, row_ids = test)
pred$score(list(msr("regr.mse"),  
                msr("regr.rmse"),
                msr("regr.rsq")))

#| label: fig-mlr-plankton
#| fig-cap: Measured phytoplankton data (dots, transformed scale) and fitted neural network
#|   for training (green) and test data (red).

pred_all <- learner$predict(task, row_ids = all)

plot(date, pred_all$truth, pch=16, cex=0.7, 
     col="navy", ylab="sqrt(XBM/max(XBM)")
lines(date, pred_all$response)
lines(date[test], pred_all$response[test], type="h", col="red")
lines(date[train], pred_all$response[train], type="h", col="green")


library("mlr3filters")
set.seed(123)
learner = lrn("regr.ranger", importance="impurity")
filter <- flt("importance", learner = learner)
filter$calculate(task)
as.data.table(filter)

