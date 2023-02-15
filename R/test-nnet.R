library(nnet)

## data already scaled to [0, 1]
dat <- read.csv("phytoplankton-cqc.csv")

dat$DATE <- as.Date(dat$DATE)
select <- c("DEPTH", "ZMIX", "ST", "TE", "TH",
           "PO4_PE", "PO4_PH", "NO3E", "NO3H",
           "SIE", "SIH", "O2_SATE", "O2_SATH")

## rescale dependent variable to interval [0, 1]
dat$XBE <- dat$XBE/max(dat$XBE)

## optional: transform dependent variable, because it contains very extreme values
dat$XBE <- dat$XBE^0.5

## use remaining columns as dependent
y <- dat$XBE
x <- dat[select]


## some networks did not converge with the default settings,
## so we change some learning parameters (decay and maxit) by trial and error

set.seed(123)

n_wts <- 15
nn <- nnet(x, y, size = n_wts, decay = 1e-3, maxit = 500)
err <- nn$value


for (i in 1:10) {
  ## fit another candidate network nn_try
  nn_try <- nnet(x, y, size=n_wts, decay=1e-3, abstol=1e-6, trace = FALSE, maxit=500)

  ## and store it if better
  if (nn_try$value < err) {
    err <- nn_try$value
    nn <- nn_try
    cat(err, "\n")
  }
}

yhat <- predict(nn)[,1]

plot(dat$DATE, dat$XBE)
lines(dat$DATE, yhat, col="red")

plot(dat$XBE, yhat)
