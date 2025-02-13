## update packages and install keras3 and reticulate
update.packages(ask=FALSE)
new <- c("remotes", "keras3", "reticulate")
old <- installed.packages()[,1]

install.packages(intersect(old, new)) # install new packages if missing

## install a compatible Python version
## Important: check which version is needed and available!
reticulate::install_python(version = "3.9.13")

## install Keras Python libraries and dependencies (takes a while)
## creates also a Python virtual environment r-keras
keras3::install_keras()
