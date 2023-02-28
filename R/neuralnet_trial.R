# File from Rodriogo, modified by tpetzoldt

library("dplyr")
library("ggplot2")
dauta <- read.csv("dauta4.csv")

# Transform the species variable into a numeric variable
levels <- unique(dauta$species)
dauta$species <- as.numeric(factor(dauta$species, levels = levels))

#Normalization
normalize <- function(x){return((x-min(x))/(max(x)-min(x)))}

dautanorm <- as.data.frame(lapply(dauta, normalize))

#Fit Regression model with a Neural Network
library(neuralnet)
set.seed(500)
nn_model <- neuralnet(growthrate ~ species + temperature + light,
                      data = dautanorm,
                      hidden = c(5, 3),
                      linear.output = TRUE,
                      rep = 5)

# Make predictions
nn_predictions <- predict(nn_model, dautanorm[, c("species", "temperature", "light")])

# Create a table to compare the predicted values with the real values
comparison_table <- data.frame(Real_GrowthRate = dautanorm$growthrate,
                               Predicted_GrowthRate = nn_predictions)

#Make predictions for a specific specie and temperature
sdat <- subset(dauta, species == 4 & temperature == 35)

light_norm <- seq(0, 1, 5/700)
yy <- predict(nn_model, data.frame(species = 1, temperature = 1, light = light_norm))

growthrate <- yy * (max(dauta$growthrate) - min(dauta$growthrate)) + min(dauta$growthrate)

light<- seq(0, 700, 5)
plot(sdat$light, sdat$growthrate)
lines(light, growthrate, col = "red", lwd = 2)

#Comparison between real and predicted growthrates for all normalized species and temperatures

newdata <- expand.grid(
  species = unique(dautanorm$species),
  temperature = unique(dautanorm$temperature),
  light = seq(0, 1, 5/700)
)

yt <- predict(nn_model, newdata)[, 1]

ggplot(data=dautanorm, mapping=aes(x=light, y=growthrate)) +
  geom_point() +
  geom_line(data=newdata, mapping=aes(x=light, y=yt), color="red", linewidth=1) +
  facet_grid(species ~ temperature)

#R2
SSresid <- sum((comparison_table$Real_GrowthRate - comparison_table$Predicted_GrowthRate)^2)
SStotal <- sum((comparison_table$Real_GrowthRate - mean(comparison_table$Real_GrowthRate))^2)
rsq <- 1 - (SSresid/SStotal)
cat("R-squared value:", round(rsq, 2))

plot(nn_model)
