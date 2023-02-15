library("randomForest")

rf <- randomForest(x, y, importance=TRUE, ntree=500)


rf                 # Fit, zeigt vor allem das r^2 (% Var explained)

par(mfrow = c(2, 1))
plot(rf)           # Lernkurve
hist(treesize(rf)) # Wie gross sind die erzeugten trees?

## Welche "variqablen" sind fuer den Effekt am wichtigsten ?
varImpPlot(rf)   # !!! Das ist eine ganz wichtige Grafik !!!

## das Selbe numerisch
#importance(rf)

yhat <- predict(rf)#[,1]

plot(dat$DATE, y)
lines(dat$DATE, yhat, col="red")

plot(dat$XBE, yhat)

### ============================================================================
library(caret)

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=5)
#control <- trainControl(method="oob", number=10) # rf only
#control <- trainControl(method="none", number=50)

# train the model
model <- train(x, y, method="rf", trControl = control)

model <- train(x, y, method="nnet", trControl = control)
#model <- train(x, y, method="glmboost")


# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)
yhat <- predict(model)

plot(dat$DATE, y)
lines(dat$DATE, yhat, col="red")

plot(dat$XBE, yhat)
abline(a=0, b=1, col="orange")
