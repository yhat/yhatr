library(yhatr)
library(randomForest)
library(plyr)


iris$Sepal.Width_sq <- iris$Sepal.Width^2
fit <- randomForest(Species ~ ., data=iris)

model.require <- function() {
  require("randomForest")
}

model.transform <- function(df) {
  df$Sepal.Width_sq <- df$Sepal.Width^2
  df
}
model.predict <- function(df) {
  data.frame("prediction"=predict(fit, df, type="response"))
}
yhat.config <- c(username = "greg", apikey = "fCVZiLJhS95cnxOrsp5e2VSkk0GfypZqeRCntTD1nHA")
yhat.deploy("irisModel")

predict(fit, iris[1,])
yhat.predict("irisModel", 6, iris[1,])

results <- ldply(1:nrow(iris), function(i) {
  data.frame(idx=i,
             yhat=yhat.predict("irisModel", 6, iris[i,])$prediction,
             local=predict(fit, iris[i,]))
})

table(results$yhat==results$local)

