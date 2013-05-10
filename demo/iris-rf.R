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
yhat.config <- c(username = "your username", apikey = "your apikey")
(deployment <- yhat.deploy("irisModel"))

active_version <- deployment[1,]$version

predict(fit, iris[1,])
yhat.predict("irisModel", active_version, iris[1,])

results <- ldply(1:nrow(iris), function(i) {
  data.frame(idx=i,
             yhat=yhat.predict("irisModel", active_version, iris[i,])$prediction,
             local=predict(fit, iris[i,]))
})

table(results$yhat==results$local)

