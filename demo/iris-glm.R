library(yhatr)
iris$Sepal.Width_sq <- iris$Sepal.Width^2
fit <- glm(I(Species)=="virginica" ~ ., data=iris)

model.require <- function() {
  # require("randomForest")
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

lapply(1:nrow(iris), function(i) {
  print(yhat.predict("irisModel", 5, iris[i,]))
})