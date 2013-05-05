model_transform <- function(df) {
  df.transformed <- transform(df, x2=x^2)
  df.transformed
}
model_predict <- function(df) {
  pred <- predict(df)
  data.frame('myPrediction' = pred)
}
model_require <- function() {
  require('library1')
  require('library2')
}
