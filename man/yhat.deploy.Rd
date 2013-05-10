\name{yhat.deploy}
\alias{yhat.deploy}
\title{Deploy a model to Yhat's servers}
\usage{
  yhat.deploy(model_name)
}
\arguments{
  \item{model_name}{name of your model}
}
\description{
  This function takes model.transform and model.predict and
  creates a model on Yhat's servers which can be called
  from any programming language via Yhat's REST API (see
  \code{\link{yhat.predict}}).
}
\examples{
yhat.config <- c(
 username = "your username",
 apikey = "your apikey"
)
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
yhat.deploy("irisModel")
}
\keyword{deploy}

