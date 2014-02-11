\name{yhatr}
\alias{yhatr}
\alias{yhatr}
\docType{package}
\title{
A package for deploying statistical models on Yhat
}
\description{
yhatr requires you to implement 2 functions \code{model.transform} and \code{model.predict} and optionally \code{model.require}. Each function should perform the actions neccessary to generate predictions for your model. You can put anything you want in each function, so long as \code{model.predict} returns a \code{\link{data.frame}}.
}
\details{
\tabular{ll}{
Package: \tab yhatr\cr
Type: \tab Package\cr
License: \tab FreeBSD\cr
}
See \url{http://www.yhathq.com/} for more detials.
}
\author{
Greg Lamp <greg@yhathq.com>
}
\references{
  \url{http://www.yhathq.com/}
}
\keyword{ package }
\seealso{
  \url{http://yhathq.com/docs/lang/r}
  \url{http://yhathq.com/docs/sample-apps}
  \url{http://yhathq.com/docs/quickstarts/r}
}
\examples{
# build a quick model
yhat.config <- c(username = "your username", apikey = "your apikey", env = "http://cloud.yhathq.com/")
iris$Sepal.Width_sq <- iris$Sepal.Width^2
fit <- glm(I(Species)=="virginica" ~ ., data=iris)


model.require <- function() {
  # your packages here
}

model.transform <- function(df) {
  df$Sepal.Width_sq <- df$Sepal.Width^2
  df
}

model.predict <- function(df) {
  data.frame("prediction"=predict(fit, df, type="response"))
} 

# yhat.deploy("irisModel")

}
