\name{yhatR-package}
\alias{yhatR-package}
\alias{yhatR}
\docType{package}
\title{
A package for deploying statistical models on Yhat
~~ package title ~~
}
\description{
yhatR requires you to implement 2 functions \code{\link{model.transform}} and \code{\link{model.predict}} and optionally \code{\link{model.require}}. Each function should perform the actions neccessary to generate predictions for your model. You can put anything you want in each function, so long as \code{\link{model.predict}} returns a  \code{\link{data.frame}}.
}
\details{
\tabular{ll}{
Package: \tab yhatR\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-05-03\cr
License: \tab FreeBSD\cr
}
This is how you use the package...
}
\author{
Greg Lamp <greg@yhathq.com>
}
\references{
  \link{http://www.yhathq.com/}
}
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
# build a quick model
iris$Sepal.Width_sq <- iris$Sepal.Width^2
fit <- glm(I(Species)=="virginica" ~ ., data=iris)


model.require <- function() {
  require("someLibrary")
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