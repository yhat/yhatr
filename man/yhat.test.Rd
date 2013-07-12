\name{yhat.test}
\alias{yhat.test}
\title{Mimics how a prediction will be made on Yhat's servers.}
\usage{
  yhat.test(data)
}
\arguments{
  \item{data}{input data for the model}
}
\description{
  This function performs the same operations that
  \link{yhat.predict} but does so locally.
}
\examples{
model.transform <- function(df) {
  df$Sepal.Length_sq <- df$Sepal.Length ^ 2
  df
}
model.predict <- function(df) {
  df
}

yhat.test(iris)
}
\keyword{local}
\keyword{predict}

