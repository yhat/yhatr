% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yhatR.R
\name{yhat.batchDeploy}
\alias{yhat.batchDeploy}
\title{Deploy a batch model to Yhat servers}
\usage{
yhat.batchDeploy(job_name, confirm = TRUE)
}
\arguments{
\item{job_name}{name of batch job}

\item{confirm}{boolean indicating whether to prompt before deploying}
}
\description{
This function will deploy your batch model to the yhat servers
}
\examples{
yhat.config <- c(
 username = "your username",
 apikey = "your apikey",
 env = "http://sandbox.yhathq.com/"
)
yhat.batch <- function() {
  name <- "ross"
  greeting <- paste("Hello", name)
  print(greeting)
}
\dontrun{
yhat.batchDeploy("helloworld")
}
}
\keyword{deploy}
