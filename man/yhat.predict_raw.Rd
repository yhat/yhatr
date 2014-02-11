\name{yhat.predict_raw}
\alias{yhat.predict_raw}
\title{Calls Yhat's REST API and returns a JSON document containing both the prediction
and associated metadata.}
\usage{
  yhat.predict_raw(model_name, data)
}
\arguments{
  \item{model_name}{the name of the model you want to call}

  \item{data}{input data for the model}
}
\description{
  Calls Yhat's REST API and returns a JSON document
  containing both the prediction and associated metadata.
}
\examples{
yhat.config <- c(
 username = "your username",
 apikey = "your apikey"
)
\dontrun{
yhat.predict_raw("irisModel", iris)
}
}

