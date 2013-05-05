\name{yhat.show_models}
\alias{yhat.show_models}
\title{Shows which models you have deployed on Yhat.}
\usage{
  yhat.show_models()
}
\description{
  This function queries the Yhat API and finds the models
  that have been deployed for your account.
}
\examples{
yhat.show_models()
# some output here
   username className                  name version
1      greg                 MySMSClassifier       6
2      greg                 MySMSClassifier       7
3      greg                 MySMSClassifier       8
4      greg                 MySMSClassifier       9
...
...
...
}

