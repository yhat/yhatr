\name{yhat.spider.block}
\alias{yhat.spider.block}
\title{Private function for recursively looking for variables}
\usage{
  yhat.spider.block(block, defined.vars = c(as.symbol("var1"),as.symbol("var2")))
}
\arguments{
  \item{block}{code block to spider}

  \item{defined.vars}{variables which have already been defined within the scope of the block}
}
\description{
  Private function for recursively looking for variables
}

