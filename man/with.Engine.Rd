% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Engine.R
\name{with.Engine}
\alias{with.Engine}
\title{Transaction Function}
\usage{
\method{with}{Engine}(engine, expr)
}
\description{
This function allows you to execute a block of code within a transaction. If the code fails, the transaction will roll back
}
