% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pb_for.R
\name{for<-}
\alias{for<-}
\title{Feed a progress bar to a for loop}
\usage{
for(it, seq, expr) <- value
}
\arguments{
\item{it}{variable used to iterate}

\item{seq}{values to iterate over}

\item{expr}{expression to loop over}

\item{value}{progress bar built with \code{progress::progress_bar$new(...)}}
}
\description{
By using \code{for<-} we allow the syntax
\code{pb -> for(it in seq) expr} where \code{pb} is created by a call to
\code{progress::progress_bar$new()}. The; only restriction compared to a regular
\code{for} call (or a \code{for} call modified by \code{pb_for()}is that \code{int} must exist
and must not be \code{NULL}.
}
