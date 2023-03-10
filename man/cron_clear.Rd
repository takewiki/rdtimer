% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cron_clear.R
\name{cron_clear}
\alias{cron_clear}
\title{Clear all cron jobs}
\usage{
cron_clear(ask = TRUE, user = "")
}
\arguments{
\item{ask}{Boolean; ask before removal?}

\item{user}{The user whose crontab we are clearing.}
}
\description{
Clear all cron jobs
}
\examples{
\dontshow{if(interactive())
\{
}
f   <- system.file(package = "cronR", "extdata", "helloworld.R")
cmd <- cron_rscript(f)
cron_add(command = cmd, frequency = 'minutely', id = 'test1', description = 'My process 1')
cron_add(command = cmd, frequency = 'daily', at="7AM", id = 'test2', description = 'My process 2')
cron_njobs()

cron_ls()
cron_clear(ask=TRUE)
cron_ls()
\dontshow{
\}
}
}
