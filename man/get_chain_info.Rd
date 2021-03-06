% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-chain-info.R
\name{get_chain_info}
\alias{get_chain_info}
\title{Function to extract chains from a \code{\link[RSiena]{sienaFit}} object}
\usage{
get_chain_info(ans)
}
\arguments{
\item{ans}{\code{sienaFit} A \code{\link[RSiena]{sienaFit}} object resulting from a \code{\link[RSiena]{siena07}} call.}
}
\value{
Data frame with 8 columns containing information on the chain object. See Details.
}
\description{
Function to extract chains from a \code{\link[RSiena]{sienaFit}} object
}
\section{Details}{

The colums of the resulting data frame are:
\itemize{
   \item \code{ego} - The acting node of the microstep
   \item \code{alter} - The receiving node of the microstep
   \item \code{recipRate} - The reciprocal of the aggregate rate function in the current wave/period
   \item \code{logEgoProb} - Log probability of choosing the ego node
   \item \code{logAlterProb} - Log probability of choosing that alter node given the chosen ego node
   \item \code{diag} - A logical value indicating if the ego node equals the alter node, called a "diagonal" network in RSiena
   \item \code{rep} - The repetition number. Each microstep process from the initial network to the next wave is repeated 1000 times. \code{rep} runs from 1-1000.
   \item \code{period} - The period number indicates the wave that is being simulated toward. If \code{period} = m, then the microsteps simulate from observation \eqn{t_m} to observation \eqn{t_{m+1}}
}
}

