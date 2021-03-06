% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sims-to-df.R
\name{sims_to_df}
\alias{sims_to_df}
\title{Convert the sims output from the saom_simulate function to a dataframe object.}
\usage{
sims_to_df(sims)
}
\arguments{
\item{sims}{list output from the saom_simulate function or from ans$sims where ans was generated by RSiena::siena07()}
}
\description{
Convert the sims output from the saom_simulate function to a dataframe object.
}
\examples{
M1sims5df <- sims_to_df(M1sims5)

}
