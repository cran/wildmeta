% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_cwb.R
\name{run_cwb}
\alias{run_cwb}
\title{Calculate bootstrap outcomes or test statistics using cluster wild
bootstrapping}
\usage{
run_cwb(
  model,
  cluster,
  R,
  f = NULL,
  ...,
  auxiliary_dist = "Rademacher",
  adjust = "CR0",
  simplify = FALSE,
  seed = NULL,
  future_args = NULL,
  future_f_args = NULL
)
}
\arguments{
\item{model}{Fitted \code{robumeta::robu()},
\code{metafor::rma.mv()}, or \code{metafor::rma.uni()}
model. For cluster wild bootstrapping, a null model is recommended, with
null model indicating a model containing all variables except the ones
being tested.}

\item{cluster}{Vector indicating which observations belong to the same
cluster.}

\item{R}{Number of bootstrap replications.}

\item{f}{Optional function to be used to calculate bootstrap test statistics
based on the bootstrapped outcomes. If f is \code{NULL} (the default), this
function returns a list containing bootstrapped outcomes.}

\item{...}{Optional arguments to be passed to the function specified in
\code{f}.}

\item{auxiliary_dist}{Character string indicating the auxiliary distribution
to be used for cluster wild bootstrapping, with available options:
"Rademacher", "Mammen", "Webb six", "uniform", "standard normal". The
default is set to "Rademacher." We recommend the Rademacher distribution
for models that have at least 10 clusters. For models with less than 10
clusters, we recommend the use of "Webb six" distribution.}

\item{adjust}{Character string specifying which small-sample adjustment
should be used to multiply the residuals by. The available options are
\code{"CRO"}, \code{"CR1"}, \code{"CR2"}, \code{"CR3"}, or \code{"CR4"},
with a default of \code{"CRO"}.}

\item{simplify}{Logical, with \code{TRUE} indicating the bootstrapped
outcomes or F statistics will be simplified to a vector or matrix and
\code{FALSE} (the default) indicating the results will be returned as a
list.}

\item{seed}{Optional seed value to ensure reproducibility.}

\item{future_args}{Optional list of additional arguments passed to the
\code{future_*()} functions used in calculating results across bootstrap
replications. Ignored if the future.apply package is not available.}

\item{future_f_args}{Optional list of additional arguments passed to the
\code{future_*()} function used in calculating \code{f} results (but not
bootstrap outcome vectors) across bootstrap replications. Ignored if the
future.apply package is not available.}
}
\value{
A list or matrix containing either the bootstrapped outcomes or
bootstrapped test statistics.
}
\description{
Calculate bootstrap outcomes or test statistics using cluster
wild bootstrapping for meta-analytic models fit using
\code{robumeta::robu()}, \code{metafor::rma.mv()}, and \code{metafor::rma.uni()}.
}
\examples{
library(clubSandwich)
library(robumeta)

model <- robu(d ~ 0 + study_type + hrs + test,
             studynum = study,
              var.eff.size = V,
              small = FALSE,
              data = SATcoaching)


bootstraps <- run_cwb(
  model = model,
  cluster =  model$data.full$study,
  R = 12,
  adjust = "CR2",
  simplify = FALSE
)

bootstraps
}
