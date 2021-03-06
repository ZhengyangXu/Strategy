% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_methods-PERFORMANCE.R
\docType{methods}
\name{performanceIndicators}
\alias{performanceIndicators}
\alias{performanceIndicators,Strategy-method}
\title{Strategy Performance Indicators}
\usage{
performanceIndicators(object, of="portfolio"
       , from=NULL, until=NULL, which=NULL, alpha=0.05
       , scaling.periods=NULL, include.weights=TRUE
       , include.costs=TRUE, use.backtest=FALSE)

\S4method{performanceIndicators}{Strategy}(object, of = c("portfolio",
  "assets"), from = NULL, until = NULL, which = NULL, alpha = 0.05,
  scaling.periods = NULL, include.weights = TRUE, include.costs = TRUE,
  use.backtest = FALSE)
}
\arguments{
\item{object}{An object of class \code{Strategy}.}

\item{of}{Indicators to be calculated for assets separately or the portfolio.}

\item{from}{The date in character format \code{"yyyy-MM-dd"} or as date-object from which performance shall be considered. If \code{NULL}, no restriction is made.}

\item{until}{The date in character format \code{"yyyy-MM-dd"} or as date-object until which performance shall be considered. If \code{NULL}, no restriction is made.}

\item{which}{Names or number of assets that should be included in calculation.}

\item{alpha}{The significance level \eqn{\alpha} that is used for propability of cumulative loss at level \eqn{1-\alpha}.}

\item{scaling.periods}{Vector with annualization factors for calculation. Default is 252, 52, 12, 4, 1 for daily, weekly, monthly, quarterly and yearly data respectively.}

\item{include.weights}{Only relevant if \code{of="assets"}: If \code{FALSE}, weights are all set to \code{1}. This might be necessary if only single stock performance without weighting shall be considered.}

\item{include.costs}{If \code{FALSE}, the fixed and relative trading costs are NOT considered for performance calculation. Default value is \code{TRUE}. As default values for costs are \code{0}, this argument is obsolete if no costs are given.}

\item{use.backtest}{If set to \code{TRUE}, the signals from the backtesting output are considered for maximum drawdown calculation. If \code{FALSE}, the signals from the initial strategy execution are used.}
}
\description{
Get a list of the performance indicators of an object of class \code{Strategy}.
}
\examples{
## Not run:

# MA(200)-Strategy
params <- list(k=200)
myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)

# Get performance indicators of MA(200)-Strategy assets
performanceIndicators(myStrat.MA, from="2015-01-01", until="2015-12-31")

## End(Not run)
}
