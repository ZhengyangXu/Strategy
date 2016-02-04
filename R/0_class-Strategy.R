# ###################################
# PROJECT THESIS
# ZHAW Winterthur
# by Julian Busch
# December 18, 2015
# ###################################


#' @import xts
#' @import zoo

setOldClass("xts") # formally declare S3 class

# ------------------------------------------------------------
# Define Class << Strategy >>
# ------------------------------------------------------------
setClass(Class="Strategy",
         slots = list(
           # input
           prices = "xts",
           weights = "xts",
           indicators = "list",
           stratFUN.name = "character",
           stratFUN.parameters = "list",
           # output
           stratFUN = "function",
           plotFUN = "function",
           strat.vals = "list",
           signals = "xts",
           backtest.signals = "xts",
           backtest.parameters = "list",
           backtest.setup = "matrix",
           costs = "xts"
         )
         , prototype = prototype(
           # input
           weights = xts(),
           indicators = list(),
           # output
           backtest.signals = xts("Call backtest(StratObj, ...) to perform backtest.", order.by=Sys.Date()),
           backtest.parameters = list(),
           backtest.setup = matrix(),
           costs = xts("Costs are not yet deployed.", order.by=Sys.Date())
         )
)

# ------------------------------------------------------------
# CONSTRUCTOR
# ------------------------------------------------------------

#' @export
#' @aliases Strategy
#' @title Create Strategy Object
#' @description Creates an object of class \code{Strategy} with the given portfolio data and strategy-function.
#' @usage Strategy(assets, stratFUN.name
#'     assetValueType = "price", weights = NULL, indicators = NULL,
#'     stratFUN.parameters = NULL, costs = NULL,
#'     printSteps = F)
#' @param assets Time series of class \code{xts} of asset values in either price or log return form on
#' which the strategy function shall be applied. This is the portfolio of assets.
#' @param stratFUN.name The name of the strategy that should be applied. This can be either a
#'   predefined strategy like MA or EWMA or a self-written function in
#'   which case the full path to the function file to be called must be supplied.
#' @param assetValueType Assets can be passed as prices or log returns. In order to identify the
#'   asset value types, either one of the types has to be selected.
#' @param weights The portfolio weights for the given assets as time series (dynamic) or numerical (constant) weights.
#' @param indicators A list of indicators that might be used within customized strategies. It is recommended to pass a named list.
#' @param stratFUN.parameters The list of parameters and their values required by the strategy function selected with parameter stratFUN.name.
#' @param costs The trading costs as constant costs per trade. This feature is not deployed yet and will be part of the bachelor thesis.
#' @param printSteps This is a feature used mainly for debugging the constructor function in order to localize where unspecified errors occur. If set to true, the different steps run within the constructor is printed to the console.
#' @examples
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, stratFUN.name="MA", stratFUN.parameters=params)
Strategy <- function(assets,
                     stratFUN.name,
                     assetValueType = c("price", "logReturn"),
                     weights = NULL,
                     indicators = NULL,
                     stratFUN.parameters = NULL,
                     costs = NULL,
                     printSteps = F) {

  # VALIDATIONS
  # -------------------------------

  # CHECK assets
  if (!is.xts(assets)) stop("Please provide assets in xts format!")
  if (ncol(assets) == 0) stop("Assets does not have any data column!")
  if (printSteps==T) print("Assets checked.")

  # CHECK assetValueType (match.arg has its own algo to check)
  assetValueType <- match.arg(assetValueType)

  # CHECK Asset Weights
  if (is.null(weights)) weights <- rep(1/ncol(assets), ncol(assets))
  if (is.xts(weights)) {
      if (! all(dim(weights) == dim(assets)) ) stop("Asset Weights must have the same dimension as assets!")
  } else if (is.numeric(weights)) { # constant weights
      if (length(weights) != dim(assets)[2]) stop("Asset Weights must have the same number of columns as assets!")
      else {
        weights.tmp <- assets
        weights.tmp[,] <- matrix(rep(weights, nrow(assets)), byrow=T, nrow=nrow(assets))
        rownames(weights.tmp) <- index(assets)
        weights <- as.xts(weights.tmp)
      }
  } else {
      stop("Please provide asset weights as numeric or xts!")
  }
  # check weights sums --> scale
  if (length(lineNo <- which(rowSums(weights) != 1)) > 0) {
    warning("Asset weights sum is not = 1 in rows: ", paste(lineNo, collapse = ", "),
            "\nAsset weights scaled accordingly.")
    weights[lineNo,] <- weights[lineNo,]/rowSums(weights)[lineNo]
  }
  if (printSteps==T) print("Weights checked.")

  # CHECK indicators
  if (!is.null(indicators)) {
    if (!is.list(indicators)) {
      stop("Please provide all indicators as xts with same length as assets and within a list!")
    } else {
      for (i in 1:length(indicators)) { #i<-1
        if (!is.xts(indicators[[i]])) stop("Please provide all indicators as xts format!")
      }
    }
  } else { indicators <- list() }
  if (printSteps==T) print("Indicators checked.")

  # MATCH strategy function and plot function
  if (is.null(stratFUN.name)) stop("Please provide strategy function (in accordance with file name excl. extension) or custom file path!")
  if (length(grep("\\||/", stratFUN.name)) > 0) { # when local strategy path passed
    stratFUN.src <- stratFUN.name
    if (!file.exists(stratFUN.src)) stop("Could not find function file in ", stratFUN.src)
    stratFUN.name <- basename(stratFUN.src)
    source(stratFUN.src)
  }

  # SET strategy and plot function
  tryCatch({
    stratFUN <- get(paste0("strategy.", tolower(stratFUN.name)), envir = environment(Strategy))
  }, error = function(e) stop(paste0("Strategy ", stratFUN.name, " could not be found. Consult Strategy()-documentation for available strategies.")) )
  tryCatch({
    plotFUN <- get(paste0("plot.", tolower(stratFUN.name)), envir = environment(Strategy))
  }, error = function(e) stop("Please define plot.STRATNAME-function first.") )

  if (printSteps==T) print("Strategy function(s) checked.")


  #
  # COSTS: will be developed at a later stage!
  #


  # PREPARE ASSET TYPES
  # -------------------------------

  # CALCULATE Prices / log returns
  if (assetValueType == "price") {
    if (length(which(assets<=0)) > 0) stop("Asset values with value type 'prices' must not be 0 or negative (because of log return calculations)!")
    prices <- assets
  } else if (assetValueType == "logReturn") {
    prices <- .LogReturnsToPrices(assets)
  }
  if (printSteps==T) print("Prices calculated.")



  # STRATEGY OUTPUT
  # -------------------------------

  # CALL strategy function and set values
  strat.Out <- stratFUN(prices = prices, weights = weights, indicators = indicators, parameters = stratFUN.parameters)
  stratFUN.parameters <- strat.Out[["parameters"]]
  strat.vals <- strat.Out[["strat.vals"]]
  signals <- strat.Out[["signals"]]
  prices <- strat.Out[["prices"]]
  logReturns <- strat.Out[["logReturns"]]
  weights <- strat.Out[["weights"]]
  indicators <- strat.Out[["indicators"]]

  if (printSteps==T) print("Strategy function executed.")


  # CREATE S4 << Strategy >> OBJECT
  # -------------------------------

  new(Class = "Strategy"
      , prices = prices
      , weights = weights
      , indicators = indicators
      , stratFUN = stratFUN
      , plotFUN = plotFUN
      , stratFUN.name = stratFUN.name
      , stratFUN.parameters = stratFUN.parameters
      , strat.vals = strat.vals
      , signals = signals
      # , backtest.performance = NOT SET HERE, will be set in backtesting method
      # , backtest.parameters = NOT SET HERE, will be set in backtesting method
      # , backtest.setup = NOT SET HERE, will be set in backtesting method
      # , costs = costs # not yet deployed --> in BA
      )
}


# ------------------------------------------------------------
# Define Basic Methods
# ------------------------------------------------------------

setMethod(f = "print",
          signature = "Strategy",
          definition = function(x) {
            signals <- getSignals(x, use.backtest=F)
            cat("\n***********************************")
            cat("\n", getStratFUN.name(x, include.params=T), sep="")
            cat("\n")
            cat("\n***********************************")
            cat("\nSignals")
            cat("\nFrom:        ", format(start(signals), format="%Y-%m-%d"), sep="")
            cat("\nUntil:       ", format(end(signals), format="%Y-%m-%d"), sep="")
            cat("\nPeriodicity: ", periodicity(signals)[["scale"]], sep="")
            cat("\n")
            cat("\nData:        ", paste0(colnames(signals[,1])), sep="")
            if (ncol(signals) > 1) {
              for (i in 2:ncol(signals))
                cat("\n            ", paste0(colnames(signals[,i])))
            }
            backtest <- head(x@backtest.signals,n=1)
            backtestExecuted <- "Executed"
            if (!is.numeric(backtest) || is.null(backtest)) {
              backtestExecuted <- "Not yet executed"
            }
            cat("\n")
            cat("\nBacktest:    ", backtestExecuted, sep="")
            cat("\n***********************************")
          }
)

setMethod(f = "show",
          signature = "Strategy",
          definition = function(object){
            print(object)
          }
)


# ------------------------------------------------------------
# PLOTS
# ------------------------------------------------------------

#' @export
#' @name plot
#' @aliases plot
#' @title Plot of a \code{Strategy}-object
#' @description Calls the \code{plot} function that is defined within the strategy definition. E.g. MA-strategy's \code{plot} function is named \code{plot.MA()}.
#' @param object An object of class \code{Strategy}.
#' @examples
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, stratFUN.name="MA", stratFUN.parameters=params)
#'
#' # Plot MA(200)-Strategy
#' plot(myStrat.MA, from="2015-01-01", until="2015-12-31", which=1)
setMethod(f = "plot",
          signature = "Strategy",
          definition = function(x, from=NULL, until=NULL, which=NULL, main=NULL) {
            plotFUN <- x@plotFUN
            par(mgp=c(2.5, 1, 0))
            plotFUN(x, from=from, until=until, which=which, main=main)
          }
)

# Plot of only the performance
# of the strategy or backtest
setGeneric(name = "plotPerformance",
           def = function(object, which=NULL, of="portfolio", from=NULL, until=NULL, use.backtest=F, plot.params=T, plot.params.names=NULL, plot.params.first=T, ...) {
             standardGeneric("plotPerformance")
           }
)

#' @export
#' @name plotPerformance
#' @aliases plotPerformance
#' @title Plot Strategy Performance
#' @description Plots performance of an object of class \code{Strategy}.
#' @usage plotPerformance(object, which=NULL, of="portfolio", from=NULL, until=NULL
#'      , use.backtest=F, plot.params=T , plot.params.names=NULL, plot.params.first=T
#'      , ...)
#' @param object An object of class \code{Strategy}.
#' @param which Names or number of assets that should be included in performance. If a portfolio performance from only a subset of the assets is calculated, the weights are scaled accordingly.
#' @param of Performance to be extracted from assets separately or the portfolio performance.
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which performance shall be plotted. If \code{NULL}, the start date of the performances is used.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which performance shall be plotted. If \code{NULL}, the end date of the performances is used.
#' @param use.backtest If \code{TRUE}, the signals from the backtesting output are considered for performance calculation. If \code{FALSE}, the signals from the normal strategy execution with the intial parameters are used.
#' @param plot.params If set to TRUE, the parameters used for the performance periods are plotted into the chart. Requires that use.backtest is set to \code{TRUE}.
#' @param plot.params.names New parameter names to be shown can be supplied. Requires that use.backtest is set to \code{TRUE} to take effect.
#' @param plot.params.first If \code{TRUE}, the parameter for the first period is plotted. Otherwise, the parameters are plot at the point on the x-axis, from which they are valid. Requires that use.backtest is set to \code{TRUE} to take effect.
#' @param ... Further arguments that can be passed to the underlying plot()-function.
#' @examples
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, stratFUN.name="MA", stratFUN.parameters=params)
#'
#' # Plot MA(200)-Strategy
#' plotPerformance(myStrat.MA, from="2015-01-01", until="2015-12-31")
#'
#' # Plot backtested MA(200)-Strategy
#' plotPerformance(myStrat.MA, from="2015-01-01", until="2015-12-31", use.backtest=T)
setMethod(f = "plotPerformance",
          signature = "Strategy",
          definition = function(object, which, of=c("portfolio", "assets"), from, until, use.backtest, plot.params, plot.params.names, plot.params.first, ...) {
            # match argument "of" --> standard set in setGeneric function
            of <- match.arg(of)
            if (!is.logical(plot.params) || !is.logical(plot.params.first))
              stop("Please provide plot.params and plot.params.first as logical!")

            performance <- performance(object, of=of, which=which, from=from, until=until, use.backtest=use.backtest)

            # PLOT Parameters / check for heading
            args <- list(...)
            plot.main <- colnames(performance)
            if ("main" %in% names(args))
              plot.main <- args[["main"]]
            if (length(plot.main) != ncol(performance))
              stop("Please provide as many headings as graphics!")
            args.inUse <- which(names(args) %in% "main")
            if (length(args.inUse) > 0)
              args <- args[-args.inUse] # select only editable arguments for plot

            for (i in 1:ncol(performance)) {
              par(mar=c(7, 4.1, 4.1, 2.1), mgp=c(2.5, 1, 0))
              do.call(function(...) { # using do.call for passing  plot arguments from ellipsis
                plot(performance[,i], main=plot.main[i], minor.ticks=F, las=2, axes=F, ...)
              }, args)
              axis(1, at=.index(performance[,i])[axTicksByTime(performance)], labels=names(axTicksByTime(performance)), las=2)
              axis(2, at=pretty(range(performance[,i])), las=2)

              if (use.backtest == T && of != "portfolio" && plot.params == T) {
                paramData_all <- getParameters(object, use.backtest=T)[[colnames(performance[,i])]] # get all
                paramData_all <- as.xts(apply(paramData_all, 2, function(x) if(is.numeric(x)) round(x, 3)))
                if (max(paramData_all) > 1000)
                  warning("Parameter values > 1000 but only first 3 numbers are shown!")

                # check for costum param names
                name.len <- 3 # standard 3 chars to display
                if (!is.null(plot.params.names)) {
                  if (length(plot.params.names) != ncol(paramData_all)) {
                    stop("Please provide as many plot.params.names as parameters are backtested! You can check by calling getParameters(stratObj, use.backtest=T).")
                  } else {
                    colnames(paramData_all) <- plot.params.names
                    name.len <- max(nchar(plot.params.names))
                  }
                }
                paramData.lbl <- matrix(apply(paramData_all, 1, function(params) paste(paste(substring(names(params),1,name.len), substring(params,1,3), sep="="), collapse="\n")))
                paramData <- xts(paramData.lbl, order.by=index(paramData_all))
                if (plot.params.first == T) {
                  paramData.idx <- as.Date(index(paramData))
                  paramData.before <- which(as.Date(paramData.idx) < start(performance[,i]))
                  if (length(paramData.before) > 0) {
                    paramData.idx[last(paramData.before)] <- start(performance[,i])
                  }
                  index(paramData) <- paramData.idx
                }
                paramData <- paramData[paste0(start(performance[,i]),"::",end(performance[,i])),]
                abline(v=.index(paramData), col="blue")
                text(x=.index(paramData), y=1, labels=paramData, adj=0, col="blue", cex=0.7)
              } #if plot parameters

            } #for performance

          } #method function
)


# ------------------------------------------------------------
# GETTERS of SLOT DATA
# ------------------------------------------------------------

setGeneric(name = "getPrices",
           def = function(object, from=NULL, until=NULL, which=NULL) {
             standardGeneric("getPrices")
           }
)

#' @export
#' @name getPrices
#' @aliases getPrices
#' @title Get price data from \code{Strategy}-object
#' @description Gets the price data of an object of class \code{Strategy} that was used within strategy calculation.
#' @usage getPrices(object, from=NULL, until=NULL, which=NULL)
#' @param object An object of class \code{Strategy}.
#' @param which Names or column-number of assets that should be included. If \code{NULL}, all prices are returned.
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which prices shall be returned. If \code{NULL}, no restriction is made.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which prices shall be returned. If \code{NULL}, no restriction is made.
#' @examples
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, stratFUN.name="MA", stratFUN.parameters=params)
#'
#' # Get price data from MA(200)-Strategy
#' getPrices(myStrat.MA, from="2015-01-01", until="2015-12-31")
setMethod(f = "getPrices",
          signature = "Strategy",
          definition = function(object, from, until, which) {

            # get all prices
            prices <- object@prices

            # set dates
            if (is.null(from)) {
              from <- start(prices)
            } else {
              from <- as.Date(from)
            }
            if (is.null(until)) {
              until <- end(prices)
            } else {
              until <- as.Date(until)
            }

            # validate which values
            which.out <- validWhich(which, prices)

            # restrict prices to range
            prices <- prices[paste0(from,"::",until), which.out]

            return(prices)
          }
)

setGeneric(name = "getWeights",
           def = function(object, from=NULL, until=NULL, which=NULL) {
             standardGeneric("getWeights")
           }
)

#' @export
#' @name getWeights
#' @aliases getWeights
#' @title Get weights from \code{Strategy}-object
#' @description Gets the weights data of an object of class \code{Strategy} that was used within strategy calculation.
#' @usage getWeights(object, from=NULL, until=NULL, which=NULL)
#' @param object An object of class \code{Strategy}.
#' @param which Names or column-number of assets that should be included. If \code{NULL}, all weights are returned.
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which weights shall be returned If \code{NULL}, no restriction is made.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which weights shall be returned. If \code{NULL}, no restriction is made.
#' @examples
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, stratFUN.name="MA", stratFUN.parameters=params)
#'
#' # Get weights data from MA(200)-Strategy
#' getWeights(myStrat.MA, from="2015-01-01", until="2015-12-31")
setMethod(f = "getWeights",
          signature = "Strategy",
          definition = function(object, from, until, which) {

            # get all weights
            weights <- object@weights

            # set dates
            if (is.null(from)) {
              from <- start(weights)
            } else {
              from <- as.Date(from)
            }
            if (is.null(until)) {
              until <- end(weights)
            } else {
              until <- as.Date(until)
            }

            # validate which values
            which.out <- validWhich(which, weights)

            # restrict weights to range
            weights <- weights[paste0(from,"::",until), which.out]

            return(weights)
          }
)

setGeneric(name = "getIndicators",
           def = function(object, from=NULL, until=NULL, which=NULL) {
             standardGeneric("getIndicators")
           }
)

#' @export
#' @name getIndicators
#' @aliases getIndicators
#' @title Get indicators from \code{Strategy}-object
#' @description Gets the indicators data of an object of class \code{Strategy} that was used within strategy calculation.
#' @usage getIndicators(object, from=NULL, until=NULL, which=NULL)
#' @param object An object of class \code{Strategy}.
#' @param which Names or list-number of indicators that should be included. If \code{NULL}, all indicators are returned.
#' @examples
#' # MA(200)-Strategy
#' params <- list(k=200)
#' indicators <- list(volume=volume) # example: volume of trades
#' myStrat.MA <- Strategy(assets=assets, stratFUN.name="MA", stratFUN.parameters=params, indicators=indicators)
#'
#' # Get indicator data from MA(200)-Strategy
#' getIndicators(myStrat.MA, from="2015-01-01", until="2015-12-31")
setMethod(f = "getIndicators",
          signature = "Strategy",
          definition = function(object, from, until, which) {

            # get all indicators
            indicators <- object@indicators


            # validate which values
            if (!is.null(which)) {
              which.out <- pmatch(which, names(indicators))
            } else {
              which.out <- names(indicators)
            }


            # restrict indicators to range
            indicators <- indicators[[which.out]]

            return(indicators)
          }
)

setGeneric(name = "getStratFUN",
           def = function(object) {
             standardGeneric("getStratFUN")
           }
)

#' @export
#' @name getStratFUN
#' @aliases getStratFUN
#' @title Get strategy function from \code{Strategy}-object
#' @description Gets the strategy function of an object of class \code{Strategy} that was used for strategy calculation.
#' @usage getStratFUN(object)
#' @param object An object of class \code{Strategy}.
#' @examples
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, stratFUN.name="MA", stratFUN.parameters=params)
#'
#' # Get strategy function from MA(200)-Strategy
#' MA.FUN <- getStratFUN(myStrat.MA)
setMethod(f = "getStratFUN",
          signature = "Strategy",
          definition = function(object) {
            return(object@stratFUN)
          }
)

setGeneric(name = "getStratFUN.name",
           def = function(object, include.params=F) {
             standardGeneric("getStratFUN.name")
           }
)

#' @export
#' @name getStratFUN.name
#' @aliases getStratFUN.name
#' @title Get strategy function name from \code{Strategy}-object
#' @description Gets the strategy function name of an object of class \code{Strategy} that was used for strategy calculation. This function is for aesthetical purposes only and does not have any numerical relevance.
#' @usage getStratFUN.name(object)
#' @param object An object of class \code{Strategy}.
#' @param include.params If set to \code{TRUE}, the parameters used for strategy evaluation are included.
#' @examples
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, stratFUN.name="MA", stratFUN.parameters=params)
#'
#' # Get strategy function name from MA(200)-Strategy
#' getStratFUN.name(myStrat.MA) # returns "MA"
#' getStratFUN.name(myStrat.MA, include.params=T) # returns "MA(200)"
setMethod(f = "getStratFUN.name",
          signature = "Strategy",
          definition = function(object, include.params) {
            if (!is.logical(include.params))
              stop("Include.params argument must be logical!")
            stratFUN.name <- object@stratFUN.name
            params <- NULL
            if (include.params == TRUE) {
              params.list <- getParameters(object)
              # dont print period or printSteps
              params.list <- params.list[!names(params.list) %in% c("period")]
              if (length(params.list) != 0) {
                params <- paste0( "(", paste(names(params.list), Reduce(cbind, params.list), sep="=", collapse=","), ")" )
              }
            }
            name <- paste0(stratFUN.name, params)
            return(name)
          }
)

# Returns parameters used for strategy
# or (optimized) parameters used in backtest windows
setGeneric(name = "getParameters",
           def = function(object, use.backtest=F) {
             standardGeneric("getParameters")
           }
)


#' @export
#' @name getParameters
#' @aliases getParameters
#' @title Get strategy function from \code{Strategy}-object
#' @description Gets the strategy function of an object of class \code{Strategy} that was used for strategy calculation.
#' @usage getStratFUN(object, use.backtest=F)
#' @param object An object of class \code{Strategy}.
#' @param use.backtest If set to \code{TRUE}, the calibrated parameters of the backtest are returned. Requires \code{\link{backtest}} to be  executed first.
#' @examples
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, stratFUN.name="MA", stratFUN.parameters=params)
#'
#' # Get parameters from MA(200)-Strategy
#' getParameters(myStrat.MA)
setMethod(f = "getParameters",
          signature = "Strategy",
          definition = function(object, use.backtest) {
            if (!is.logical(use.backtest))
              stop("Please provide boolean expression for use.backtest.")
            if (use.backtest==T) {
              stratFUN.params <- object@backtest.parameters
              if (length(stratFUN.params) ==0 )
                stop("Please execute backtest-method first!")
            } else {
              stratFUN.params <- object@stratFUN.parameters
            }

            return(stratFUN.params)
          }
)

setGeneric(name = "getSignals",
           def = function(object, use.backtest=F, which=NULL) {
             standardGeneric("getSignals")
           }
)

#' @export
#' @name getSignals
#' @aliases getSignals
#' @title Get trading signals from \code{Strategy}-object
#' @description Gets the trading signals of an object of class \code{Strategy} that were output from strategy calculation.
#' @usage getSignals(object, use.backtest=F, which=NULL)
#' @param object An object of class \code{Strategy}.
#' @param use.backtest If set to \code{TRUE}, the signals of the backtest are returned. Requires \code{\link{backtest}} to be  executed first.
#' @param which Names or column-number of assets that should be returned. If \code{NULL}, all prices are returned.
#' @examples
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, stratFUN.name="MA", stratFUN.parameters=params)
#'
#' # Get signals from MA(200)-Strategy
#' getSignals(myStrat.MA) # all signals returned
#' getSignals(myStrat.MA which=c(1,2), use.backtest=T) # backtest signals for first two assets returned
setMethod(f = "getSignals",
          signature = "Strategy",
          definition = function(object, use.backtest, which) {
            if (!is.logical(use.backtest))
              stop("The backtest-argument is a boolean expression!")
            if (use.backtest==T) {
              signals <- object@backtest.signals
              if (!is.numeric(signals) || is.null(signals))
                stop("Please execute backtest-method first!")
            } else {
              signals <- object@signals
            }
            which <- validWhich(which, signals)
            return(signals[,which])
          }
)

# Returns a list with strategy values
# as xts objects
# e.g. the MA-values and KAMA-values of the crossing KAMA strategy
setGeneric(name = "getStratVals",
           def = function(object) {
             standardGeneric("getStratVals")
           }
)


#' @export
#' @name getStratVals
#' @aliases getStratVals
#' @title Get strategy values from \code{Strategy}-object
#' @description Gets the strategy values of an object of class \code{Strategy} that were output from strategy calculation.
#' @usage getStratVals(object)
#' @param object An object of class \code{Strategy}.
#' @examples
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, stratFUN.name="MA", stratFUN.parameters=params)
#'
#' # Get strategy values from MA(200)-Strategy
#' getStratVals(myStrat.MA) # all strategy values returned
setMethod(f = "getStratVals",
          signature = "Strategy",
          definition = function(object) {
            return(object@strat.vals)
          }
)

# Returns the matrix
# of the setup of the backtest parameters
setGeneric(name = "getBacktestSetup",
           def = function(object) {
             standardGeneric("getBacktestSetup")
           }
)

#' @export
#' @name getBacktestSetup
#' @aliases getBacktestSetup
#' @title Get backtest parameter values from \code{Strategy}-object
#' @description Gets the backtest parameter values of an object of class \code{Strategy} that were used for backtesting the strategy. This includes the information about the parameters,
#' @usage getBacktestSetup(object)
#' @param object An object of class \code{Strategy}.
#' @examples
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, stratFUN.name="MA", stratFUN.parameters=params)
#'
#' # Get backtest setup from MA(200)-Strategy
#' getBacktestSetup(myStrat.MA)
setMethod(f = "getBacktestSetup",
          signature = "Strategy",
          definition = function(object) {
            return(object@backtest.setup)
          }
)

# ------------------------------------------------------------
# GETTERS of PERFORMANCE
# ------------------------------------------------------------


# Returns the performance of the strategy
# of either the weighted portfolio or the single assets (chooseable by the "which" argument)
# within potential date range
# as xts object
setGeneric(name = "performance",
           def = function(object, of="portfolio", from=NULL, until=NULL, which=NULL, type="performance", use.backtest=F) {
             standardGeneric("performance")
           }
)


#' @export
#' @name performance
#' @aliases performance
#' @title Get Strategy Performance
#' @description Gets performance of an object of class \code{Strategy}.
#' @usage performance(object, which=NULL, of="portfolio", from=NULL, until=NULL, type="performance"
#'      , use.backtest=F
#'      , ...)
#' @param object An object of class \code{Strategy}.
#' @param which Names or number of assets that should be included in performance. If a portfolio performance from only a subset of the assets is calculated, the weights are scaled accordingly.
#' @param of Performance to be extracted from assets separately or the portfolio performance.
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which performance shall be returned If \code{NULL}, no restriction is made.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which performance shall be returned. If \code{NULL}, no restriction is made.
#' @param use.backtest If \code{TRUE}, the signals from the backtesting output are considered for performance calculation. If \code{FALSE}, the signals from the initial strategy execution are used.
#' @param type Which type of performance shall be returned. \code{performance} is the cumulative performance starting at \code{1}, \code{logReturns} to get logarithmic returns or \code{returns} for arithmetic returns.
#' @examples
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, stratFUN.name="MA", stratFUN.parameters=params)
#'
#' # Get performance of MA(200)-Strategy
#' performance(myStrat.MA, from="2015-01-01", until="2015-12-31")
#'
#' ## Not run:
#' # Get backtest performance of MA(200)-Strategy
#' performance(myStrat.MA, from="2015-01-01", until="2015-12-31", use.backtest=T, type="logReturns")
#' ## End(Not run)
setMethod(f = "performance",
          signature = "Strategy",
          definition = function(object, of=c("portfolio", "assets"), from, until, which, type=c("performance", "logReturns", "returns"), use.backtest) {
            of <- match.arg(of)
            type <- match.arg(type)
            if (!is.logical(use.backtest))
              stop("The backtest-argument must be a boolean!")
            signals <- getSignals(object, use.backtest, which=which)
            prices <- getPrices(object, which=colnames(signals))

            # SET start date (performance=1)
            if (start(prices) < start(signals)) {
              from.start <- last(index(prices[index(prices) < start(signals)]))
            } else
              from.start <- start(prices)

            # PERIODIC prices
            prices <- prices[index(signals)]

            # VALIDATE Date Range Input!
            # from
            if (!is.null(from)) {
              # if from is not a valid date, this will throw an error
              from <- as.Date(from)
              # do not start before price data
              if (from < start(prices))
                from <- start(prices)
            } else {
              from <- from.start
            }
            # until
            if (!is.null(until)) {
              # if until is not a valid date, this will throw an error
              until <- as.Date(until)
            } else {
              until <- end(signals)
            }
            # range validation
            if (from > until)
              stop("From date cannot be greater than until date!")

            logReturns <- .PricesToLogReturns(prices[paste0(from,"::",until),])

            # add signals first row = 0 to let performance start at 1
            # in case logReturns start equal to signals, this is redundant
            if (start(logReturns) < start(signals)) {
              signals_cash <- xts(t(rep(0, ncol(signals))), order.by=from)
              signals_cash2 <- NULL
              if (from < from.start && from.start < start(signals))
                signals_cash2 <- xts(t(rep(0, ncol(signals))), order.by=from.start)
              signals <- rbind(signals_cash, signals_cash2, signals)
              colnames(signals) <- colnames(prices)
            }


            # Calculate strategy log returns
            strat.logReturns <- logReturns * signals


            # calculate performances for assets
            performance <- exp(cumsum(strat.logReturns))

            # weight performances to portfolio level
            if (of == "portfolio") {
              # multiply with weights
              weights <- getWeights(object)[,colnames(performance)]
              # scale weights (in case not all assets selected)
              weights <- weights / rowSums(weights)
              performance <- xts(rowSums(performance * weights), order.by=index(performance))
              if (use.backtest == T) {
                colnames(performance) <- "Backtest Portfolio"
              } else {
                colnames(performance) <- "Portfolio"
              }
            }

            if (type == "logReturns") {
              performance <- .PricesToLogReturns(performance)
            } else if (type == "returns") {
              performance <- exp(.PricesToLogReturns(performance)) - 1
            }
            return(performance)
          }
)

# Sharpe Ratio calculation
setGeneric(name = "sharpe",
           def = function(object, rf=0, of="portfolio", from=NULL, until=NULL, which=NULL, scaling.periods=1, use.backtest=F) {
             standardGeneric("sharpe")
           }
)

#' @export
#' @name sharpe
#' @aliases sharpe
#' @title Get Sharpe Ratio of Performance
#' @description Get the sharpe ratio of the performance of an object of class \code{Strategy}.
#' @usage sharpe(object, rf=0, of="portfolio", which=NULL, from=NULL, until=NULL, scaling.periods=1
#'      , use.backtest=F
#'      , ...)
#' @param object An object of class \code{Strategy}.
#' @param rf Risk free rate in decimal, e.g. \code{rf=0.01} equals \code{1 percent}.
#' @param which Names or number of assets that should be included in calculation.
#' @param of Sharpe ratio to be calculated for assets separately or the portfolio sharpe.
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which performance shall be considered. If \code{NULL}, no restriction is made.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which performance shall be considered. If \code{NULL}, no restriction is made.
#' @param use.backtest If \code{TRUE}, the performance of the backtesting output is considered for sharpe ratio calculation. If \code{FALSE}, the performance of the initial strategy execution are used.
#' @param scaling.periods Vector with annualization factors for sharpe ratio calculation. E.g. if data is monthly, scaling.periods is to be set to 12 for the respective .
#' @examples
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, stratFUN.name="MA", stratFUN.parameters=params)
#'
#' # Get sharpe of MA(200)-Strategy portfolio
#' sharpe(myStrat.MA, from="2015-01-01", until="2015-12-31")
#'
#' ## Not run:
#' # Get backtest annualized sharpe of MA(200)-Strategy (daily data = 252 trading days)
#' sharpe(myStrat.MA, from="2015-01-01", until="2015-12-31", use.backtest=T, scaling.periods=252)
#' ## End(Not run)
setMethod(f = "sharpe",
          signature = "Strategy",
          definition = function(object, rf, of=c("portfolio", "assets"), from, until, which, scaling.periods, use.backtest) {
            of <- match.arg(of)
            if (!is.numeric(rf))
              stop("Please provide risk free return as numeric!")
            if (!is.numeric(scaling.periods))
              stop("Please provide scaling periods as numeric!")
            logReturns <- performance(object, of=of, from=from, until=until, which=which, type="logReturns", use.backtest=use.backtest)
            logDiff <- logReturns - log(1+rf)
            returns <- exp(logReturns) - 1
            # correction term neglected / + 1/2* vapply(d, sd, 0)^2
            sharpe <- ( exp( vapply(logDiff, mean, 0)*scaling.periods ) - 1 )/ ( vapply(returns, sd, 0) * sqrt(scaling.periods) )
            return(sharpe)
          }
)


# Returns the Maximum Drawdown of the strategy
# either on portfolio or single asset basis
# and either in absolute values or relative drawdowns
setGeneric(name = "MDD",
           def = function(object, of="portfolio", from=NULL, until=NULL, which=NULL, type="relative", use.backtest=F) {
             standardGeneric("MDD")
           }
)

#' @export
#' @name MDD
#' @aliases MDD
#' @title Strategy Performance Maximum Drawdown
#' @description Gets the maximum drawdown of the performance of an object of class \code{Strategy}.
#' @usage MDD(object, which=NULL, of="portfolio", from=NULL, until=NULL, type="relative"
#'      , use.backtest=F
#'      , ...)
#' @param object An object of class \code{Strategy}.
#' @param which Names or number of assets that should be included in calculation.
#' @param of Maximum Drawdown to be calculated for assets separately or the portfolio.
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which performance shall be considered. If \code{NULL}, no restriction is made.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which performance shall be considered. If \code{NULL}, no restriction is made.
#' @param use.backtest If set to \code{TRUE}, the signals from the backtesting output are considered for maximum drawdown calculation. If \code{FALSE}, the signals from the initial strategy execution are used.
#' @param type If the \code{absolute} or \code{relative} drawdown of the performance shall be returned.
#' @examples
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, stratFUN.name="MA", stratFUN.parameters=params)
#'
#' # Get MDD of MA(200)-Strategy portfolio
#' MDD(myStrat.MA, from="2015-01-01", until="2015-12-31")
#'
#' ## Not run:
#' # Get MDD of MA(200)-Strategy (daily data = 252 trading days)
#' MDD(myStrat.MA, from="2015-01-01", until="2015-12-31", use.backtest=T)
#' ## End(Not run)
setMethod(f = "MDD",
          signature = "Strategy",
          definition = function(object, of=c("portfolio", "assets"), from, until, which, type=c("absolute", "relative"), use.backtest) {
            of <- match.arg(of)
            type <- match.arg(type)
            vals <- performance(object, of=of, from=from, until=until, which=which, use.backtest=use.backtest)

            maxdrawdown <- function(x)
            {
              x.dates <- index(x)
              x <- as.matrix(x)
              # get Drawdowns
              dd <- cummax(x) - x
              # get Max Drawdown Position
              to.pos <- last(which(dd==max(dd))) # to which position
              # get absolute MDD
              mdd <- dd[to.pos]
              # get min value (dropdown end)
              to <- x[to.pos]
              # from which position drops
              from.pos <- last(which(x==cummax(x)[to.pos]))
              # get max value (mdd drop start)
              from <- x[from.pos]
              if (type=="relative")
                mdd <- mdd/from # in percent
              # set dates as names
              names(to) <- x.dates[to.pos]
              names(from) <- x.dates[from.pos]
              return(list(mdd = mdd, from = from, to = to))
            }
            mdd <- lapply(vals, maxdrawdown)
            return(mdd)
          }
)

setGeneric(name = "performanceIndicators",
           def = function(object, of="portfolio", from=NULL, until=NULL, which=NULL, use.backtest=F, scaling.periods=1) {
             standardGeneric("performanceIndicators")
           }
)
setMethod(f = "performanceIndicators",
          signature = "Strategy",
          definition = function(object, of=c("portfolio", "assets"), from, until, which, use.backtest, scaling.periods) {

            of <- match.arg(of)

            # GET PERFORMANCES
            logReturns <- performance(object, of=of, from=from, until=until, which=which, use.backtest=use.backtest, type="logReturns")
            meanReturns <- exp(apply(logReturns, 2, mean) * scaling.periods)  - 1
            returns <- exp(logReturns) - 1
            vola <- apply(returns, 2, sd) * sqrt(scaling.periods)
            sharpe <- sharpe(object, of=of, from=from, until=until, which=which, scaling.periods=scaling.periods, use.backtest=use.backtest)
            mdd <- sapply(MDD(object, of=of, type="relative", from=from, until=until, which=which, use.backtest=use.backtest), function(x) x[["mdd"]])

            # validate which values
            prices <- head(getPrices(object, from=from, until=until, which=which))
            which.out <- validWhich(which, prices)

            # TRADES
            signals <- getSignals(object, use.backtest)[paste0(start(returns),"::",end(returns)),which.out]
            trades <- diff(signals, lag=1)
            trades[1,] <- 0
            if (of == "portfolio") {
              tradesSum <- sum(abs(trades))
              names(tradesSum) <- "Portfolio Trades"
            } else {
              tradesSum <- apply(abs(trades), 2, sum)
            }

            return(list(meanReturns=meanReturns, vola=vola, sharpe=sharpe, mdd=mdd, trades=tradesSum))
          }
)


# ------------------------------------------------------------
# COMPARE STRATEGIE Performances
# ------------------------------------------------------------

setGeneric(name = "compare",
           signature = "...",
           def = function(..., from=NULL, until=NULL, which=NULL, scaling.periods=1, use.backtest=F, include.params=F) {
             standardGeneric("compare")
           }
)
# COMPARE different Strategy-objects' performances
# with annualized returns and vola, sharpe and mdd
# benchmark: provide first strategy object
# with: list with other strategy objects
# from: from period date
# until: until period date
# use.backtest: use backtest performance?

#' @export
setMethod(f = "compare",
          signature = signature("..."="Strategy"),
          definition = function(..., from, until, which, scaling.periods, use.backtest, include.params) {

            args <- list(...)

            # CHECK scaling.periods
            if (!is.numeric(scaling.periods) || is.null(scaling.periods))
              stop("Please provide scaling.periods as numeric!")
            if (length(scaling.periods) > 1) {
              if (length(scaling.periods) != length(args))
                stop("Please provide as many scaling.periods-values as Strategy-objects!")
            } else {
              scaling.periods <- rep(scaling.periods, length(args))
            }


            # VALIDATE Strategy Objects
            args.strategies <- rep(FALSE, length(args))
            for (i in 1:length(args.strategies)) {
              if ("Strategy" %in% class(args[[i]]))
                args.strategies[i] <- TRUE
            }

            if (length(args.strategies[args.strategies==FALSE]) > 0) {
              if (length(args.strategies[args.strategies==TRUE]) == 0) {
                stop("Please provide only objects of class Strategy to compare!")
              } else {
                warning(paste0("Please provide only objects of class Strategy. The following will be ignored: ", paste0(names(args[which(args.strategies==FALSE)]), collapse=",")))
              }
            }
            # select valid Strategy objects
            strategies.list <- args[which(args.strategies==T)]
            len <- length(strategies.list)
            scaling.periods <- scaling.periods[which(args.strategies==T)]

            # get strategy names
            names <- sapply(strategies.list, function(strat) getStratFUN.name(strat, include.params=include.params))

            # INITIALIE performance matrice
            performance_mat <- matrix(ncol=len, nrow=5)
            rownames(performance_mat) <- c("Scaled Returns", "Scaled Volatility", "Scaled Sharpe", "Maximum Drawdown", "Number of Trades")
            colnames(performance_mat) <- names

            for (i in 1:len) { #i<-1
              strategy <- strategies.list[[i]]

              # GET PERFORMANCES
              performance.indicators <- performanceIndicators(strategy, of="portfolio", from=from, until=until, which=which, use.backtest=use.backtest, scaling.period=scaling.periods[i])
              returns_a <- performance.indicators[["meanReturns"]]
              vola_a <- performance.indicators[["vola"]]
              sharpe_a <- performance.indicators[["sharpe"]]
              mdd <- performance.indicators[["mdd"]]
              trades <- performance.indicators[["trades"]]
              performance_mat[,i] <- c(returns_a, vola_a, sharpe_a, mdd, trades)
            } #for strategy

            return(performance_mat)
          }
)


# ------------------------------------------------------------
# BACKTESTING METHOD
# ------------------------------------------------------------

# BACKTESTING FUNCTION to backtest the strategy of the object
# within a rolling window of time
# OUTPUT: the Strategy-object will have filled slots with backtest-performance and the used parameters
# VARIABLES:
# object: the strategy object
# data.width: period for calibration (in-sample)
# horizon: period for performnance (out-of-sample) performance
# keep.history: move rolling window or start at same point?
# optim.param: character vector with params to be optimized
# optim.param.min: min values of parameters to optimize
# optim.param.max: max values of parameters to optimize
# optim.param.scale: accuracy of values of parameters to optimize
# from: backtest period start
# until: backtest period end
# rf: risk free return
setGeneric(name = "backtest",
           def = function(object, horizon="6m", data.width="24m", keep.history=F,
                          optim.param=NULL, #parameter names (Strings) as vector
                          optim.param.min=1, optim.param.max=10, #numeric vector of mins and max to be tested (not inf)
                          optim.param.scale=.1,
                          from=NULL, until=NULL, which=NULL, rf=0, printSteps=F) {
             standardGeneric("backtest")
           }
)


#' @export
#' @name backtest
#' @title Backtest Strategy
setMethod(f = "backtest",
          signature = "Strategy",
          definition = function(object, horizon, data.width, keep.history,
                                optim.param, #parameter names (Strings) as vector
                                optim.param.min, optim.param.max, #numeric vector of mins and max to be tested (not inf)
                                optim.param.scale,
                                from, until, which, rf, printSteps) {


            # VALIDATIONS
            if (!is.null(optim.param)) {
              if (!is.character(optim.param)) stop("Please provide parameter name to be optimized as character!")
              if (!(is.null(optim.param.min) || is.null(optim.param.max)
                    || is.numeric(optim.param.min) || is.numeric(optim.param.max))
                  || length(optim.param.min) != length(optim.param) || length(optim.param.max) != length(optim.param)
                  || any(optim.param.min>optim.param.max)
              ) stop("Please provide optimization parameter minimum and maximum as increasing or equal numerics with correct scaling for each parameter!")
              if (!all((optim.param.max-optim.param.min)/optim.param.scale >= 1))
                warning(paste0("The following parameters will be fixed: ", paste(optim.param[which((optim.param.max-optim.param.min)/optim.param.scale < 1)], collapse=", ")))

            } else { # if no params given, take objet's existing params
              warning("No parameters to optimize given in optim.params, values of strategy object taken. Performance will be the same as in strategy execution without backtesting.")
              parameters <- getParameters(object)
              parameters.numeric <- list()
              m <- 1
              for (i in 1:length(parameters)) {
                if (is.numeric(parameters[[i]])) {
                  parameters.numeric[m] <- parameters[i]
                  names(parameters.numeric)[m] <- names(parameters[i])
                  m <- m+1
                }
              }
              if (length(parameters.numeric) > 0) {
                optim.param <- names(parameters.numeric)
                optim.param.min <- optim.param.max <- (1:length(optim.param)) * NA
                optim.param.scale <- rep(1, length(optim.param))
                for (i in 1:length(optim.param)) {
                  optim.param.min[i] <- optim.param.max[i] <- parameters.numeric[[i]]
                }
              } else {
                optim.param <- "pseudo"
                optim.param.min <- optim.param.max <- optim.param.scale <- 1
              }
            } # if no params
            if (!is.numeric(rf) || length(rf) != 1)
              stop("Please provide risk free return as numeric (single) value!")

            # SET VARIABLES
            prices <- getPrices(object, from=from, until=until, which=which)
            strategy <- getStratFUN.name(object)
            indicators <- getIndicators(object)
            object.name <- deparse(substitute(object))
            object.env <- parent.frame()

            # save setup
            setup <- matrix(ncol=length(optim.param), nrow=3)
            colnames(setup) <- optim.param
            rownames(setup) <- c("Minimum", "Maximum", "Scaling")
            setup[1,] <- optim.param.min
            setup[2,] <- optim.param.max
            setup[3,] <- optim.param.scale

            # VALIDATE stratFUN
            stratFUN <- getStratFUN(object)

            # Start and End dates of each estimation period
            rolling.dates <- xts.rollingWindows(prices, data.width, horizon, keep.history)
            rolling.from <- rolling.dates$start
            rolling.to <- rolling.dates$end

            # Number of rebalancing periods
            nbPeriods <- length(rolling.to)
            # Predefine calibration parameter xts (for output)
            param.calib_mat <- matrix(nrow=nbPeriods, ncol=length(optim.param))
            rownames(param.calib_mat) <- rolling.to
            colnames(param.calib_mat) <- optim.param
            param.calib <- as.list(rep(NA, ncol(prices)))
            names(param.calib) <- colnames(prices)
            for (i in 1:length(param.calib))
              param.calib[[i]] <- as.xts(param.calib_mat)

            # Predefine signals as list (for output)
            signals <- list()

            # EXPAND Grid
            optim.list <- list()
            for (i in 1:length(optim.param)) { #i<-1
              optim.list[[i]] <- seq(optim.param.min[i], optim.param.max[i], optim.param.scale[i])
            }
            names(optim.list) <- optim.param
            params.grid <- expand.grid(optim.list)


            # SHARPE Ratio Function
            # SET sharp ratio under Strategy as optimization criteria
            # with x: parameter combination for stratFUN
            sharpeFUN <- function(x, rf, calib.data) {

              params <- as.list(x)

              if (printSteps==T)
                print(paste("Params: ", paste(names(params), params, sep="=")))

              stratvals <- stratFUN(prices=calib.data, indicators=indicators, parameters=params)
              signals <- stratvals$signals
              logReturns <- stratvals$logReturns[paste0(index(signals))]
              returns <- exp(logReturns * signals) - 1
              vola <- vapply(returns-rf, sd, 0)
              vola[vola==0] <- 1 #if vola=0, it is risk free portfolio and Rp=rf
              sharpe.ratios <- colMeans(returns-rf)/vola

              if (printSteps==T)
                print(paste("Sharpes: ", paste(names(sharpe.ratios), sharpe.ratios, sep="=")))

              return(sharpe.ratios)
            }

            if (printSteps == T) {
              print("Period Windows:")
              print(t(t(apply(cbind(rolling.from, rolling.to), 1, paste, collapse=" until "))))
              print("Parameter Combinations:")
              print(params.grid)
            }

            # FOR EACH time period: calibrate and get out-of-sample performance

            for(tn in 1:nbPeriods) { #tn<-1#tn<-nbPeriods

              # Always print progress of period
              print(paste0("Period ", tn, " of ", nbPeriods, " started."))

              # Data used for calibration
              calibration.data <- prices[paste0(rolling.from[tn], "::", rolling.to[tn])]
              calibration.data <- calibration.data[-nrow(calibration.data),] #cut last value, is redundant with first in performance.data

              # OPTIMIZATION
              # rowwise=1 apply sharpeFUN to param combination
              # and get max sharpe for each asset
              sharpe.calib <- as.matrix(t(apply(params.grid, 1, sharpeFUN, rf=rf, calib.data=calibration.data)))
              sharpe.calib.max <- apply(sharpe.calib, 2, max, na.rm=T)
              names(sharpe.calib.max) <- colnames(prices)

              # initialize param.max matrix
              param.max <- matrix(nrow=ncol(params.grid), ncol=ncol(prices))
              rownames(param.max) <- colnames(params.grid)
              colnames(param.max) <- colnames(prices)

              # GET Params that maximized sharpe for each asset
              # sharpe.calib[is.na(sharpe.calib)] <- min(sharpe.calib, na.rm=T) #replace NA with minimums
              sharpe.calib.which.max <- sharpe.calib*NA #initialize & clear values
              for (i in 1:ncol(prices)) { #i<-1
                sharpe.calib.which.max[,i] <- sharpe.calib.max[colnames(prices[,i])] == sharpe.calib[,i]
                param.max[,i] <- as.numeric(params.grid[min(which(sharpe.calib.which.max[,i]==1)),])
              }

              # Performance DATA for out-of-sample (following period (steps))
              if (tn < nbPeriods) { #tn<-1
                # GET prices for out-of-sample performance
                performance.data <- prices[paste0(rolling.to[tn], "::", rolling.to[tn+1])]
                performance.data <- performance.data[-nrow(performance.data),]
              } else {
                # GET prices for out-of-sample performance
                performance.data <- prices[paste0(rolling.to[tn], "::")]
              }

              # RUN out-of-sample performance
              strat.signals <- list()
              for ( i in 1:ncol(performance.data)) { #i<-1
                params.perf <- as.list(param.max[,i])
                names(params.perf) <- optim.param
                if (printSteps==T) {
                  print(paste("Params for out-of-sample: ", colnames(performance.data[,i]), " ", paste(names(params.perf), params.perf, sep="=", collapse=",")))
                }
                stratvals <- stratFUN(rbind(calibration.data[,i], performance.data[,i]), indicators=indicators, parameters=params.perf)
                signals_o <- stratvals$signals[paste0(index(performance.data))] # is shifted, take only performance period
                #logReturns <- stratvals$logReturns[paste0(index(performance.data))] # take only performance period
                #strat.logReturns <- logReturns * signals
                strat.signals[[i]] <- signals_o
              }

              # SAVE signals
              names(strat.signals) <- colnames(performance.data)
              signals[[tn]] <- as.xts(do.call(cbind, strat.signals))


              # SAVE calibration parameters used for period tn for each asset
              for (j in 1:length(param.calib)) #j<-1
                param.calib[[j]][tn,] <- param.max[,j]


            }#foreach

            # Set Names

            # SAVE BACKTEST Results to Strategy object
            signals.xts <- Reduce(rbind, signals)
            object@backtest.signals <- signals.xts
            object@backtest.parameters <- param.calib
            object@backtest.setup <- setup
            assign(object.name, object, env=object.env)


          } # method function
) # method
