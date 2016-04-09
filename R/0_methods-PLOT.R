#' @export
#' @name plot
#' @aliases plot
#' @title Plot of a \code{Strategy}-object
#' @description Calls the \code{plot} function that is defined within the strategy definition. E.g. MA-strategy's \code{plot} function is named \code{plot.MA()}.
#' @param x An object of class \code{Strategy}.
#' @param from From date that chart is to be plotted.
#' @param until Until date that chart is to be plotted.
#' @param which.assets Which assets shall be plotted (each one will result in single plot)
#' @param which.filters Which filters shall be added to price plot. Default value \code{NULL} will return all filters from the strategy. 
#' @param which.indicators Which indicators shall be added to indicator plot. Default value \code{NULL} will return all filters from the strategy. If \code{"none"}, no indicator is plotted and indicator area is not shown. 
#' @param main The main title of the plot.
#' @param include.costs If \code{FALSE}, the fixed and relative trading costs are NOT considered for performance calculation. Default value is \code{TRUE}. As default values for costs are \code{0}, this argument is redundant if no costs are given.
#' @examples
#' ##Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Plot first asset of MA(200)-Strategy
#' plot(myStrat.MA, from="2015-01-01", until="2015-12-31", which=1)
#'
#' ##End(Not run)
setMethod(f = "plot",
          signature = "Strategy",
          definition = function(x, from=NULL, until=NULL, which.assets=NULL, which.filters=NULL, which.indicators=NULL, main=NULL, show.signals=T, include.costs=T, ...) {
            
            object <- x
            which <- which.assets
            fontsize <- 1 # generally not change
            
            if (!is.null(object@plotFUN)) {
              plot.args.def <- names(formals(object@plotFUN))
              plot.args.list <- list(object=object, which=which, from=from, until=until, which.filters=which.filters
                                   , which.indicators=which.indicators, main=main, show.signals=show.signals, '...'=list(...))
              plot.args <- plot.args.list[which(names(plot.args.list) %in% plot.args.def)]
              do.call(object@plotFUN, plot.args)
              
            } else { # if no plotFUN defined by strategy

            
              if (!is.logical(show.signals))
                stop("Argument show.signals must be a boolean!")
              
              par(mgp=c(2.5, 1, 0))
    
              # GET VALUES
              prices <- getPrices(object, from=from, until=until, which=which)
              filters <- lapply(getFilters(object, which=which.filters), function(x) x[paste0(start(prices),"::",end(prices)), colnames(prices)])
              
              indicators <- lapply(getIndicators(object, which=which.indicators), function(x) x[paste0(start(prices),"::",end(prices))])
              performance <- performance(object, of="assets", from=start(prices), until=end(prices), which=which, include.costs=include.costs)
              
              # lengths
              flen <- length(filters)
              ilen <- length(indicators)
              
              # define plot range for price window
              if (flen!=0) {
                prices.min <- pmin(apply(prices, 2, min), apply(matrix(Reduce(rbind, lapply(filters, function(filter) apply(filter, 2, min))), ncol=ncol(prices)), 2, min) )
                prices.max <- pmax(apply(prices, 2, max), apply(matrix(Reduce(rbind, lapply(filters, function(filter) apply(filter, 2, max))), ncol=ncol(prices)), 2, max) )
              } else {
                prices.min <- apply(prices, 2, min)
                prices.max <- apply(prices, 2, max)
              }
              
              if (show.signals==T) {
                signals <- getSignals(object, which=which, from=start(prices), until=end(prices))
                trades <- sign(abs(getTrades(object, which=which, from=start(prices), until=end(prices))))
                trades[1,] <- 1 # to ensure plotting
                trades[nrow(trades),] <- 1 # to ensure plotting
              }
            
              # PLOT main
              if (is.null(main)) {
                plot.main <- colnames(prices)
              } else {
                if (!is.character(main)) stop("Please provide plot headings as character!")
                if (length(main) == 1) plot.main <- rep(main, ncol(prices))
              }
              if (length(plot.main) != ncol(prices))
                stop("Please provide as many headings as graphics!")
              
              par.mar <- par()$mar # keep standard margins
              margins <- c(7, 4.1, 4.1, 3)
              
              # layout conditions
              layout.len <- 6
              heights <- c(0.45, 0.2, 0.35)
              if (show.signals==F && ilen==0) {
                layout.len <- 4
                heights <- c(0.6, 0.4)
              }
  
              
              # PLOT1 Output
              for (i in 1:ncol(prices)) { #i<-1
                layout(matrix(1:layout.len, ncol=2, byrow=T), widths=c(0.8, 0.2), heights=heights)
                #layout.show(2)
                
                prices_i <- prices[,i]
                
                # PLOT1: Plot Price Values
                par(mar=c(0, margins[2:4]))              
                plot.xts(prices_i, ylim=c(prices.min[i],prices.max[i]), main=plot.main[i], minor.ticks=F, axes=F, type="n")
                lines(prices_i, col="black")
                axis(2, las=2)
                # Draw filter vals
                if (flen > 0) {
                  for (fNo in 1:flen) {
                    lines(filters[[fNo]][,i], col=rainbow(flen)[fNo])
                  }
                }
                graphics::box()
                
                # PLOT2: LEGEND prices
                par(mar=c(0,0,0,0))
                plot(1:2, 1:2, type="n", axes=F, ann=F) #only for layout
                legend.names <- c(colnames(prices_i), names(filters))
                legend("left", legend=legend.names, col=c("black",rainbow(flen)), lty=rep(1,flen+1), cex=fontsize, bty="n");
                
                
                if (layout.len == 6) {
                  
                  # PLOT3: indicators & signals
                  par(mar=c(0, margins[2], 0, margins[4]))
                  # pseudo for same time domain
                  plot.xts(prices_i, ylim=c(-1,1), type="n", main="", axes=F, auto.grid = T)
                  
                  # signals
                  if (show.signals==T) {
                    par(new=T)
                    trades_i <- trades[trades[,i]!=0,i]
                    signals_i <- signals[index(trades_i),i]
                    signals_i <- na.fill(na.locf(merge.xts(signals[,i], prices_i)[,1]), 0) # resolve time domain issue
                    signals_range <- c(-1,1)*max(abs(signals_i), na.rm=T) # 0 is middle
                    # plot init / pseudo range for rectangles
                    plot.xts(prices_i, ylim=signals_range, type="n", main="", axes=F, auto.grid=F)
                    # plot colors
                    cols <- signals_i*NA
                    cols[signals_i>0] <- "green"
                    cols[signals_i<0] <- "red"
                    rect(xleft = .index(signals_i), ybottom = rep(0,nrow(signals_i)), xright = c(.index(signals_i[2:nrow(signals_i),]), .index(prices_i[nrow(prices),])), ytop = signals_i, col = cols, border = NA)
                    #barplot(signals_i, ylim=signals_range, axes=F, axisnames=F, col="lightblue", space=0, border=NA, main="")
                  }
                  abline(h=0, col="gray")
                  
                  # indicators
                  if (ilen > 0) {
                    for (indNo in 1:ilen) { #indNo<-1
                      par(new=T)
                      ind <- merge.xts(indicators[[indNo]], prices_i)[,1] # resolve time domain issue
                      ind_range <- c(-1,1)*max(abs(ind), na.rm=T)
                      plot.xts(na.locf(ind), ylim=ind_range, col=rainbow(ilen)[indNo], type="l", main="", axes=F, auto.grid=F)
                    }  
                  } 
                  graphics::box() # draw box line
                  
                  # PLOT4: LEGEND indicators
                  par(mar=c(0,0,0,0))
                  plot(1:2, 1:2, type="n", axes=F, ann=F) #only for layout
                  if (show.signals==T) {
                    legend("left", legend=c("Position", names(indicators)), col=c("lightblue", rainbow(ilen)), lty=rep(1,1+ilen), cex=fontsize, bty="n");
                  } else {
                    legend("left", legend=names(indicators), col=rainbow(ilen), lty=rep(1,ilen), cex=fontsize, bty="n");
                  }
                }
                
                # PLOT5: PERFORMANCE
                par(mar=c(margins[1:2], 0, margins[4]))
                # pseudo for same time domain
                plot(prices_i, ylim=range(performance[,i]), type="n", main="", axes=F)
                axis(1, at=.index(prices_i)[axTicksByTime(prices)], labels=names(axTicksByTime(prices)), las=2)
                axis(4, at=pretty(range(performance[,i])), las=2) # right axis
                # PERFORMANCE
                lines(performance[,i], col="darkgray")
                graphics::box()
                
                # PLOT6: LEGEND performance
                par(mar=c(margins[1],0,0,0))
                plot(1:2, 1:2, type="n", axes=F, ann=F) #only for layout
                # LEGEND
                legend("left", legend="Performance", col=c("darkgray"), lty=1, cex=fontsize, bty="n")
                
              } # for prices
              
              layout(1) #reset layout
              par(mar=par.mar) #reset margins
            }
          } # end if no plotFUN defined
)

# Plot of only the performance
# of the strategy or backtest
setGeneric(name = "plotPerformance",
           def = function(object, which=NULL, of="portfolio", from=NULL, until=NULL, use.backtest=F, include.costs=T, plot.params=T, plot.params.names=NULL, plot.params.first=T, ...) {
             standardGeneric("plotPerformance")
           }
)

#' @export
#' @name plotPerformance
#' @aliases plotPerformance
#' @title Plot Strategy Performance
#' @description Plots performance of an object of class \code{Strategy}.
#' @usage plotPerformance(object, which=NULL, of="portfolio", from=NULL, until=NULL
#'      , use.backtest=F, include.costs=T
#'      , plot.params=T , plot.params.names=NULL, plot.params.first=T
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
#' @param include.costs If \code{FALSE}, the fixed and relative trading costs are NOT considered for performance calculation. Default value is \code{TRUE}. As default values for costs are \code{0}, this argument is redundant if no costs are given.
#' @param ... Further arguments that can be passed to the underlying plot()-function.
#' @examples
#' ##Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Plot MA(200)-Strategy
#' plotPerformance(myStrat.MA, from="2015-01-01", until="2015-12-31")
#'
#' # Plot backtested MA(200)-Strategy
#' plotPerformance(myStrat.MA, from="2015-01-01", until="2015-12-31", use.backtest=T)
#'
#' ##End(Not run)
setMethod(f = "plotPerformance",
          signature = "Strategy",
          definition = function(object, which, of=c("portfolio", "assets"), from, until, use.backtest, include.costs, plot.params, plot.params.names, plot.params.first, ...) {
            # match argument "of" --> standard set in setGeneric function
            of <- match.arg(of)
            if (!is.logical(plot.params) || !is.logical(plot.params.first))
              stop("Please provide plot.params and plot.params.first as logical!")
            
            performance <- performance(object, of=of, which=which, from=from, until=until, use.backtest=use.backtest, include.costs=include.costs)
            
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

# Plot rolling drawdowns
setGeneric(name = "plotDrawdowns",
           def = function(object, from=NULL, until=NULL, which=NULL, of="portfolio", type="relative", include.costs=T, use.backtest=F, returnValues=F, ...) {
             standardGeneric("plotDrawdowns")
           }
)

#' @export
#' @name plotDrawdowns
#' @aliases plotDrawdowns
#' @title Plot Strategy Drawdowns
#' @description Plots drawdowns of the performance of an object of class \code{Strategy}.
#' @usage plotDrawdowns(object, which=NULL, from=NULL, until=NULL
#'      , of="portfolio", use.backtest=F
#'      , include.costs=T, returnValues=F, ...)
#' @param object An object of class \code{Strategy}.
#' @param which Names or number of assets that should be included in performance. If a portfolio performance from only a subset of the assets is calculated, the weights are scaled accordingly.
#' @param of Performance to be extracted from assets separately or the portfolio performance.
#' @param type If the \code{absolute} or \code{relative} drawdown of the performance shall be returned.
#' @param from The date in character format \code{"yyyy-MM-dd"} or as date-object from which drawdowns shall be plotted. If \code{NULL}, the start date of the performances is used.
#' @param until The date in character format \code{"yyyy-MM-dd"} or as date-object until which drawdowns shall be plotted. If \code{NULL}, the end date of the performances is used.
#' @param use.backtest If \code{TRUE}, the signals from the backtesting output are considered for drawdowns calculation. If \code{FALSE}, the signals from the normal strategy execution with the intial parameters are used.
#' @param include.costs If \code{FALSE}, the fixed and relative trading costs are NOT considered for performance calculation. Default value is \code{TRUE}. As default values for costs are \code{0}, this argument is redundant if no costs are given.
#' @param returnValues If \code{TRUE}, the drawdown values are returned.
#' @param ... Further arguments that can be passed to the underlying plot()-function.
#' @examples
#' ##Not run:
#'
#' # MA(200)-Strategy
#' params <- list(k=200)
#' myStrat.MA <- Strategy(assets=assets, strat="MA", strat.params=params)
#'
#' # Plot MA(200)-Strategy drawdowns
#' plotDrawdowns(myStrat.MA, from="2015-01-01", until="2015-12-31")
#'
#' # Plot backtested MA(200)-Strategy drawdowns
#' plotDrawdowns(myStrat.MA, from="2015-01-01", until="2015-12-31", use.backtest=T)
#'
#' ##End(Not run)
setMethod(f = "plotDrawdowns",
          signature = "Strategy",
          definition = function(object, from, until, which, of=c("portfolio","assets"), type=c("relative","absolute"), include.costs, use.backtest, returnValues, ...) {
            of <- match.arg(of)
            type <- match.arg(type)
            
            if (!is.logical(returnValues))
              stop("Argument returnValues must be a boolean!")
            
            performance <- performance(object, of=of, which=which, from=from, until=until, include.costs=include.costs, use.backtest=use.backtest)
            
            drawdowns <- function(x, type) {
              x.dates <- index(x)
              x <- as.matrix(x)
              # get Drawdowns
              dd <- cummax(x) - x
              # get Max Drawdown Position
              to.pos <- last(which(dd==max(dd))) # to which position
              # get absolute MDD
              mdd <- dd[to.pos]
              # from which position drops
              from.pos <- last(which(x==cummax(x)[to.pos]))
              # get max value (mdd drop start)
              from <- x[from.pos]
              if (type=="relative") {
                dd <- dd/cummax(x) # in percent
                mdd <- mdd/from # in percent
              }
              return(list(dd=-dd, mdd=-mdd, from=from.pos, to=to.pos))
            }
            
            dd.list <- lapply(performance, FUN=function(x, type) drawdowns(x,type), type=type)
            dd <- as.xts(Reduce(cbind, lapply(dd.list, FUN=function(x) x[["dd"]])))
            from <- Reduce(cbind, lapply(dd.list, FUN=function(x) x[["from"]]))
            to <- Reduce(cbind, lapply(dd.list, FUN=function(x) x[["to"]]))
            mdd <- Reduce(cbind, lapply(dd.list, FUN=function(x) x[["mdd"]]))
            
            # PLOT Parameters / check for heading
            args <- list(...)
            plot.main <- paste0(toupper(substring(type,1,1)), tolower(substring(type,2)), " Drawdowns of ", colnames(performance))
            if ("main" %in% names(args))
              plot.main <- args[["main"]]
            if (length(plot.main) != ncol(performance))
              stop("Please provide as many headings as graphics!")
            args.inUse <- which(names(args) %in% "main")
            if (length(args.inUse) > 0)
              args <- args[-args.inUse] # select only editable arguments for plot
            
            par.mar <- par()$mar # keep standard margins
            margins <- c(7, 4.1, 4.1, 3)
            
            for (i in 1:ncol(dd)) { #i<-1
              par(mar=margins)
              # plot(dd[,i], ylim=c(min(dd[,i])*1.04, 0), type="n", main=plot.main[i], minor.ticks=F, las=2, axes=F, yaxs="i", xaxs="i", ...)
              plot(dd[,i], type="n", main=plot.main[i], minor.ticks=F, las=2, axes=F, ...)
              axis(2, las=2)
              axis(1, at=.index(dd[,i])[axTicksByTime(dd)], labels=names(axTicksByTime(dd)), las=2)
              arr.x <- c(index(dd[from[i],i]), index(dd[to[i],i]))
              arr.y <- c(0, mdd[i])
              arrows(x0=arr.x[1], y0=arr.y[1], x1=arr.x[2], y1=arr.y[2], col="red", lwd=2, length=0.15, angle=25, code=2)
              text(paste0("MDD ",round(mdd[i]*100,0),"%"), x=arr.x[2], y=arr.y[2], col="red", pos=2, cex=0.8)
              lines(dd[,i])
              #pol.y <- rbind(-dd[,i],xts(0,last(index(dd))))
              #polygon(x=c(.index(dd),last(.index(dd))), y=pol.y, col = "bisque3", border = NA)
              #lines(-dd[,i])
              #box()
            }
            
            if (returnValues==T)
              return(dd)
        } #method function
)