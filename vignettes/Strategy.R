## ------------------------------------------------------------------------
require(Strategy)

# Generate positive random walks for random assets
set.seed(1)
len <- 1000
n <- 10
assets <- abs(apply(matrix(rnorm(n*len), ncol=n), 2, cumsum)) + 100
colnames(assets) <- paste0("asset", 1:n)
assets <- xts(assets, order.by = seq(from=Sys.Date()-len, length.out=len, by="d"))

# MA(200)-strategy
myStrat.MA200 <- Strategy(assets = assets
                          , strat = "MA"
                          , strat.params = list(k=200))


