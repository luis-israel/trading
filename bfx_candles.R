library(jsonlite)
library(httr)
library(xts)

# given a timeframe, symbol, section and limit (see https://api.bitfinex.com/v2/),
# return the given currency's last limit candles

bfx_candles <- function(timeframe, symbol, section, limit) {
  api <- "https://api.bitfinex.com/v2/"
  endpoint <- "candles/trade"
  url <- paste0(
    api, endpoint,
    ":", timeframe,
    ":", symbol,
    "/", section, "/"
  )
  request <- GET(url, query = list(limit = limit))
  candles <- content(request)
  candles <- data.frame(matrix(unlist(candles),
                               nrow = length(candles),
                               byrow = T
  ))
  colnames(candles) <- c("mts", "open", "close", "high", "low", "volume")
  candles <- candles[dim(candles)[1]:1, ]
  dates <- seq(Sys.Date() - limit + 1, Sys.Date(), by = "days")
  candles <- xts(candles, order.by = dates)
}