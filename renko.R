library(data.table)
library(xts)

# given a timeseries and a size or a vector of sizes, returns a linear renko
# bricks timeseries

renko_brick <- function(x, size) {
  ratio <- x / size
  ratio <- floor(ratio)
  brick <- diff(ratio)
  brick <- brick[brick != 0]
  brick <- ifelse(sign(brick) == sign(shift(brick)),
                  brick, (abs(brick) - 1) * sign(brick))
  brick <- brick[brick != 0]
  brick <- ifelse(sign(brick) == sign(shift(brick)),
                  brick, (abs(brick) - 1) * sign(brick))
  brick <- brick[brick != 0]
  out <- merge(brick, x)
  brick <- out[, 1]
}