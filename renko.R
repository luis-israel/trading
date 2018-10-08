library(data.table)
library(xts)

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

renko_signal <- function(brick, skip, leverage) {
  if (missing(leverage)) {
    long <- 1
    short <- 0
  }
  else {
    long <- leverage[1]
    short <- leverage[2]
  }
  if (missing(skip)) skip <- 1
  skip <- round(skip)
  brick_nl <- brick[!is.na(brick)]
  total <- brick_nl
  if (skip > 1) for (i in 1:skip) total <- total + shift(brick_nl, i)
  sig <- ifelse(total >= skip, long, 0)
  sig <- ifelse(total <= skip * -1, -1, sig)
  out <- merge(sig, brick)
  sig <- out[, 1]
  sig[sig == 0] <- NA
  sig <- na.locf(sig)
  sig <- ifelse(sig == -1, short, sig)
}