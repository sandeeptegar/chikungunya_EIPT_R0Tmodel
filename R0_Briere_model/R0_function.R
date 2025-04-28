briere <- function(T, c, T0, Tm) {
  if (is.numeric(T) && length(T) > 1) {  # Handle vector-like input
    result <- c * T * (T - T0) * sqrt(Tm - T)
    result[T < T0] <- 0  # Set rate to 0 for temperatures below T0
    result[T > Tm] <- 0  # Set rate to 0 for temperatures above Tm
    return(result)
  } else if (T0 < T && T < Tm) {  # Handle single value input
    return(c * T * (T - T0) * sqrt(Tm - T))
  } else {
    return(0)  # Return 0 if temperature is out of range
  }
}

quadratic <- function(T, c, T0, Tm) {
  if (is.numeric(T) && length(T) > 1) {  # Handle vector-like input
    result <- -c * (T - T0) * (T - Tm)
    result[T < T0] <- 0  # Set rate to 0 for temperatures below T0
    result[T > Tm] <- 0  # Set rate to 0 for temperatures above Tm
    return(result)
  } else if (T0 < T && T < Tm) {  # Handle single value input
    return(-c * (T - T0) * (T - Tm))
  } else {
    return(0)  # Return 0 if temperature is out of range
  }
}