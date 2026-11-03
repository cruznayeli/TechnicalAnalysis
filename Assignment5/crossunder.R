crossunder <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  if (length(arr1) == 0) {
    return(character(0))
  }

  # Initialize a vector to store the crossunder signals
  crossunder_signals <- character(length(arr1))
  crossunder_signals[1] <- "None"

  # Check for crossunder signals at each data point
  for (i in seq_along(arr1)[-1]) {
    if (isTRUE(arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1])) {
      crossunder_signals[i] <- "True"
    } else {
      crossunder_signals[i] <- "False"
    }
  }

  return(crossunder_signals)
}
