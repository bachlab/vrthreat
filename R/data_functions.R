#' Convert 1-3 dimensional array to STAN format
#'
#' STAN represents a multidimensional array as an array of arrays. For example,
#' a 2-dimensional array in STAN is internally a 1-dimensional array, in which
#' each element corresponds to a row in the 2-dimensional array.
#' Multidimensional arrays can be passed directly to a STAN model in R format
#' when the model is called with Rstan, but exposed
#' STAN functions will expect the internal STAN format.
#' This function handles
#' 1-3 dimensional arrays. 1-dimensional arrays do not need any change and this
#' is included for convenience only.
#'
#' @param in_array
#'
#' @return out_array a stan-compatible array of arrays
#' @export
#'
#' @examples
array2STAN <- function(in_array) {
  if (length(dim(in_array)) == 1) {
    return(in_array)
  }
  if (length(dim(in_array)) == 2) {
    return(lapply(
      1:nrow(in_array),
      FUN = function(i) {
        in_array[i, ]
      }
    ))
  }
  if (length(dim(in_array)) == 3) {
    return(lapply(
      1:nrow(in_array),
      FUN = function(i) {
        array2STAN(in_array[i, ,])
      }
    ))
  }
}
