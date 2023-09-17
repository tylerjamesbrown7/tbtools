#' Winsorizes outliers based on a percentile
#'
#' @export
winsorize <- function(list, percentile = 0.90, bounds = c(min(list[!is.na(list) & is.finite(list)]), max(list[!is.na(list) & is.finite(list)]))){
  if (missing(bounds) & (percentile < 1 & percentile > 0)){
    windsor_bounds <- stats::quantile(list, probs = c((1-percentile)/2, percentile + (1-percentile)/2), na.rm=T)
    list[list < windsor_bounds[1]] <- windsor_bounds[1]
    list[list > windsor_bounds[2]] <- windsor_bounds[2]
    print('hello one')
    return(list)
  } else if (bounds[1] > min(list[!is.na(list)]) & bounds[2] < max(list[!is.na(list)])) {
    print('hello two')
    list[list < bounds[1]] <- bounds[1]
    list[list > bounds[2]] <- bounds[2]

    return(list)
  } else if (bounds[1] < min(list[!is.na(list) & is.finite(list)]) | bounds[2] > max(list[!is.na(list) & is.finite(list)])) {
    stop("Bounds outside of data range")
  } else if (percentile > 1 | percentile < 0) {
    stop("Percentile must be btx. 0 and 1")
  }
}
