#' Winsorizes outliers based on a percentile
#'
#' @export
winsorize <- function(list, percentile = 0.90, bounds = c(min(list), max(list))){
  if (missing(bounds) & (percentile < 1 & percentile > 0)){
    windsor_bounds <- stats::quantile(list, probs = c((1-percentile)/2, percentile + (1-percentile)/2), na.rm=T)
    list[list < windsor_bounds[1]] <- windsor_bounds[1]
    list[list > windsor_bounds[2]] <- windsor_bounds[2]
    return(list)
  } else if ((bounds[1] > min(list) & bounds[2] < max(list) )) {
    list[list < bounds[1]] <- bounds[1]
    list[list > bounds[2]] <- bounds[2]
    return(list)
  } else {
    stop("Percentile must be in (0,1)")
  }
}
