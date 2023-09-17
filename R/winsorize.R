#' Winsorizes outliers based on a percentile
#'
#' @export
winsorize <- function(list, percentile = 0.90){

  if (percentile < 1 & percentile > 0){
    bounds <- quantile(list, probs = c((1-percentile)/2, percentile + (1-percentile)/2), na.rm=T)
    list[list < bounds[1]] <- bounds[1]
    list[list > bounds[2]] <- bounds[2]
    return(list)
  } else {
    stop("Percentile must be in (0,1)")
  }
}
