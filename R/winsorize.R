#' Winsorizes outliers based on a percentile
#'
#' @export
winsorize <- function(df, var, group, percentile = 0.90, bounds = c(min({{ var }}), max({{ var }}))){
  require(dplyr)
  if (missing(bounds) & (percentile < 1 & percentile > 0)){
    df %>%
    dplyr::mutate(quant_low := stats::quantile({{ var }}, probs = (1-percentile)/2, na.rm=T),
              quant_high = stats::quantile({{ var }}, probs = (1-percentile)/2 + percentile, na.rm=T)) %>%
      dplyr::mutate({{ var }} := dplyr::case_when({{ var }} < quant_low ~ quant_low,
                                                      {{ var }} > quant_high ~ quant_high,
                                                      TRUE ~ {{ var }})) %>%
      dplyr::mutate(quant_low = NULL,
                    quant_high = NULL)

  } else if (missing(percentile)) {
    df %>%
      dplyr::mutate({{ var }} := dplyr::case_when({{ var }} < bounds[1] ~ bounds[1],
                                                  {{ var }} > bounds[2] ~ bounds[2],
                                                  TRUE ~ {{ var }}))

  }
}
