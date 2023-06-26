#' IV standard errors
#'
#' @export
iv_se <- function(model){
  lmtest::coeftest(model, vcov = sandwich::vcovHC(model, type = 'HC0'))
}
