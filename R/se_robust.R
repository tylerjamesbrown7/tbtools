#' IV standard errors
#'
#' @export
se_robust <- function(model, se_type = 'HC0'){
  lmtest::coeftest(model, vcov = sandwich::vcovHC(model, type = se_type))
}
