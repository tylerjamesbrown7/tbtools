#' tables for models
#'
#' @export
regtable <- function(..., robust = FALSE, se_type = 'HC0'){
  if (robust){
    stargazer::stargazer(list(lapply(list(...), FUN = se_robust, se_type = se_type)), type = 'text')
  } else {
    stargazer::stargazer(...,type = 'text')
  }
}

source("R/se_robust.R")



